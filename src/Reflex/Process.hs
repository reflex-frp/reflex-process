{-|
Module: Reflex.Process
Description: Run interactive shell commands in reflex
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Reflex.Process
  ( createProcess
  , createRedirectedProcess
  , Process(..)
  , ProcessConfig(..)
  ) where

import Control.Concurrent (forkIO, killThread, ThreadId)
import Control.Exception (mask_)
import Control.Monad (void, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import Data.Default
import qualified GHC.IO.Handle as H
import GHC.IO.Handle (Handle)
import System.Exit (ExitCode)
import qualified System.Posix.Signals as P
import qualified System.Process as P
import System.Process hiding (createProcess)
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import Data.Function

import Reflex

-- | The inputs to a process
data ProcessConfig t i = ProcessConfig
  { _processConfig_stdin :: Event t i
  -- ^ "stdin" input to be fed to the process
  , _processConfig_signal :: Event t P.Signal
  -- ^ Signals to send to the process
  , _processConfig_createProcess
    :: P.CreateProcess -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
  -- ^ Used for dependency injection (for example to ensure, in the test suite,
  -- that the child process is properly terminated). Defaults to
  -- 'System.Process.createProcess'
  }

instance Reflex t => Default (ProcessConfig t i) where
  def = ProcessConfig never never P.createProcess

-- | The output of a process
data Process t o e = Process
  { _process_handle :: P.ProcessHandle
  , _process_stdout :: Event t o
  -- ^ Fires whenever there's some new stdout output. Depending on the buffering strategy of the implementation, this could be anything from whole lines to individual characters.
  , _process_stderr :: Event t e
  -- ^ Fires whenever there's some new stderr output. See note on '_process_stdout'.
  , _process_exit :: Event t ExitCode
  , _process_signal :: Event t P.Signal
  -- ^ Fires when a signal has actually been sent to the process (via '_processConfig_signal').
  }

-- | Runs a process and uses the given input and output handler functions to
-- interact with the process via the standard streams. Used to implement
-- 'createProcess'.
--
-- NB: The 'std_in', 'std_out', and 'std_err' parameters of the
-- provided 'CreateProcess' are replaced with new pipes and all output is redirected
-- to those pipes.
createRedirectedProcess
  :: forall t m i o e. (MonadIO m, TriggerEvent t m, PerformEvent t m, MonadIO (Performable m))
  => (Handle -> IO (i -> IO ()))
  -- ^ Builder for the standard input handler
  -> (Handle -> (o -> IO ()) -> IO (IO ()))
  -- ^ Builder for the standard output handler
  -> (Handle -> (e -> IO ()) -> IO (IO ()))
  -- ^ Builder for the standard error handler
  -> CreateProcess
  -> ProcessConfig t i
  -> m (Process t o e)
createRedirectedProcess mkWriteStdInput mkReadStdOutput mkReadStdError p (ProcessConfig input signal createProcessFunction) = do
  let redirectedProc = p
        { std_in = CreatePipe
        , std_out = CreatePipe
        , std_err = CreatePipe
        }
  po@(mi, mout, merr, ph) <- liftIO $ createProcessFunction redirectedProc
  case (mi, mout, merr) of
    (Just hIn, Just hOut, Just hErr) -> do
      writeInput :: i -> IO () <- liftIO $ mkWriteStdInput hIn
      performEvent_ $ liftIO . writeInput <$> input
      sigOut :: Event t (Maybe P.Signal) <- performEvent $ ffor signal $ \sig -> liftIO $ do
        mpid <- P.getPid ph
        case mpid of
          Nothing -> return Nothing
          Just pid -> do
            P.signalProcess sig pid >> return (Just sig)
      let
          output :: Handle -> m (Event t o, ThreadId)
          output h = do
            (e, trigger) <- newTriggerEvent
            reader <- liftIO $ mkReadStdOutput h trigger
            t <- liftIO $ forkIO reader
            return (e, t)

      let
          err_output :: Handle -> m (Event t e, ThreadId)
          err_output h = do
            (e, trigger) <- newTriggerEvent
            reader <- liftIO $ mkReadStdError h trigger
            t <- liftIO $ forkIO reader
            return (e, t)
      (out, outThread) <- output hOut
      (err, errThread) <- err_output hErr
      (ecOut, ecTrigger) <- newTriggerEvent
      void $ liftIO $ forkIO $ do
        waited <- waitForProcess ph
        mask_ $ do
          ecTrigger waited
          P.cleanupProcess po
          killThread outThread
          killThread errThread
      return $ Process
        { _process_exit = ecOut
        , _process_stdout = out
        , _process_stderr = err
        , _process_signal = fmapMaybe id sigOut
        , _process_handle = ph
        }
    _ -> error "Reflex.Process.createRedirectedProcess: Created pipes were not returned by System.Process.createProcess."

-- | Run a shell process, feeding it input using an 'Event' and exposing its output
-- 'Event's representing the process exit code, stdout and stderr.
--
-- The input 'Handle' is not buffered and the output 'Handle's are line-buffered.
--
-- NB: The 'std_in', 'std_out', and 'std_err' parameters of the
-- provided 'CreateProcess' are replaced with new pipes and all output is redirected
-- to those pipes.
createProcess
  :: (MonadIO m, TriggerEvent t m, PerformEvent t m, MonadIO (Performable m))
  => CreateProcess
  -> ProcessConfig t ByteString
  -> m (Process t ByteString ByteString)
createProcess p processConfig = do
  channel <- liftIO newTChanIO

  let
    input h = do
      H.hSetBuffering h H.NoBuffering
      void $ liftIO $ forkIO $ fix $ \loop -> do
        newMessage <- atomically $ readTChan channel
        open <- H.hIsOpen h
        when open $ do
          writable <- H.hIsWritable h
          when writable $ do
            Char8.hPutStrLn h newMessage
            loop
      return (liftIO . atomically . writeTChan channel)
    output h trigger = do
      H.hSetBuffering h H.LineBuffering
      let go = do
            open <- H.hIsOpen h
            when open $ do
              readable <- H.hIsReadable h
              when readable $ do
                out <- BS.hGetSome h 32768
                if BS.null out
                  then return ()
                  else do
                    void $ trigger out
                    go
      return go

  createRedirectedProcess input output output p processConfig

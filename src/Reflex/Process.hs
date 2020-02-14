{-|
Module: Reflex.Process
Description: Run interactive shell commands in reflex
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Reflex.Process
  ( createProcess
  , createRedirectedProcess
  , Process(..)
  , ProcessConfig(..)
  ) where

import Control.Concurrent (forkIO, killThread)
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

import Reflex

-- | The inputs to a process
data ProcessConfig t i = ProcessConfig
  { _processConfig_stdin :: Event t i
  -- ^ "stdin" input to be fed to the process
  , _processConfig_signal :: Event t P.Signal
  -- ^ Signals to send to the process
  }

instance Reflex t => Default (ProcessConfig t i) where
  def = ProcessConfig never never

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
  :: (MonadIO m, TriggerEvent t m, PerformEvent t m, MonadIO (Performable m))
  => (Handle -> IO (i -> IO ()))
  -- ^ Builder for the standard input handler
  -> (Handle -> (o -> IO ()) -> IO (IO ()))
  -- ^ Builder for the standard output handler
  -> (Handle -> (e -> IO ()) -> IO (IO ()))
  -- ^ Builder for the standard error handler
  -> CreateProcess
  -> ProcessConfig t i
  -> m (Process t o e)
createRedirectedProcess mkWriteStdInput mkReadStdOutput mkReadStdError p (ProcessConfig input signal) = do
  let redirectedProc = p
        { std_in = CreatePipe
        , std_out = CreatePipe
        , std_err = CreatePipe
        }
  po@(mi, mout, merr, ph) <- liftIO $ P.createProcess redirectedProc
  case (mi, mout, merr) of
    (Just hIn, Just hOut, Just hErr) -> do
      writeInput <- liftIO $ mkWriteStdInput hIn
      performEvent_ $ liftIO . writeInput <$> input
      sigOut <- performEvent $ ffor signal $ \sig -> liftIO $ do
        mpid <- P.getPid ph
        case mpid of
          Nothing -> return Nothing
          Just pid -> do
            P.signalProcess sig pid >> return (Just sig)
      let output h = do
            (e, trigger) <- newTriggerEvent
            reader <- liftIO $ mkReadStdOutput h trigger
            t <- liftIO $ forkIO reader
            return (e, t)

      let err_output h = do
            (e, trigger) <- newTriggerEvent
            reader <- liftIO $ mkReadStdError h trigger
            t <- liftIO $ forkIO reader
            return (e, t)
      (out, outThread) <- output hOut
      (err, errThread) <- err_output hErr
      (ecOut, ecTrigger) <- newTriggerEvent
      void $ liftIO $ forkIO $ waitForProcess ph >>= \ec -> mask_ $ do
        ecTrigger ec
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
createProcess = createRedirectedProcess input output output
  where
    input h = do
      H.hSetBuffering h H.NoBuffering
      let go b = do
            open <- H.hIsOpen h
            when open $ do
              writable <- H.hIsWritable h
              when writable $ Char8.hPutStrLn h b
      return go
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

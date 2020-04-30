{-|
Module: Reflex.Process
Description: Run interactive shell commands in reflex
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Reflex.Process
  ( createProcess
  , createProcessBufferingInput
  , createProcessWith
  , defProcessConfig
  , redirectingCreateProcess
  , Process(..)
  , ProcessConfig(..)
  ) where

import Control.Concurrent (forkIO, killThread, ThreadId)
import Control.Concurrent.Chan (newChan, readChan, writeChan)
import Control.Exception (mask_)
import Control.Monad (void, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified GHC.IO.Handle as H
import GHC.IO.Handle (Handle)
import System.Exit (ExitCode)
import Data.Function
import qualified System.Posix.Signals as P
import qualified System.Process as P
import System.Process (std_err, std_in, std_out)

import Reflex

-- | The inputs to a process
data ProcessConfig t i = ProcessConfig
  { _processConfig_stdin :: Event t i
  -- ^ "stdin" input to be fed to the process
  , _processConfig_signal :: Event t P.Signal
  -- ^ Signals to send to the process
  , _processConfig_createProcess :: IO (Handle, Handle, Handle, P.ProcessHandle)
  -- ^ Configurable version of 'System.Process.createProcess' that must create pipes
  -- for 'std_in', 'std_out', and 'std_err'.
  }

-- | Make a default 'ProcessConfig' with the given 'System.Process.CreateProcess'
-- using 'redirectingCreateProcess'.
defProcessConfig :: Reflex t => P.CreateProcess -> ProcessConfig t i
defProcessConfig p = ProcessConfig never never (redirectingCreateProcess p)


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
createProcessWith
  :: forall t m i o e. (MonadIO m, TriggerEvent t m, PerformEvent t m, MonadIO (Performable m))
  => (Handle -> IO (i -> IO ()))
  -- ^ Builder for the standard input handler
  -> (Handle -> (o -> IO ()) -> IO (IO ()))
  -- ^ Builder for the standard output handler
  -> (Handle -> (e -> IO ()) -> IO (IO ()))
  -- ^ Builder for the standard error handler
  -> ProcessConfig t i
  -> m (Process t o e)
createProcessWith mkWriteStdInput mkReadStdOutput mkReadStdError (ProcessConfig input signal createProc) = do
  (hIn, hOut, hErr, ph) <- liftIO createProc
  writeInput :: i -> IO () <- liftIO $ mkWriteStdInput hIn
  performEvent_ $ liftIO . writeInput <$> input
  sigOut :: Event t (Maybe P.Signal) <- performEvent $ ffor signal $ \sig -> liftIO $ do
    mpid <- P.getPid ph
    case mpid of
      Nothing -> return Nothing
      Just pid -> Just sig <$ P.signalProcess sig pid
  let
    output :: Handle -> m (Event t o, ThreadId)
    output h = do
      (e, trigger) <- newTriggerEvent
      reader <- liftIO $ mkReadStdOutput h trigger
      t <- liftIO $ forkIO reader
      return (e, t)

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
    waited <- P.waitForProcess ph
    mask_ $ do
      ecTrigger waited
      P.cleanupProcess (Just hIn, Just hOut, Just hErr, ph)
      killThread outThread
      killThread errThread
  return $ Process
    { _process_exit = ecOut
    , _process_stdout = out
    , _process_stderr = err
    , _process_signal = fmapMaybe id sigOut
    , _process_handle = ph
    }

-- | Create a process feeding it input using an 'Event' and exposing its output with 'Event's
-- for its exit code, stdout, and stderr. The input is fed via a buffer represented by a
-- reading action and a writing action.
--
-- The @stdout@ and @stderr@ 'Handle's are line-buffered.
--
-- NB: The 'std_in', 'std_out', and 'std_err' parameters of the
-- provided 'CreateProcess' are replaced with new pipes and all output is redirected
-- to those pipes.
createProcessBufferingInput
  :: (MonadIO m, TriggerEvent t m, PerformEvent t m, MonadIO (Performable m))
  => IO ByteString -- ^ Read a value from the input stream buffer
  -> (ByteString -> IO ()) -- ^ Write a value to the input stream buffer
  -> ProcessConfig t ByteString -- ^ The process configuration in terms of Reflex
  -> m (Process t ByteString ByteString)
createProcessBufferingInput readBuffer writeBuffer procConfig = do
  let
    input h = do
      H.hSetBuffering h H.NoBuffering
      void $ liftIO $ forkIO $ fix $ \loop -> do
        newMessage <- readBuffer
        open <- H.hIsOpen h
        when open $ do
          writable <- H.hIsWritable h
          when writable $ do
            BS.hPutStr h newMessage
            loop
      return writeBuffer

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
  createProcessWith input output output procConfig

-- | Create a process feeding it input using an 'Event' and exposing its output
-- 'Event's representing the process exit code, stdout, and stderr.
--
-- The @stdout@ and @stderr@ 'Handle's are line-buffered.
--
-- N.B. The process input is buffered with an unbounded channel! For more control of this,
-- use 'createProcessBufferingInput' directly.
--
-- NB: The 'std_in', 'std_out', and 'std_err' parameters of the
-- provided 'CreateProcess' are replaced with new pipes and all output is redirected
-- to those pipes.
createProcess
  :: (MonadIO m, TriggerEvent t m, PerformEvent t m, MonadIO (Performable m))
  => ProcessConfig t ByteString
  -> m (Process t ByteString ByteString)
createProcess procConfig = do
  channel <- liftIO newChan
  createProcessBufferingInput (readChan channel) (writeChan channel) procConfig


-- | Like 'System.Process.createProcess' but always redirects 'std_in', 'std_out', and 'std_err'.
redirectingCreateProcess :: P.CreateProcess -> IO (Handle, Handle, Handle, P.ProcessHandle)
redirectingCreateProcess p = do
  r <- P.createProcess p { std_in = P.CreatePipe, std_out = P.CreatePipe, std_err = P.CreatePipe }
  case r of
    (Just hIn, Just hOut, Just hErr, ph) -> pure (hIn, hOut, hErr, ph)
    _ -> error "Reflex.Process.redirectingCreateProcess: Created pipes were not returned by System.Process.createProcess."

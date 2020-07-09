{-|
Module: Reflex.Process
Description: Run processes and interact with them in reflex
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Reflex.Process
  ( createProcess
  , createProcessBufferingInput
  , defProcessConfig
  , unsafeCreateProcessWithHandles
  , Process(..)
  , ProcessConfig(..)
  , SendPipe (..)

  -- Deprecations
  , createRedirectedProcess
  ) where

import Control.Concurrent.Async (Async, async, waitBoth)
import Control.Concurrent.Chan (newChan, readChan, writeChan)
import Control.Exception (finally)
import Control.Monad (void, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Default (Default, def)
import Data.Function (fix)
import Data.Traversable (for)
import GHC.IO.Handle (Handle)
import qualified GHC.IO.Handle as H
import System.Exit (ExitCode)
import qualified System.Posix.Signals as P
import System.Process hiding (createProcess)
import qualified System.Process as P

import Reflex

data SendPipe i
  = SendPipe_Message i
  -- ^ A message that's sent to the underlying process. This does NOT include a trailing newline when sending your message.
  | SendPipe_EOF
  -- ^ Send an EOF to the underlying process. Once this is sent no further messages will be processed.
  | SendPipe_LastMessage i
  -- ^ Send the last message along with an EOF. Once this is sent no further messages will be processed.

-- | The inputs to a process
data ProcessConfig t i = ProcessConfig
  { _processConfig_stdin :: Event t i
  -- ^ @stdin@ input to be fed to the process
  , _processConfig_signal :: Event t P.Signal
  -- ^ Signals to send to the process
  }
instance Reflex t => Default (ProcessConfig t i) where
  -- | An alias for 'defProcessConfig'.
  def = defProcessConfig

-- | A default 'ProcessConfig' where @stdin@ and signals are never sent.
--
-- You can also use 'Data.Default.def'.
defProcessConfig :: Reflex t => ProcessConfig t i
defProcessConfig = ProcessConfig never never


-- | The output of a process
data Process t o e = Process
  { _process_handle :: P.ProcessHandle
  , _process_stdout :: Event t o
  -- ^ Fires whenever there's some new stdout output. Depending on the buffering strategy of the implementation, this could be anything from whole lines to individual characters.
  , _process_stderr :: Event t e
  -- ^ Fires whenever there's some new stderr output. See note on '_process_stdout'.
  , _process_exit :: Event t ExitCode
  -- ^ Fires when the process is over and no @stdout@ or @stderr@ data is left.
  -- Once this fires, no other 'Event's for the process will fire again.
  , _process_signal :: Event t P.Signal
  -- ^ Fires when a signal has actually been sent to the process (via '_processConfig_signal').
  }

-- | Create a process feeding it input using an 'Event' and exposing its output
-- 'Event's representing the process exit code, @stdout@, and @stderr@.
--
-- The @stdout@ and @stderr@ 'Handle's are line-buffered.
--
-- N.B. The process input is buffered with an unbounded channel! For more control of this,
-- use 'createProcessBufferingInput' directly.
--
-- N.B.: The 'std_in', 'std_out', and 'std_err' parameters of the
-- provided 'CreateProcess' are replaced with new pipes and all output is redirected
-- to those pipes.
createProcess
  :: (MonadIO m, TriggerEvent t m, PerformEvent t m, MonadIO (Performable m))
  => P.CreateProcess -- ^ Specification of process to create
  -> ProcessConfig t (SendPipe ByteString) -- ^ Reflex-level configuration for the process
  -> m (Process t ByteString ByteString)
createProcess p procConfig = do
  channel <- liftIO newChan
  createProcessBufferingInput (readChan channel) (writeChan channel) p procConfig

-- | Create a process feeding it input using an 'Event' and exposing its output with 'Event's
-- for its exit code, @stdout@, and @stderr@. The input is fed via a buffer represented by a
-- reading action and a writing action.
--
-- The @stdout@ and @stderr@ 'Handle's are line-buffered.
--
-- For example, you may use @Chan@ for an unbounded buffer (like 'createProcess' does) like this:
--
-- >  channel <- liftIO newChan
-- >  createProcessBufferingInput (readChan channel) (writeChan channel) myConfig
--
-- Similarly you could use @TChan@.
--
-- Bounded buffers may cause the Reflex network to block when you trigger an 'Event' that would
-- cause more data to be sent to a process whose @stdin@ is blocked.
--
-- If an unbounded channel would lead to too much memory usage you will want to consider
--   * speeding up the consuming process.
--   * buffering with the file system or another persistent storage to reduce memory usage.
--   * if your usa case allows, dropping 'Event's or messages that aren't important.
--
-- N.B.: The 'std_in', 'std_out', and 'std_err' parameters of the
-- provided 'CreateProcess' are replaced with new pipes and all output is redirected
-- to those pipes.
createProcessBufferingInput
  :: (MonadIO m, TriggerEvent t m, PerformEvent t m, MonadIO (Performable m))
  => IO (SendPipe ByteString)
  -- ^ An action that reads a value from the input stream buffer.
  -- This will run in a separate thread and must block when the buffer is empty or not ready.
  -> (SendPipe ByteString -> IO ())
  -- ^ An action that writes a value to the input stream buffer.
  -> P.CreateProcess -- ^ Specification of process to create
  -> ProcessConfig t (SendPipe ByteString) -- ^ Reflex-level configuration for the process
  -> m (Process t ByteString ByteString)
createProcessBufferingInput readBuffer writeBuffer = unsafeCreateProcessWithHandles input output output
  where
    input :: Handle -> IO (SendPipe ByteString -> IO ())
    input h = do
      H.hSetBuffering h H.NoBuffering
      void $ liftIO $ async $ fix $ \loop -> do
        newMessage <- readBuffer
        open <- H.hIsOpen h
        when open $ do
          writable <- H.hIsWritable h
          when writable $ do
            case newMessage of
              SendPipe_Message m -> BS.hPutStr h m *> loop
              SendPipe_LastMessage m -> BS.hPutStr h m *> H.hClose h
              SendPipe_EOF -> H.hClose h
      return writeBuffer
    output h trigger = do
      H.hSetBuffering h H.LineBuffering
      pure $ fix $ \go -> do
        open <- H.hIsOpen h
        when open $ do
          readable <- H.hIsReadable h
          when readable $ do
            out <- BS.hGetSome h 32768
            if BS.null out
              then H.hClose h
              else void (trigger out) *> go

-- | Runs a process and uses the given input and output handler functions to
-- interact with the process via the standard streams. Used to implement
-- 'createProcess'.
--
-- N.B.: The 'std_in', 'std_out', and 'std_err' parameters of the
-- provided 'CreateProcess' are replaced with new pipes and all output is redirected
-- to those pipes.
unsafeCreateProcessWithHandles
  :: forall t m i o e. (MonadIO m, TriggerEvent t m, PerformEvent t m, MonadIO (Performable m))
  => (Handle -> IO (i -> IO ()))
  -- ^ Builder for the standard input handler. The 'Handle' is the write end of the process' @stdin@ and
  -- the resulting @i -> IO ()@ is a function that writes each input 'Event t i' to into 'Handle'.
  -- This function must not block or the entire Reflex network will block.
  -> (Handle -> (o -> IO ()) -> IO (IO ()))
  -- ^ Builder for the standard output handler. The 'Handle' is the read end of the process' @stdout@ and
  -- the @o -> IO ()@ is a function that will trigger the output @Event t o@ when called. The resulting
  -- @IO ()@ will be run in a separate thread and must block until there is no more data in the 'Handle' to
  -- process.
  -> (Handle -> (e -> IO ()) -> IO (IO ()))
  -- ^ Builder for the standard error handler. The 'Handle' is the read end of the process' @stderr@ and
  -- the @e -> IO ()@ is a function that will trigger the output @Event t e@ when called. The resulting
  -- @IO ()@ will be run in a separate thread and must block until there is no more data in the 'Handle' to
  -- process.
  -> P.CreateProcess -- ^ Specification of process to create
  -> ProcessConfig t i -- ^ Reflex-level configuration for the process
  -> m (Process t o e)
unsafeCreateProcessWithHandles mkWriteStdInput mkReadStdOutput mkReadStdError p (ProcessConfig input signal) = do
  po <- liftIO $ P.createProcess p { std_in = P.CreatePipe, std_out = P.CreatePipe, std_err = P.CreatePipe }
  (hIn, hOut, hErr, ph) <- case po of
    (Just hIn, Just hOut, Just hErr, ph) -> pure (hIn, hOut, hErr, ph)
    _ -> error "Reflex.Process.unsafeCreateProcessWithHandles: Created pipes were not returned by System.Process.createProcess."
  writeInput :: i -> IO () <- liftIO $ mkWriteStdInput hIn
  performEvent_ $ liftIO . writeInput <$> input
  sigOut :: Event t (Maybe P.Signal) <- performEvent $ ffor signal $ \sig -> liftIO $ do
    mpid <- P.getPid ph
    for mpid $ \pid -> sig <$ P.signalProcess sig pid
  let
    output :: Handle -> m (Event t o, Async ())
    output h = do
      (e, trigger) <- newTriggerEvent
      reader <- liftIO $ mkReadStdOutput h trigger
      t <- liftIO $ async reader
      return (e, t)

    errOutput :: Handle -> m (Event t e, Async ())
    errOutput h = do
      (e, trigger) <- newTriggerEvent
      reader <- liftIO $ mkReadStdError h trigger
      t <- liftIO $ async reader
      return (e, t)

  (out, outThread) <- output hOut
  (err, errThread) <- errOutput hErr
  (ecOut, ecTrigger) <- newTriggerEvent
  void $ liftIO $ async $ flip finally (P.cleanupProcess (Just hIn, Just hOut, Just hErr, ph)) $ do
    waited <- waitForProcess ph
    _ <- waitBoth outThread errThread
    ecTrigger waited -- Output events should never fire after process completion
  return $ Process
    { _process_exit = ecOut
    , _process_stdout = out
    , _process_stderr = err
    , _process_signal = fmapMaybe id sigOut
    , _process_handle = ph
    }

{-# DEPRECATED createRedirectedProcess "Use unsafeCreateProcessWithHandles instead." #-}
createRedirectedProcess
  :: forall t m i o e. (MonadIO m, TriggerEvent t m, PerformEvent t m, MonadIO (Performable m))
  => (Handle -> IO (i -> IO ()))
  -> (Handle -> (o -> IO ()) -> IO (IO ()))
  -> (Handle -> (e -> IO ()) -> IO (IO ()))
  -> P.CreateProcess
  -> ProcessConfig t i
  -> m (Process t o e)
createRedirectedProcess = unsafeCreateProcessWithHandles

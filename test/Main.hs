{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Concurrent (MVar, newEmptyMVar, takeMVar, threadDelay, tryPutMVar)
import Control.Concurrent.Async (race)
import Control.Exception (finally)
import Control.Monad (guard, void)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first, second)
import Data.ByteString (ByteString)
import Data.IORef (newIORef, writeIORef, readIORef)
import Data.Foldable (traverse_)
import Reflex
import System.Timeout (timeout)
import qualified Data.ByteString.Char8 as BS
import qualified System.Process as P

import Reflex.Host.Headless
import Reflex.Process
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "reflex-process" $ do
    it "isn't blocked by a downstream non-blocking process" $ do
      timeoutWrapperAsync (checkFRPBlocking $ P.proc "cat" []) `shouldReturn` Right (Just Exit)
    it "isn't blocked by a downstream blocking process" $ do
      timeoutWrapperAsync (checkFRPBlocking $ P.proc "sleep" ["infinity"]) `shouldReturn` Right (Just Exit)
    it "sends messages on stdin and receives messages on stdout and stderr" $ do
      () <- runHeadlessApp $ do
        let
          -- Produces an event when the given message is seen on both stdout and stderr of the given process events
          getSawMessage procOut msg = do
            let filterMsg = mapMaybe (guard . (== msg))
            seen <- foldDyn ($) (False, False) $
              mergeWith (.)
                [ first (const True) <$ filterMsg (_process_stdout procOut)
                , second (const True) <$ filterMsg (_process_stderr procOut)
                ]
            pure $ mapMaybe (guard . (== (True, True))) $ updated seen

        rec
          procOut <- createProcess (P.proc "tee" ["/dev/stderr"]) $ ProcessConfig send never
          aWasSeen <- getSawMessage procOut "a\n"
          bWasSeen <- getSawMessage procOut "b\n"
          pb <- getPostBuild
          let
            send = leftmost
              [ SendPipe_Message "a\n" <$ pb
              , SendPipe_Message "b\n" <$ aWasSeen
              , SendPipe_LastMessage "c\n" <$ bWasSeen
              ]

        getSawMessage procOut "c\n"
      pure ()

    it "sends signals" $ do
      () <- runHeadlessApp $ void . _process_exit <$> sendSignalTest
      pure ()
    it "fires event when signal is sent" $ do
      () <- runHeadlessApp $ void . _process_signal <$> sendSignalTest
      pure ()

  where
    sendSignalTest :: MonadHeadlessApp t m => m (Process t ByteString ByteString)
    sendSignalTest = do
      (signal, signalTrigger) <- newTriggerEvent
      procOut <- createProcess (P.proc "sleep" ["infinity"]) $ ProcessConfig never signal
      liftIO $ threadDelay 1000000 *> signalTrigger 15 -- SIGTERM
      pure procOut

-- This datatype signals that the FRP network was able to exit on its own.
data Exit = Exit deriving (Show, Eq)

-- This creates the MVar with which the FRP network sends the exit signals, and
-- checks if a response from the FRP networks comes back in the allotted time.
timeoutWrapperAsync :: (MVar Exit -> IO ()) -> IO (Either () (Maybe Exit))
timeoutWrapperAsync wrapped = do
  exitCommMVar :: MVar Exit <- newEmptyMVar
  race (wrapped exitCommMVar) (timeout (3*1000000) (takeMVar exitCommMVar))

-- The frp network spawns, starts a timer, and tries to send some long input to
-- the created process. If the underlying process blocks and is able to block
-- the FRP network, the first tick of the timer will never happen, and, the Exit
-- signal will never be put in the MVar.
checkFRPBlocking :: P.CreateProcess -> MVar Exit -> IO ()
checkFRPBlocking downstreamProcess exitMVar = do
  spawnedProcess <- newIORef Nothing

  finally
    (runHeadlessApp $ do
      timer <- tickLossyFromPostBuildTime 1
      void $ performEvent $ liftIO (tryPutMVar exitMVar Exit) <$ timer

      (ev, evTrigger :: SendPipe ByteString -> IO ()) <- newTriggerEvent
      processOutput <- createProcess downstreamProcess $ ProcessConfig ev never
      liftIO $ writeIORef spawnedProcess (Just $ _process_handle processOutput)

      liftIO $ evTrigger $ SendPipe_Message $ veryLongByteString 'a'
      liftIO $ evTrigger $ SendPipe_Message $ veryLongByteString 'b'
      liftIO $ evTrigger $ SendPipe_LastMessage $ veryLongByteString 'c'

      void $ performEvent $ liftIO . BS.putStrLn <$> _process_stdout processOutput
      pure never
    )
    (readIORef spawnedProcess >>= traverse_ P.terminateProcess)

-- It's important to try this with long bytestrings to be sure that they're not
-- put in an operative system inter-process buffer.
veryLongByteString :: Char -> ByteString
veryLongByteString = BS.replicate 100000
--------------------------------------------------------------------------------

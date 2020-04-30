{-# language ScopedTypeVariables #-}

module Main where

import Control.Concurrent (MVar, newEmptyMVar, takeMVar, tryPutMVar)
import Control.Concurrent.Async (race)
import Control.Exception (finally)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.IORef (newIORef, writeIORef, readIORef)
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
      timeoutWrapperAsync (checkFRPBlocking goodProcess) `shouldReturn` Right (Just Exit)
    it "isn't blocked by a downstream blocking process" $ do
      timeoutWrapperAsync (checkFRPBlocking blockingProcess) `shouldReturn` Right (Just Exit)

-------------------------------CheckFrpBlocking---------------------------------
goodProcess, blockingProcess :: P.CreateProcess
goodProcess     = P.proc "cat" []
blockingProcess = P.proc "sleep" ["infinity"]

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

  let
    createProcessWithTermination cp = do
      procData <- redirectingCreateProcess cp
      writeIORef spawnedProcess (Just procData)
      pure procData

  finally
    (runHeadlessApp $ do
      timer <- tickLossyFromPostBuildTime 1
      void $ performEvent $ liftIO (tryPutMVar exitMVar Exit) <$ timer

      (ev, evTrigger :: ByteString -> IO ()) <- newTriggerEvent
      processOutput <- createProcess $ ProcessConfig ev never (createProcessWithTermination downstreamProcess)
      liftIO $ evTrigger $ veryLongByteString 'a'
      liftIO $ evTrigger $ veryLongByteString 'b'
      liftIO $ evTrigger $ veryLongByteString 'c'

      void $ performEvent $ liftIO . BS.putStrLn <$> _process_stdout processOutput
      pure never
    )
    (readIORef spawnedProcess >>= traverse_ (\(hIn, hOut, hErr, ph) -> P.cleanupProcess (Just hIn, Just hOut, Just hErr, ph)))

-- It's important to try this with long bytestrings to be sure that they're not
-- put in an operative system inter-process buffer.
veryLongByteString :: Char -> ByteString
veryLongByteString = BS.replicate 100000
--------------------------------------------------------------------------------

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
module HeadlessHost where

import Data.Maybe
import Reflex
import Control.Monad.Fix
import Control.Monad.Primitive
import Reflex.Host.Class
import Control.Monad.IO.Class
import Control.Monad.Ref
import Data.IORef
import Data.Dependent.Sum
import Control.Concurrent.Chan (newChan, readChan)
import Control.Monad (forM, forM_)
import Control.Monad.Identity (Identity(..))

type HeadlessResult t = Event t ()

type MonadHeadlessApp t m =
  ( Reflex t
  , MonadHold t m
  , MonadFix m
  , PrimMonad (HostFrame t)
  , ReflexHost t
  , MonadIO (HostFrame t)
  , Ref m ~ IORef
  , Ref (HostFrame t) ~ IORef
  , MonadRef (HostFrame t)
  , NotReady t m
  , TriggerEvent t m
  , PostBuild t m
  , PerformEvent t m
  , MonadIO m
  , MonadIO (Performable m)
  , Adjustable t m
  )

runHeadlessApp
  :: (forall t m. MonadHeadlessApp t m => m (HeadlessResult t))
  -> IO ()
runHeadlessApp guest =
  (runSpiderHost :: SpiderHost Global a -> IO a) $ do
    (postBuild, postBuildTriggerRef) <- newEventWithTriggerRef
    events <- liftIO newChan
    (result, fc@(FireCommand fire)) <- do
      hostPerformEventT $
        flip runPostBuildT postBuild $
          flip runTriggerEventT events $
            guest
    mPostBuildTrigger <- readRef postBuildTriggerRef
    forM_ mPostBuildTrigger $ \postBuildTrigger ->
      fire [postBuildTrigger :=> Identity ()] $ return ()
    shutdown <- subscribeEvent result
    fix $ \loop -> do
      ers <- liftIO $ readChan events
      stop <- fireEventTriggerRefs fc ers $ readEvent shutdown >>= \case
        Nothing -> return False
        Just _ -> return True
      if or stop
        then return ()
        else loop
  where
    -- TODO Some part of this is probably general enough to belong in reflex
    -- | Use the given 'FireCommand' to fire events that have subscribers
    -- and call the callback for the 'TriggerInvocation' of each.
    fireEventTriggerRefs
      :: (Monad (ReadPhase m), MonadIO m)
      => FireCommand t m
      -> [DSum (EventTriggerRef t) TriggerInvocation]
      -> ReadPhase m a
      -> m [a]
    fireEventTriggerRefs (FireCommand fire) ers rcb = do
      mes <- liftIO $
        forM ers $ \(EventTriggerRef er :=> TriggerInvocation a _) -> do
          me <- readIORef er
          return $ fmap (\e -> e :=> Identity a) me
      a <- fire (catMaybes mes) rcb
      liftIO $ forM_ ers $ \(_ :=> TriggerInvocation _ cb) -> cb
      return a

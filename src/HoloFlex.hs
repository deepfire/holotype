-- Derived from https://raw.githubusercontent.com/reflex-frp/reflex-platform/develop/examples/host.hs
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnicodeSyntax #-}
module HoloFlex where

import Prelude.Unicode
import Reflex
import Reflex.Host.Class (newEventWithTriggerRef, runHostFrame, fireEvents)
import Control.Concurrent (forkIO)
import qualified Control.Concurrent.Chan           as CH
import Control.Monad ((>>), unless)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Identity (Identity(..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Function ((&))
import Data.IORef (readIORef)
import Data.Dependent.Sum (DSum ((:=>)))
import qualified System.IO                         as Sys
import Text.Printf (printf)

-- Local imports
import HoloCube
import HoloInput
import WindowSys


-- | Define the type for apps using our host framework.  Programmers
--   will write programs of type @HoloFlex t m@ and use our
--   framework to run them.
--
--   In this framework, the user will write programs that take an input
--   event representing keystrokes and produce an output behavior representing
--   the current view to be shown.  This is similar to how polling-driven
--   output frameworks such as OpenGL will work.
type ReflexGLFWCtx t m = (Reflex t, MonadHold t m, MonadFix m, MonadIO m, MonadAdjust t m, PerformEvent t m, MonadIO (Performable m))
type ReflexGLFW t m
  = ReflexGLFWCtx t m
  ⇒ ObjectStream
  → Event t ()
  → Event t Frame
  → Event t InputEvent
  → m (Behavior t Bool)

-- | Run a program written in the framework.  This will do all the necessary
--   work to integrate the Reflex-based guest program with the outside world
--   via IO.
host ∷ (∀ t m. ReflexGLFW t m)
        -- ^ By keeping t and m abstract, we ensure that the user (the
        --   programmer using our framework) can't make any assumptions
        --   about which Reflex implementation is being used
     → IO ()
host myGuest = do
  Sys.hSetBuffering Sys.stdout Sys.NoBuffering

  win           ← makeGLWindow "holotype"
  eventChan     ← makeEventChannel win
  (renderer, stream)
                ← makeSimpleRenderedStream win (("canvasStream", "canvasMtl") ∷ (ObjArrayNameS, UniformNameS))
  rendererDrawFrame renderer

  -- Use the Spider implementation of Reflex.
  runSpiderHost $ do
    (s, sTriggerRef) ← newEventWithTriggerRef -- Setup event
    (f, fTriggerRef) ← newEventWithTriggerRef -- Frames
    (i, iTriggerRef) ← newEventWithTriggerRef -- Input

    -- Evaluate our user's program to set up the data flow graph.
    -- This usually only needs to be done once; the user can change the data
    -- flow graph arbitrarily in response to events.
    --
    -- runHostFrame is an efficient way of running a computation that
    -- can build arbitrary data flow graphs using 'hold' and 'sample'.
    --
    -- (The pure combinators in the Reflex class can be used in any context,
    -- so they don't need any special treatment - but inside runHostFrame is
    -- as good a place as any to run them.)
    (b, FireCommand threadedFire) ← hostPerformEventT $ myGuest stream s f i
    mTrig ← liftIO $ readIORef sTriggerRef
    case mTrig of
      Nothing   → pure ()
      Just trig → (>> pure ()) $ threadedFire [trig :=> Identity ()] $ pure ()

    -- Begin our event processing loop.
    let loop ∷ SpiderHost Global ()
        loop = do
          frame ← liftIO $ rendererSetupFrame renderer
          input ← readEvent eventChan

          (terminate, FireCommand threadedFire') ← hostPerformEventT $ sample b

          -- unless initDone $ do
          mTrig ← liftIO $ readIORef fTriggerRef
          case mTrig of
            Nothing   → pure [()]
            Just trig → threadedFire [trig :=> Identity frame] $ pure ()
          mTrig ← liftIO $ readIORef iTriggerRef
          case (mTrig, input) of
            (Nothing, _)   → pure [()]
            (_, Nothing)   → pure [()]
            (Just trig, Just input) →
              threadedFire [trig :=> Identity input] $ pure ()

          liftIO $ rendererDrawFrame renderer
          unless terminate $
            loop
    loop
    shutdownRenderer renderer

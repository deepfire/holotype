-- Derived from https://raw.githubusercontent.com/reflex-frp/reflex-platform/develop/examples/host.hs
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnicodeSyntax #-}
module Reflex.GLFW where

import           Prelude.Unicode

import           Control.Concurrent                        (forkIO)
import qualified Control.Concurrent.Chan           as CH
import           Control.Monad                             ((>>), unless)
import           Control.Monad.Fix                         (MonadFix)
import           Control.Monad.Identity                    (Identity(..))
import           Control.Monad.IO.Class                    (MonadIO, liftIO)
import           Data.Function                             ((&))
import           Data.IORef                                (readIORef)
import           Data.Dependent.Sum                        (DSum ((:=>)))

import qualified Graphics.GL.Core33 as GL
import "GLFW-b"  Graphics.UI.GLFW                  as GLFW

import           Reflex
import           Reflex.Host.Class                         (newEventWithTriggerRef, runHostFrame, fireEvents)

import qualified System.IO                         as Sys
import           Text.Printf                               (printf)

-- Local imports
import HoloInput


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
  ⇒ GLFW.Window
  → Event t GLFW.Window -- ^ The window to draw on, fired on every frame.
  → Event t InputEvent  -- ^ Fired whenever input happens, which isn't always the case..
  → m (Behavior t Bool)

defaultGLWindow ∷ (MonadIO m) ⇒ String → m GLFW.Window
defaultGLWindow title = liftIO $ do
  let (width, height) = (1024, 768)
  GLFW.init
  defaultWindowHints
  mapM_ windowHint
    [ WindowHint'ContextVersionMajor 3
    , WindowHint'ContextVersionMinor 3
    , WindowHint'OpenGLProfile OpenGLProfile'Core
    , WindowHint'OpenGLForwardCompat True
    ]
  Just win ← createWindow width height title Nothing Nothing
  makeContextCurrent $ Just win
  GL.glEnable GL.GL_FRAMEBUFFER_SRGB
  swapInterval 0
  return win

-- | A host that uses 'defaultGLwindow' and no cleanup.
simpleHost ∷ (MonadIO n)
           ⇒ String
           → (∀ t m. ReflexGLFW t m)
           → n ()
simpleHost name = host (defaultGLWindow name) (const $ pure ())

-- | Run a program written in the framework.  This will do all the necessary
--   work to integrate the Reflex-based guest program with the outside world
--   via IO.
host ∷ (MonadIO n)
     ⇒ n GLFW.Window
     → (GLFW.Window → n ())
     → (∀ t m. ReflexGLFW t m)
     → n ()
host glWindow cleanup myGuest = do
  win           ← glWindow
  eventChan     ← makeEventChannel win

  -- Use the Spider implementation of Reflex.
  liftIO $ runSpiderHost $ do
    (f, fTriggerRef) ← newEventWithTriggerRef -- Frames
    (i, iTriggerRef) ← newEventWithTriggerRef -- Input

    (b, FireCommand threadedFire) ← hostPerformEventT $ myGuest win f i

    let loop ∷ SpiderHost Global ()
        loop = do
          input ← readEvent eventChan

          (terminate, FireCommand _) ← hostPerformEventT $ sample b

          mTrig ← liftIO $ readIORef fTriggerRef
          case mTrig of
            Nothing   → pure [()]
            Just trig →
              threadedFire [trig :=> Identity win] $ pure ()
          mTrig ← liftIO $ readIORef iTriggerRef
          case (mTrig, input) of
            (Nothing, _)   → pure [()]
            (_, Nothing)   → pure [()]
            (Just trig, Just input) →
              threadedFire [trig :=> Identity input] $ pure ()

          unless terminate $
            loop
    -- Begin our event processing loop.
    loop
  cleanup win
  liftIO $ GLFW.destroyWindow win

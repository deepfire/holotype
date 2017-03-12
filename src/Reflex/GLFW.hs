-- Derived from:
--  1. https://raw.githubusercontent.com/reflex-frp/reflex-platform/develop/examples/host.hs
--  2. http://hackage.haskell.org/package/GLFW-b-demo-1.0.4/src/src/Main.hs
--
-- See the documentation of the ReflexGLFW type for caveats.
{-# OPTIONS_GHC -Wall -Wno-unused-do-bind -Wno-unused-top-binds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnicodeSyntax #-}
module Reflex.GLFW
  ( ReflexGLFW, ReflexGLFWCtx
  , withGLWindow, defaultGLWindowSetup
  , host, simpleHost
  , Input(..)
  , keyPress, stateIsPress
  )
where

import           Prelude.Unicode

import qualified Control.Concurrent.STM             as STM (TQueue, atomically, newTQueueIO, tryReadTQueue, writeTQueue)
import           Control.Monad                             (unless, when)
import           Control.Monad.Fix                         (MonadFix)
import           Control.Monad.Identity                    (Identity(..))
import           Control.Monad.IO.Class                    (MonadIO, liftIO)
import           Data.IORef                                (readIORef)
import           Data.Dependent.Sum                        (DSum ((:=>)))

import qualified Graphics.GL.Core33 as GL
import "GLFW-b"  Graphics.UI.GLFW                  as GLFW

import           Reflex
import           Reflex.Host.Class                         (newEventWithTriggerRef)


-- | Define the type for apps using our host framework.  Programmers
--   will write programs of type @ReflexGLFW t m@ and use our
--   framework to run them.
--
--   In this framework, the user will write programs that:
--   - take:
--     - the window object
--     - frame update events (XXX#1: currently at unconstrained rate)
--     - GLFW input events (XXX#2: currently, a non-configurable subset of them)
--   - and produce an output boolean behavior, that is interpreted
--     as a request for event loop termination.
--
--   XXX#3: currently, input events are injected at rate of one-per-frame.
--          Not sure how much of a problem this is..
--
type ReflexGLFW t m
  = ReflexGLFWCtx t m
  ⇒ GLFW.Window
  → Event t ()          -- ^ The initial "setup" event, that arrives just once, at the very first frame.
  → Event t GLFW.Window -- ^ The window to draw on, fired on every frame.
  → Event t Input       -- ^ Fired whenever input happens, which isn't always the case..
  → m (Behavior t Bool)
type ReflexGLFWCtx t m = (Reflex t, MonadHold t m, MonadFix m, MonadIO m, MonadAdjust t m, PerformEvent t m, MonadIO (Performable m))


-- * Window management
--
-- | A default GLFW window setup function.
defaultGLWindowSetup ∷ (MonadIO m) ⇒ GLFW.Window → m ()
defaultGLWindowSetup _ = liftIO $ do
  GL.glEnable GL.GL_FRAMEBUFFER_SRGB
  swapInterval 0

-- * GLFW-b is made to be very close to the C API, so creating a window is pretty
--   clunky by Haskell standards.  A higher-level API would have some function
--   like 'withWindow'.
withGLWindow ∷ (MonadIO m) ⇒ Int → Int → String → (GLFW.Window → m ()) → m ()
withGLWindow width height title f = do
    liftIO $ GLFW.setErrorCallback $ Just simpleErrorCallback
    r ← liftIO $ GLFW.init
    when r $ do
      liftIO $ defaultWindowHints
      liftIO $ mapM_ windowHint
        [ WindowHint'ContextVersionMajor 3
        , WindowHint'ContextVersionMinor 3
        , WindowHint'OpenGLProfile OpenGLProfile'Core
        , WindowHint'OpenGLForwardCompat True ]
      m ← liftIO $ GLFW.createWindow width height title Nothing Nothing
      case m of
        Just win → do
          liftIO $ GLFW.makeContextCurrent m
          f win
          liftIO $ GLFW.setErrorCallback $ Just simpleErrorCallback
          liftIO $ GLFW.destroyWindow win
        Nothing → pure ()
      liftIO $ GLFW.terminate
  where
    simpleErrorCallback e s =
        putStrLn $ unwords [show e, show s]


-- * Events
--
stateIsPress ∷ GLFW.KeyState → Bool
stateIsPress GLFW.KeyState'Pressed   = True
stateIsPress GLFW.KeyState'Repeating = True
stateIsPress _                       = False

keyPress ∷ Reflex t ⇒ GLFW.Key → Event t Input → Event t Input
keyPress key inputE = flip ffilter inputE $
                      \case EventKey _ k _ ks _ → k ≡ key ∧ stateIsPress ks
                            _                   → False

-- | The type describing deliverable GLFW events.
data Input =
    EventError           !GLFW.Error !String
  | EventWindowPos       !GLFW.Window !Int !Int
  | EventWindowSize      !GLFW.Window !Int !Int
  | EventWindowClose     !GLFW.Window
  | EventWindowRefresh   !GLFW.Window
  | EventWindowFocus     !GLFW.Window !GLFW.FocusState
  | EventWindowIconify   !GLFW.Window !GLFW.IconifyState
  | EventFramebufferSize !GLFW.Window !Int !Int
  | EventMouseButton     !GLFW.Window !GLFW.MouseButton !GLFW.MouseButtonState !GLFW.ModifierKeys
  | EventCursorPos       !GLFW.Window !Double !Double
  | EventCursorEnter     !GLFW.Window !GLFW.CursorState
  | EventScroll          !GLFW.Window !Double !Double
  | EventKey             !GLFW.Window !GLFW.Key !Int !GLFW.KeyState !GLFW.ModifierKeys
  | EventChar            !GLFW.Window !Char
  deriving Show

errorCallback           ∷ STM.TQueue Input → GLFW.Error → String                                                          → IO ()
windowPosCallback       ∷ STM.TQueue Input → GLFW.Window → Int → Int                                                      → IO ()
windowSizeCallback      ∷ STM.TQueue Input → GLFW.Window → Int → Int                                                      → IO ()
windowCloseCallback     ∷ STM.TQueue Input → GLFW.Window                                                                  → IO ()
windowRefreshCallback   ∷ STM.TQueue Input → GLFW.Window                                                                  → IO ()
windowFocusCallback     ∷ STM.TQueue Input → GLFW.Window → GLFW.FocusState                                                → IO ()
windowIconifyCallback   ∷ STM.TQueue Input → GLFW.Window → GLFW.IconifyState                                              → IO ()
framebufferSizeCallback ∷ STM.TQueue Input → GLFW.Window → Int → Int                                                      → IO ()
mouseButtonCallback     ∷ STM.TQueue Input → GLFW.Window → GLFW.MouseButton   → GLFW.MouseButtonState → GLFW.ModifierKeys → IO ()
cursorPosCallback       ∷ STM.TQueue Input → GLFW.Window → Double → Double                                                → IO ()
cursorEnterCallback     ∷ STM.TQueue Input → GLFW.Window → GLFW.CursorState                                               → IO ()
scrollCallback          ∷ STM.TQueue Input → GLFW.Window → Double → Double                                                → IO ()
keyCallback             ∷ STM.TQueue Input → GLFW.Window → GLFW.Key → Int → GLFW.KeyState → GLFW.ModifierKeys             → IO ()
charCallback            ∷ STM.TQueue Input → GLFW.Window → Char                                                           → IO ()
errorCallback           tc     e s        = STM.atomically $ STM.writeTQueue tc $ EventError           e s
windowPosCallback       tc win x y        = STM.atomically $ STM.writeTQueue tc $ EventWindowPos       win x y
windowSizeCallback      tc win w h        = STM.atomically $ STM.writeTQueue tc $ EventWindowSize      win w h
windowCloseCallback     tc win            = STM.atomically $ STM.writeTQueue tc $ EventWindowClose     win
windowRefreshCallback   tc win            = STM.atomically $ STM.writeTQueue tc $ EventWindowRefresh   win
windowFocusCallback     tc win fa         = STM.atomically $ STM.writeTQueue tc $ EventWindowFocus     win fa
windowIconifyCallback   tc win ia         = STM.atomically $ STM.writeTQueue tc $ EventWindowIconify   win ia
framebufferSizeCallback tc win w h        = STM.atomically $ STM.writeTQueue tc $ EventFramebufferSize win w h
mouseButtonCallback     tc win mb mba mk  = STM.atomically $ STM.writeTQueue tc $ EventMouseButton     win mb mba mk
cursorPosCallback       tc win x y        = STM.atomically $ STM.writeTQueue tc $ EventCursorPos       win x y
cursorEnterCallback     tc win ca         = STM.atomically $ STM.writeTQueue tc $ EventCursorEnter     win ca
scrollCallback          tc win x y        = STM.atomically $ STM.writeTQueue tc $ EventScroll          win x y
keyCallback             tc win k sc ka mk = STM.atomically $ STM.writeTQueue tc $ EventKey             win k sc ka mk
charCallback            tc win c          = STM.atomically $ STM.writeTQueue tc $ EventChar            win c

enableErrorEvents            ∷ MonadIO m ⇒ STM.TQueue Input → m ()
enableWindowPosEvents        ∷ MonadIO m ⇒ STM.TQueue Input → GLFW.Window → m ()
enableWindowSizeEvents       ∷ MonadIO m ⇒ STM.TQueue Input → GLFW.Window → m ()
enableWindowCloseEvents      ∷ MonadIO m ⇒ STM.TQueue Input → GLFW.Window → m ()
enableWindowRefreshEvents    ∷ MonadIO m ⇒ STM.TQueue Input → GLFW.Window → m ()
enableWindowFocusEvents      ∷ MonadIO m ⇒ STM.TQueue Input → GLFW.Window → m ()
enableWindowIconifyEvents    ∷ MonadIO m ⇒ STM.TQueue Input → GLFW.Window → m ()
enableFramebufferSizeEvents  ∷ MonadIO m ⇒ STM.TQueue Input → GLFW.Window → m ()
enableMouseButtonEvents      ∷ MonadIO m ⇒ STM.TQueue Input → GLFW.Window → m ()
enableCursorPosEvents        ∷ MonadIO m ⇒ STM.TQueue Input → GLFW.Window → m ()
enableCursorEnterEvents      ∷ MonadIO m ⇒ STM.TQueue Input → GLFW.Window → m ()
enableScrollEvents           ∷ MonadIO m ⇒ STM.TQueue Input → GLFW.Window → m ()
enableKeyEvents              ∷ MonadIO m ⇒ STM.TQueue Input → GLFW.Window → m ()
enableCharEvents             ∷ MonadIO m ⇒ STM.TQueue Input → GLFW.Window → m ()
disableErrorEvents           ∷ MonadIO m ⇒                                  m ()
disableWindowPosEvents       ∷ MonadIO m ⇒                    GLFW.Window → m ()
disableWindowSizeEvents      ∷ MonadIO m ⇒                    GLFW.Window → m ()
disableWindowCloseEvents     ∷ MonadIO m ⇒                    GLFW.Window → m ()
disableWindowRefreshEvents   ∷ MonadIO m ⇒                    GLFW.Window → m ()
disableWindowFocusEvents     ∷ MonadIO m ⇒                    GLFW.Window → m ()
disableWindowIconifyEvents   ∷ MonadIO m ⇒                    GLFW.Window → m ()
disableFramebufferSizeEvents ∷ MonadIO m ⇒                    GLFW.Window → m ()
disableMouseButtonEvents     ∷ MonadIO m ⇒                    GLFW.Window → m ()
disableCursorPosEvents       ∷ MonadIO m ⇒                    GLFW.Window → m ()
disableCursorEnterEvents     ∷ MonadIO m ⇒                    GLFW.Window → m ()
disableScrollEvents          ∷ MonadIO m ⇒                    GLFW.Window → m ()
disableKeyEvents             ∷ MonadIO m ⇒                    GLFW.Window → m ()
disableCharEvents            ∷ MonadIO m ⇒                    GLFW.Window → m ()
enableErrorEvents            iq     = liftIO $ GLFW.setErrorCallback               $ Just $ errorCallback           iq
enableWindowPosEvents        iq win = liftIO $ GLFW.setWindowPosCallback       win $ Just $ windowPosCallback       iq
enableWindowSizeEvents       iq win = liftIO $ GLFW.setWindowSizeCallback      win $ Just $ windowSizeCallback      iq
enableWindowCloseEvents      iq win = liftIO $ GLFW.setWindowCloseCallback     win $ Just $ windowCloseCallback     iq
enableWindowRefreshEvents    iq win = liftIO $ GLFW.setWindowRefreshCallback   win $ Just $ windowRefreshCallback   iq
enableWindowFocusEvents      iq win = liftIO $ GLFW.setWindowFocusCallback     win $ Just $ windowFocusCallback     iq
enableWindowIconifyEvents    iq win = liftIO $ GLFW.setWindowIconifyCallback   win $ Just $ windowIconifyCallback   iq
enableFramebufferSizeEvents  iq win = liftIO $ GLFW.setFramebufferSizeCallback win $ Just $ framebufferSizeCallback iq
enableMouseButtonEvents      iq win = liftIO $ GLFW.setMouseButtonCallback     win $ Just $ mouseButtonCallback     iq
enableCursorPosEvents        iq win = liftIO $ GLFW.setCursorPosCallback       win $ Just $ cursorPosCallback       iq
enableCursorEnterEvents      iq win = liftIO $ GLFW.setCursorEnterCallback     win $ Just $ cursorEnterCallback     iq
enableScrollEvents           iq win = liftIO $ GLFW.setScrollCallback          win $ Just $ scrollCallback          iq
enableKeyEvents              iq win = liftIO $ GLFW.setKeyCallback             win $ Just $ keyCallback             iq
enableCharEvents             iq win = liftIO $ GLFW.setCharCallback            win $ Just $ charCallback            iq
disableErrorEvents                  = liftIO $ GLFW.setErrorCallback               $ Nothing
disableWindowPosEvents          win = liftIO $ GLFW.setWindowPosCallback       win $ Nothing
disableWindowSizeEvents         win = liftIO $ GLFW.setWindowSizeCallback      win $ Nothing
disableWindowCloseEvents        win = liftIO $ GLFW.setWindowCloseCallback     win $ Nothing
disableWindowRefreshEvents      win = liftIO $ GLFW.setWindowRefreshCallback   win $ Nothing
disableWindowFocusEvents        win = liftIO $ GLFW.setWindowFocusCallback     win $ Nothing
disableWindowIconifyEvents      win = liftIO $ GLFW.setWindowIconifyCallback   win $ Nothing
disableFramebufferSizeEvents    win = liftIO $ GLFW.setFramebufferSizeCallback win $ Nothing
disableMouseButtonEvents        win = liftIO $ GLFW.setMouseButtonCallback     win $ Nothing
disableCursorPosEvents          win = liftIO $ GLFW.setCursorPosCallback       win $ Nothing
disableCursorEnterEvents        win = liftIO $ GLFW.setCursorEnterCallback     win $ Nothing
disableScrollEvents             win = liftIO $ GLFW.setScrollCallback          win $ Nothing
disableKeyEvents                win = liftIO $ GLFW.setKeyCallback             win $ Nothing
disableCharEvents               win = liftIO $ GLFW.setCharCallback            win $ Nothing

makeInputQueue ∷ (MonadIO m) ⇒ GLFW.Window → m (STM.TQueue Input)
makeInputQueue win = liftIO $ do
  iq ← STM.newTQueueIO
  enableErrorEvents       iq
  enableMouseButtonEvents iq win
  enableKeyEvents         iq win
  enableCharEvents        iq win
  pure iq

readInput ∷ (MonadIO m) ⇒ STM.TQueue Input → m (Maybe Input)
readInput queue = liftIO $ do
  GLFW.pollEvents
  STM.atomically $ STM.tryReadTQueue queue

-- | A host that uses 'defaultGLwindow' and no cleanup.
simpleHost ∷ (MonadIO io)
           ⇒ String
           → (∀ t m. ReflexGLFW t m)
           → io ()
simpleHost title guest =
  withGLWindow 1024 768 title $ \win → do
    defaultGLWindowSetup win
    host win guest

-- | Run a program written in the framework.  This will do all the necessary
--   work to integrate the Reflex-based guest program with the outside world
--   via IO.
host ∷ (MonadIO io)
     ⇒ GLFW.Window
     → (∀ t m. ReflexGLFW t m)
     → io ()
host win myGuest = do
  queue ← makeInputQueue win

  -- Use the Spider implementation of Reflex.
  liftIO $ runSpiderHost $ do
    (s, sTriggerRef) ← newEventWithTriggerRef -- Setup
    (f, fTriggerRef) ← newEventWithTriggerRef -- Frames
    (i, iTriggerRef) ← newEventWithTriggerRef -- Input

    (b, FireCommand threadedFire) ← hostPerformEventT $ myGuest win s f i
    mTrig ← liftIO $ readIORef sTriggerRef
    case mTrig of
      Nothing   → pure ()
      Just trig → (>> pure ()) $ threadedFire [trig :=> Identity ()] $ pure ()

    let loop ∷ SpiderHost Global ()
        loop = do
          mInput ← readInput queue

          (stopRequest, FireCommand _) ← hostPerformEventT $ sample b

          mTrig' ← liftIO $ readIORef fTriggerRef
          case mTrig' of
            Nothing   → pure [()]
            Just trig →
              threadedFire [trig :=> Identity win] $ pure ()
          mTrig'' ← liftIO $ readIORef iTriggerRef
          case (mTrig'', mInput) of
            (Nothing, _)   → pure [()]
            (_, Nothing)   → pure [()]
            (Just trig, Just input) →
              threadedFire [trig :=> Identity input] $ pure ()

          unless stopRequest $
            loop
    -- Begin our event processing loop.
    loop

-- Derived from:
--  1. https://raw.githubusercontent.com/reflex-frp/reflex-platform/develop/examples/host.hs
--  2. http://hackage.haskell.org/package/GLFW-b-demo-1.0.4/src/src/Main.hs
--
-- See the documentation of the ReflexGLFW type for caveats.
{-# OPTIONS_GHC -Wall -Wno-unused-do-bind -Wno-unused-top-binds -Wno-unticked-promoted-constructors #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UnicodeSyntax #-}
module Reflex.GLFW
  ( ReflexGLFW, ReflexGLFWCtx
  , withGLWindow, defaultGLWindowSetup
  , host, simpleHost
  , EventType(..), Input(..), InputU(..)
  -- * Samples
  , JoystickSample(..), sampleJoystick
  , PointerSample(..), samplePointer, pointerSampleZero
  , mouseButtonStateIsPress, keyStateIsPress
  , cursorPosCoords
  , scrollX, scrollY
  -- * Input events
  , filterError
  , filterWindowRefresh
  , filterFramebufferSize
  , filterScroll
  , filterKey, filterKey', filterKeyPress
  , filterMouseButton, filterMouseButton', filterMouseButtonStateChange
  , filterCursorPos
  , filterChar
  -- * Input dynamics
  , mouseButtonState
  , keyState
  )
where

import           Prelude                            hiding (Char)
import qualified Prelude                            as Prelude
import           Prelude.Unicode

import qualified Control.Concurrent.STM             as STM (TQueue, atomically, newTQueueIO, tryReadTQueue, writeTQueue)
import           Control.Lens
import           Control.Monad                             (unless, when)
import           Control.Monad.Fix                         (MonadFix)
import           Control.Monad.Identity                    (Identity(..))
import           Control.Monad.IO.Class                    (MonadIO, liftIO)
import           Data.IORef                                (readIORef)
import           Data.Dependent.Sum                        (DSum ((:=>)))

import qualified Graphics.GL.Core33                 as GL
import qualified "GLFW-b" Graphics.UI.GLFW          as GL

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
  ⇒ GL.Window
  → Event t ()          -- ^ The initial "setup" event, that arrives just once, at the very first frame.
  → Event t GL.Window   -- ^ The window to draw on, fired on every frame.
  → Event t InputU      -- ^ Fired whenever input happens, which isn't always the case..
  → m (Behavior t Bool)
type ReflexGLFWCtx t m = (Reflex t, MonadHold t m, MonadFix m, MonadIO m, MonadAdjust t m, PerformEvent t m, MonadIO (Performable m))


-- * Window management
--
-- | A default GLFW window setup function.
defaultGLWindowSetup ∷ (MonadIO m) ⇒ GL.Window → m ()
defaultGLWindowSetup _ = liftIO $ do
  GL.glEnable GL.GL_FRAMEBUFFER_SRGB
  GL.swapInterval 0

-- * GLFW-b is made to be very close to the C API, so creating a window is pretty
--   clunky by Haskell standards.  A higher-level API would have some function
--   like 'withWindow'.
withGLWindow ∷ (MonadIO m) ⇒ Int → Int → String → (GL.Window → m ()) → m ()
withGLWindow width height title f = do
    liftIO $ GL.setErrorCallback $ Just simpleErrorCallback
    r ← liftIO $ GL.init
    when r $ do
      liftIO $ defaultWindowHints
      liftIO $ mapM_ windowHint
        [ WindowHint'ContextVersionMajor 3
        , WindowHint'ContextVersionMinor 3
        , WindowHint'OpenGLProfile OpenGLProfile'Core
        , WindowHint'OpenGLForwardCompat True ]
      m ← liftIO $ GL.createWindow width height title Nothing Nothing
      case m of
        Just win → do
          liftIO $ GL.makeContextCurrent m
          f win
          liftIO $ GL.setErrorCallback $ Just simpleErrorCallback
          liftIO $ GL.destroyWindow win
        Nothing → pure ()
      liftIO $ GL.terminate
  where
    simpleErrorCallback e s =
        putStrLn $ unwords [show e, show s]


-- * Samples
newtype JoystickSample = JoystickSample { jSample ∷ [Double] }
newtype PointerSample  = PointerSample  { pSample ∷ (Double, Double) }

pointerSampleZero ∷ PointerSample
pointerSampleZero = PointerSample (0, 0)

sampleJoystick ∷ ReflexGLFWCtx t m ⇒ GL.Joystick → Event t a → m (Event t JoystickSample)
sampleJoystick js ev = do
  e ← fmapMaybe id <$> (performEvent $ ev <&> (const $ liftIO $ GL.getJoystickAxes js))
  pure $ JoystickSample <$> e

samplePointer ∷ ReflexGLFWCtx t m ⇒ GL.Window → Event t a → m (Event t PointerSample)
samplePointer win ev = do
  e ← performEvent $ ev <&> (const $ liftIO $ GL.getCursorPos win)
  pure $ PointerSample <$> e

keyStateIsPress ∷ GL.KeyState → Bool
keyStateIsPress GL.KeyState'Pressed   = True
keyStateIsPress GL.KeyState'Repeating = True
keyStateIsPress _                     = False

mouseButtonStateIsPress ∷ GL.MouseButtonState → Bool
mouseButtonStateIsPress GL.MouseButtonState'Pressed = True
mouseButtonStateIsPress _                           = False

cursorPosCoords ∷ Input CursorPos → (Double, Double)
cursorPosCoords (EventCursorPos _ x y) = (x, y)

scrollX, scrollY ∷ Input Scroll → Double
scrollX (EventScroll _ x _) = x
scrollY (EventScroll _ _ y) = y


-- * Events
--
filterError           ∷ Reflex t ⇒ Event t InputU → Event t (Input Error)
filterWindowRefresh   ∷ Reflex t ⇒ Event t InputU → Event t (Input WindowRefresh)
filterFramebufferSize ∷ Reflex t ⇒ Event t InputU → Event t (Input FramebufferSize)
filterScroll          ∷ Reflex t ⇒ Event t InputU → Event t (Input Scroll)
filterKey             ∷ Reflex t ⇒ Event t InputU → Event t (Input Key)
filterMouseButton     ∷ Reflex t ⇒ Event t InputU → Event t (Input MouseButton)
filterCursorPos       ∷ Reflex t ⇒ Event t InputU → Event t (Input CursorPos)
filterChar            ∷ Reflex t ⇒ Event t InputU → Event t (Input Char)
filterError           = fmapMaybe (\case U e@(EventError _ _)             → Just e; _ → Nothing)
filterWindowRefresh   = fmapMaybe (\case U e@(EventWindowRefresh _)       → Just e; _ → Nothing)
filterFramebufferSize = fmapMaybe (\case U e@(EventFramebufferSize _ _ _) → Just e; _ → Nothing)
filterScroll          = fmapMaybe (\case U e@(EventScroll _ _ _)          → Just e; _ → Nothing)
filterKey             = fmapMaybe (\case U e@(EventKey _ _ _ _  _)        → Just e; _ → Nothing)
filterMouseButton     = fmapMaybe (\case U e@(EventMouseButton _ _ _ _)   → Just e; _ → Nothing)
filterCursorPos       = fmapMaybe (\case U e@(EventCursorPos _ _ _)       → Just e; _ → Nothing)
filterChar            = fmapMaybe (\case U e@(EventChar _ _)              → Just e; _ → Nothing)

filterMouseButton'      ∷ Reflex t   ⇒ GL.MouseButton → Event t (Input MouseButton) → Event t (Input MouseButton)
filterMouseButton'           btn       = ffilter (\case EventMouseButton _ k _ _ → k ≡ btn)
filterMouseButtonStateChange ∷ Reflex t ⇒ GL.MouseButton → Bool → Event t (Input MouseButton) → Event t (Input MouseButton)
filterMouseButtonStateChange btn state = ffilter (\case EventMouseButton _ k st _ → k ≡ btn ∧ mouseButtonStateIsPress st ≡ state)

filterKey', filterKeyPress       ∷ Reflex t   ⇒ GL.Key         → Event t (Input Key)         → Event t (Input Key)
filterKey'     key = ffilter (\case EventKey _ k _ _  _ → k ≡ key)
filterKeyPress key = ffilter (\case EventKey _ k _ ks _ → k ≡ key ∧ keyStateIsPress ks)

mouseButtonState ∷ ReflexGLFWCtx t m ⇒ GL.MouseButton → Event t (Input MouseButton) → m (Dynamic t (Maybe (Double, Double)))
mouseButtonState btn inputE = do
  -- XXX/optimise:  maybe 'toggle' is to help us here?
  e ← performEvent $ filterMouseButton' btn inputE <&>
    (\case EventMouseButton win _ ks _ → do
             if mouseButtonStateIsPress ks
             then Just <$> (liftIO $ GL.getCursorPos win)
             else pure Nothing)
  holdDyn Nothing e

keyState ∷ ReflexGLFWCtx t m ⇒ GL.Key → Event t (Input Key) → m (Dynamic t Bool)
keyState key inputE =
  -- XXX/optimise:  maybe 'toggle' is to help us here?
  holdDyn False $ (\case EventKey _ _ _ ks _       → keyStateIsPress ks)         <$> filterKey' key inputE

-- | The type describing deliverable GLFW events.
data EventType
  = Error
  | WindowPos
  | WindowSize
  | WindowClose
  | WindowRefresh
  | WindowFocus
  | WindowIconify
  | FramebufferSize
  | MouseButton
  | CursorPos
  | CursorEnter
  | Scroll
  | Key
  | Char

data InputU where
  U ∷ Input k → InputU
deriving instance Show InputU

data Input (k ∷ EventType) where
  EventError           ∷ !GL.Error  → !String                                                    → Input Error
  EventWindowPos       ∷ !GL.Window → !Int → !Int                                                → Input WindowPos
  EventWindowSize      ∷ !GL.Window → !Int → !Int                                                → Input WindowSize
  EventWindowClose     ∷ !GL.Window                                                              → Input WindowClose
  EventWindowRefresh   ∷ !GL.Window                                                              → Input WindowRefresh
  EventWindowFocus     ∷ !GL.Window → !GL.FocusState                                             → Input WindowFocus
  EventWindowIconify   ∷ !GL.Window → !GL.IconifyState                                           → Input WindowIconify
  EventFramebufferSize ∷ !GL.Window → !Int → !Int                                                → Input FramebufferSize
  EventMouseButton     ∷ !GL.Window → !GL.MouseButton → !GL.MouseButtonState → !GL.ModifierKeys  → Input MouseButton
  EventCursorPos       ∷ !GL.Window → !Double → !Double                                          → Input CursorPos
  EventCursorEnter     ∷ !GL.Window → !GL.CursorState                                            → Input CursorEnter
  EventScroll          ∷ !GL.Window → !Double → !Double                                          → Input Scroll
  EventKey             ∷ !GL.Window → !GL.Key → !Int → !GL.KeyState → !GL.ModifierKeys           → Input Key
  EventChar            ∷ !GL.Window → !Prelude.Char                                              → Input Char
deriving instance Show (Input k)

errorCallback           ∷ STM.TQueue InputU → GL.Error → String                                                    → IO ()
windowPosCallback       ∷ STM.TQueue InputU → GL.Window → Int → Int                                                → IO ()
windowSizeCallback      ∷ STM.TQueue InputU → GL.Window → Int → Int                                                → IO ()
windowCloseCallback     ∷ STM.TQueue InputU → GL.Window                                                            → IO ()
windowRefreshCallback   ∷ STM.TQueue InputU → GL.Window                                                            → IO ()
windowFocusCallback     ∷ STM.TQueue InputU → GL.Window → GL.FocusState                                            → IO ()
windowIconifyCallback   ∷ STM.TQueue InputU → GL.Window → GL.IconifyState                                          → IO ()
framebufferSizeCallback ∷ STM.TQueue InputU → GL.Window → Int → Int                                                → IO ()
mouseButtonCallback     ∷ STM.TQueue InputU → GL.Window → GL.MouseButton   → GL.MouseButtonState → GL.ModifierKeys → IO ()
cursorPosCallback       ∷ STM.TQueue InputU → GL.Window → Double → Double                                          → IO ()
cursorEnterCallback     ∷ STM.TQueue InputU → GL.Window → GL.CursorState                                           → IO ()
scrollCallback          ∷ STM.TQueue InputU → GL.Window → Double → Double                                          → IO ()
keyCallback             ∷ STM.TQueue InputU → GL.Window → GL.Key → Int → GL.KeyState → GL.ModifierKeys             → IO ()
charCallback            ∷ STM.TQueue InputU → GL.Window → Prelude.Char                                             → IO ()
errorCallback           tc     e s        = STM.atomically ∘ STM.writeTQueue tc ∘ U $ EventError           e s
windowPosCallback       tc win x y        = STM.atomically ∘ STM.writeTQueue tc ∘ U $ EventWindowPos       win x y
windowSizeCallback      tc win w h        = STM.atomically ∘ STM.writeTQueue tc ∘ U $ EventWindowSize      win w h
windowCloseCallback     tc win            = STM.atomically ∘ STM.writeTQueue tc ∘ U $ EventWindowClose     win
windowRefreshCallback   tc win            = STM.atomically ∘ STM.writeTQueue tc ∘ U $ EventWindowRefresh   win
windowFocusCallback     tc win fa         = STM.atomically ∘ STM.writeTQueue tc ∘ U $ EventWindowFocus     win fa
windowIconifyCallback   tc win ia         = STM.atomically ∘ STM.writeTQueue tc ∘ U $ EventWindowIconify   win ia
framebufferSizeCallback tc win w h        = STM.atomically ∘ STM.writeTQueue tc ∘ U $ EventFramebufferSize win w h
mouseButtonCallback     tc win mb mba mk  = STM.atomically ∘ STM.writeTQueue tc ∘ U $ EventMouseButton     win mb mba mk
cursorPosCallback       tc win x y        = STM.atomically ∘ STM.writeTQueue tc ∘ U $ EventCursorPos       win x y
cursorEnterCallback     tc win ca         = STM.atomically ∘ STM.writeTQueue tc ∘ U $ EventCursorEnter     win ca
scrollCallback          tc win x y        = STM.atomically ∘ STM.writeTQueue tc ∘ U $ EventScroll          win x y
keyCallback             tc win k sc ka mk = STM.atomically ∘ STM.writeTQueue tc ∘ U $ EventKey             win k sc ka mk
charCallback            tc win c          = STM.atomically ∘ STM.writeTQueue tc ∘ U $ EventChar            win c

enableErrorEvents            ∷ MonadIO m ⇒ STM.TQueue InputU →             m ()
enableWindowPosEvents        ∷ MonadIO m ⇒ STM.TQueue InputU → GL.Window → m ()
enableWindowSizeEvents       ∷ MonadIO m ⇒ STM.TQueue InputU → GL.Window → m ()
enableWindowCloseEvents      ∷ MonadIO m ⇒ STM.TQueue InputU → GL.Window → m ()
enableWindowRefreshEvents    ∷ MonadIO m ⇒ STM.TQueue InputU → GL.Window → m ()
enableWindowFocusEvents      ∷ MonadIO m ⇒ STM.TQueue InputU → GL.Window → m ()
enableWindowIconifyEvents    ∷ MonadIO m ⇒ STM.TQueue InputU → GL.Window → m ()
enableFramebufferSizeEvents  ∷ MonadIO m ⇒ STM.TQueue InputU → GL.Window → m ()
enableMouseButtonEvents      ∷ MonadIO m ⇒ STM.TQueue InputU → GL.Window → m ()
enableCursorPosEvents        ∷ MonadIO m ⇒ STM.TQueue InputU → GL.Window → m ()
enableCursorEnterEvents      ∷ MonadIO m ⇒ STM.TQueue InputU → GL.Window → m ()
enableScrollEvents           ∷ MonadIO m ⇒ STM.TQueue InputU → GL.Window → m ()
enableKeyEvents              ∷ MonadIO m ⇒ STM.TQueue InputU → GL.Window → m ()
enableCharEvents             ∷ MonadIO m ⇒ STM.TQueue InputU → GL.Window → m ()
disableErrorEvents           ∷ MonadIO m ⇒                                 m ()
disableWindowPosEvents       ∷ MonadIO m ⇒                     GL.Window → m ()
disableWindowSizeEvents      ∷ MonadIO m ⇒                     GL.Window → m ()
disableWindowCloseEvents     ∷ MonadIO m ⇒                     GL.Window → m ()
disableWindowRefreshEvents   ∷ MonadIO m ⇒                     GL.Window → m ()
disableWindowFocusEvents     ∷ MonadIO m ⇒                     GL.Window → m ()
disableWindowIconifyEvents   ∷ MonadIO m ⇒                     GL.Window → m ()
disableFramebufferSizeEvents ∷ MonadIO m ⇒                     GL.Window → m ()
disableMouseButtonEvents     ∷ MonadIO m ⇒                     GL.Window → m ()
disableCursorPosEvents       ∷ MonadIO m ⇒                     GL.Window → m ()
disableCursorEnterEvents     ∷ MonadIO m ⇒                     GL.Window → m ()
disableScrollEvents          ∷ MonadIO m ⇒                     GL.Window → m ()
disableKeyEvents             ∷ MonadIO m ⇒                     GL.Window → m ()
disableCharEvents            ∷ MonadIO m ⇒                     GL.Window → m ()
enableErrorEvents            iq     = liftIO $ GL.setErrorCallback               $ Just $ errorCallback           iq
enableWindowPosEvents        iq win = liftIO $ GL.setWindowPosCallback       win $ Just $ windowPosCallback       iq
enableWindowSizeEvents       iq win = liftIO $ GL.setWindowSizeCallback      win $ Just $ windowSizeCallback      iq
enableWindowCloseEvents      iq win = liftIO $ GL.setWindowCloseCallback     win $ Just $ windowCloseCallback     iq
enableWindowRefreshEvents    iq win = liftIO $ GL.setWindowRefreshCallback   win $ Just $ windowRefreshCallback   iq
enableWindowFocusEvents      iq win = liftIO $ GL.setWindowFocusCallback     win $ Just $ windowFocusCallback     iq
enableWindowIconifyEvents    iq win = liftIO $ GL.setWindowIconifyCallback   win $ Just $ windowIconifyCallback   iq
enableFramebufferSizeEvents  iq win = liftIO $ GL.setFramebufferSizeCallback win $ Just $ framebufferSizeCallback iq
enableMouseButtonEvents      iq win = liftIO $ GL.setMouseButtonCallback     win $ Just $ mouseButtonCallback     iq
enableCursorPosEvents        iq win = liftIO $ GL.setCursorPosCallback       win $ Just $ cursorPosCallback       iq
enableCursorEnterEvents      iq win = liftIO $ GL.setCursorEnterCallback     win $ Just $ cursorEnterCallback     iq
enableScrollEvents           iq win = liftIO $ GL.setScrollCallback          win $ Just $ scrollCallback          iq
enableKeyEvents              iq win = liftIO $ GL.setKeyCallback             win $ Just $ keyCallback             iq
enableCharEvents             iq win = liftIO $ GL.setCharCallback            win $ Just $ charCallback            iq
disableErrorEvents                  = liftIO $ GL.setErrorCallback               $ Nothing
disableWindowPosEvents          win = liftIO $ GL.setWindowPosCallback       win $ Nothing
disableWindowSizeEvents         win = liftIO $ GL.setWindowSizeCallback      win $ Nothing
disableWindowCloseEvents        win = liftIO $ GL.setWindowCloseCallback     win $ Nothing
disableWindowRefreshEvents      win = liftIO $ GL.setWindowRefreshCallback   win $ Nothing
disableWindowFocusEvents        win = liftIO $ GL.setWindowFocusCallback     win $ Nothing
disableWindowIconifyEvents      win = liftIO $ GL.setWindowIconifyCallback   win $ Nothing
disableFramebufferSizeEvents    win = liftIO $ GL.setFramebufferSizeCallback win $ Nothing
disableMouseButtonEvents        win = liftIO $ GL.setMouseButtonCallback     win $ Nothing
disableCursorPosEvents          win = liftIO $ GL.setCursorPosCallback       win $ Nothing
disableCursorEnterEvents        win = liftIO $ GL.setCursorEnterCallback     win $ Nothing
disableScrollEvents             win = liftIO $ GL.setScrollCallback          win $ Nothing
disableKeyEvents                win = liftIO $ GL.setKeyCallback             win $ Nothing
disableCharEvents               win = liftIO $ GL.setCharCallback            win $ Nothing

makeInputQueue ∷ (MonadIO m) ⇒ GL.Window → m (STM.TQueue InputU)
makeInputQueue win = liftIO $ do
  iq ← STM.newTQueueIO
  enableErrorEvents       iq
  enableMouseButtonEvents iq win
  enableCursorPosEvents   iq win
  enableScrollEvents      iq win
  enableKeyEvents         iq win
  enableCharEvents        iq win
  pure iq

readInput ∷ (MonadIO m) ⇒ STM.TQueue InputU → m (Maybe InputU)
readInput queue = liftIO $ do
  GL.pollEvents
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
     ⇒ GL.Window
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

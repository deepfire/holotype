-- Window system (..hello WIndowSys..)
{-# LANGUAGE Arrows #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}
module HoloInput
  ( InputEvent(..)
  , makeEventChannel
  )
where

-- Basis
import           Prelude                           hiding ((.), id)
import           Prelude.Unicode

-- Generic
import qualified Control.Concurrent.Chan           as CH
import qualified Control.Concurrent.STM            as STM (TQueue, atomically, newTQueueIO, readTQueue, writeTQueue)
import           Control.Monad.IO.Class                   (MonadIO, liftIO)

-- Input
import "GLFW-b"  Graphics.UI.GLFW                  as GLFW

-- Local imports


makeEventChannel ∷ (MonadIO m) ⇒ GLFW.Window → m (CH.Chan InputEvent)
makeEventChannel win = liftIO $ do
   eventChan ← CH.newChan
   -- GLFW.setErrorCallback               $ Just $ errorCallback           eventChan
   -- GLFW.setWindowPosCallback       win $ Just $ windowPosCallback       eventChan
   -- GLFW.setWindowSizeCallback      win $ Just $ windowSizeCallback      eventChan
   -- GLFW.setWindowCloseCallback     win $ Just $ windowCloseCallback     eventChan
   -- GLFW.setWindowRefreshCallback   win $ Just $ windowRefreshCallback   eventChan
   -- GLFW.setWindowFocusCallback     win $ Just $ windowFocusCallback     eventChan
   -- GLFW.setWindowIconifyCallback   win $ Just $ windowIconifyCallback   eventChan
   -- GLFW.setFramebufferSizeCallback win $ Just $ framebufferSizeCallback eventChan
   -- GLFW.setMouseButtonCallback     win $ Just $ mouseButtonCallback     eventChan
   -- GLFW.setCursorPosCallback       win $ Just $ cursorPosCallback       eventChan
   -- GLFW.setCursorEnterCallback     win $ Just $ cursorEnterCallback     eventChan
   -- GLFW.setScrollCallback          win $ Just $ scrollCallback          eventChan
   -- GLFW.setKeyCallback             win $ Just $ keyCallback             eventChan
   GLFW.setCharCallback            win $ Just $ charCallback            eventChan
   pure eventChan


data InputEvent =
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


errorCallback           :: CH.Chan InputEvent -> GLFW.Error -> String                                                            -> IO ()
windowPosCallback       :: CH.Chan InputEvent -> GLFW.Window -> Int -> Int                                                       -> IO ()
windowSizeCallback      :: CH.Chan InputEvent -> GLFW.Window -> Int -> Int                                                       -> IO ()
windowCloseCallback     :: CH.Chan InputEvent -> GLFW.Window                                                                     -> IO ()
windowRefreshCallback   :: CH.Chan InputEvent -> GLFW.Window                                                                     -> IO ()
windowFocusCallback     :: CH.Chan InputEvent -> GLFW.Window -> GLFW.FocusState                                                  -> IO ()
windowIconifyCallback   :: CH.Chan InputEvent -> GLFW.Window -> GLFW.IconifyState                                                -> IO ()
framebufferSizeCallback :: CH.Chan InputEvent -> GLFW.Window -> Int -> Int                                                       -> IO ()
mouseButtonCallback     :: CH.Chan InputEvent -> GLFW.Window -> GLFW.MouseButton   -> GLFW.MouseButtonState -> GLFW.ModifierKeys -> IO ()
cursorPosCallback       :: CH.Chan InputEvent -> GLFW.Window -> Double -> Double                                                 -> IO ()
cursorEnterCallback     :: CH.Chan InputEvent -> GLFW.Window -> GLFW.CursorState                                                 -> IO ()
scrollCallback          :: CH.Chan InputEvent -> GLFW.Window -> Double -> Double                                                 -> IO ()
keyCallback             :: CH.Chan InputEvent -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys            -> IO ()
charCallback            :: CH.Chan InputEvent -> GLFW.Window -> Char                                                             -> IO ()

errorCallback           tc e s            = CH.writeChan tc $ EventError           e s
windowPosCallback       tc win x y        = CH.writeChan tc $ EventWindowPos       win x y
windowSizeCallback      tc win w h        = CH.writeChan tc $ EventWindowSize      win w h
windowCloseCallback     tc win            = CH.writeChan tc $ EventWindowClose     win
windowRefreshCallback   tc win            = CH.writeChan tc $ EventWindowRefresh   win
windowFocusCallback     tc win fa         = CH.writeChan tc $ EventWindowFocus     win fa
windowIconifyCallback   tc win ia         = CH.writeChan tc $ EventWindowIconify   win ia
framebufferSizeCallback tc win w h        = CH.writeChan tc $ EventFramebufferSize win w h
mouseButtonCallback     tc win mb mba mk  = CH.writeChan tc $ EventMouseButton     win mb mba mk
cursorPosCallback       tc win x y        = CH.writeChan tc $ EventCursorPos       win x y
cursorEnterCallback     tc win ca         = CH.writeChan tc $ EventCursorEnter     win ca
scrollCallback          tc win x y        = CH.writeChan tc $ EventScroll          win x y
keyCallback             tc win k sc ka mk = CH.writeChan tc $ EventKey             win k sc ka mk
charCallback            tc win c          = CH.writeChan tc $ EventChar            win c

-- processEvent :: InputEvent -> IO ()
-- processEvent ev =
--     case ev of
--       (EventError e s) -> do
--           printEvent "error" [show e, show s]
--           win <- asks envWindow
--           liftIO $ GLFW.setWindowShouldClose win True
--       (EventWindowPos _ x y) ->
--           printEvent "window pos" [show x, show y]
--       (EventWindowSize _ width height) ->
--           printEvent "window size" [show width, show height]
--       (EventWindowClose _) ->
--           printEvent "window close" []
--       (EventWindowRefresh _) ->
--           printEvent "window refresh" []
--       (EventWindowFocus _ fs) ->
--           printEvent "window focus" [show fs]
--       (EventWindowIconify _ is) ->
--           printEvent "window iconify" [show is]
--       (EventFramebufferSize _ width height) -> do
--           printEvent "framebuffer size" [show width, show height]
--           modify $ \s -> s { stateWindowWidth  = width, stateWindowHeight = height }
--           adjustWindow
--       (EventMouseButton _ mb mbs mk) -> do
--           printEvent "mouse button" [show mb, show mbs, showModifierKeys mk]
--           when (mb == GLFW.MouseButton'1) $ do
--               let pressed = mbs == GLFW.MouseButtonState'Pressed
--               modify $ \s -> s { stateMouseDown = pressed }
--               unless pressed $
--                 modify $ \s -> s { stateDragging = False }
--       (EventCursorPos _ x y) -> do
--           let x' = round x :: Int
--               y' = round y :: Int
--           printEvent "cursor pos" [show x', show y']
--           state <- get
--           when (stateMouseDown state && not (stateDragging state)) $
--             put $ state
--               { stateDragging        = True
--               , stateDragStartX      = x
--               , stateDragStartY      = y
--               , stateDragStartXAngle = stateXAngle state
--               , stateDragStartYAngle = stateYAngle state }
--       (EventCursorEnter _ cs) ->
--           printEvent "cursor enter" [show cs]
--       (EventScroll _ x y) -> do
--           let x' = round x :: Int
--               y' = round y :: Int
--           printEvent "scroll" [show x', show y']
--           env <- ask
--           modify $ \s -> s { stateZDist = let zDist' = stateZDist s + realToFrac (negate $ y / 2)
--                                           in curb (envZDistClosest env) (envZDistFarthest env) zDist' }
--           adjustWindow
--       (EventKey win k scancode ks mk) -> do
--           printEvent "key" [show k, show scancode, show ks, showModifierKeys mk]
--           when (ks == GLFW.KeyState'Pressed) $ do
--               -- Q, Esc: exit
--               when (k == GLFW.Key'Q || k == GLFW.Key'Escape) $
--                 liftIO $ GLFW.setWindowShouldClose win True
--               -- ?: print instructions
--               when (k == GLFW.Key'Slash && GLFW.modifierKeysShift mk) $
--                 liftIO printInstructions
--               -- i: print GLFW information
--               when (k == GLFW.Key'I) $
--                 liftIO $ printInformation win
--       (EventChar _ c) ->
--           printEvent "char" [show c]

-- processEvents ∷ CH.Chan InputEvent → IO ()
-- processEvents c = loop
--   where loop = do
--           processEvent =<< CH.readChan c
--           loop

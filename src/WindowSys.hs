{-# LANGUAGE PackageImports #-}
{-# LANGUAGE UnicodeSyntax #-}
module WindowSys where

import           Control.Monad.IO.Class                   (MonadIO, liftIO)
import "GLFW-b"  Graphics.UI.GLFW   as GLFW
import qualified Graphics.GL.Core33 as GL

makeGLWindow ∷ (MonadIO m) ⇒ String → m GLFW.Window
makeGLWindow title = liftIO $ do
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

keyIsPressed ∷ (MonadIO m) ⇒ GLFW.Window → GLFW.Key → m Bool
keyIsPressed win k = liftIO $ fmap (==KeyState'Pressed) $ GLFW.getKey win k

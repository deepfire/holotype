{-# LANGUAGE PackageImports #-}
{-# LANGUAGE UnicodeSyntax #-}
module WindowSys where

import "GLFW-b"  Graphics.UI.GLFW   as GLFW
import qualified Graphics.GL.Core33 as GL

makeGLWindow ∷ String → IO GLFW.Window
makeGLWindow title = do
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

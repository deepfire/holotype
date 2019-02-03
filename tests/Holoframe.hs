{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors -Wno-type-defaults #-}
module Main where

import           Control.Monad
import           Data.Maybe
import           Linear                            hiding (trace)
import           Prelude                           hiding (id, Word)

import           Flatland
import           Flex

import           HoloTypes

import           HoloPrelude                       hiding ((<>))
import qualified Holo
import           HoloPort
import qualified Holo.System                       as HOS

-- TEMPORARY
import qualified "GLFW-b" Graphics.UI.GLFW         as GLFW
import qualified Graphics.GL.Core33                as GL

main ∷ IO ()
main = do
  let simpleErrorPrinter ∷ GLFW.ErrorCallback
      simpleErrorPrinter e s = printf $ unwords [show e, show s]
      scrDi@(Di (V2 width height)) = di 800 600

  GLFW.setErrorCallback $ Just simpleErrorPrinter
  GLFW.init
  GLFW.defaultWindowHints
  mapM_ GLFW.windowHint
    [ GLFW.WindowHint'ContextVersionMajor 3
    , GLFW.WindowHint'ContextVersionMinor 3
    , GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
    , GLFW.WindowHint'OpenGLForwardCompat True ]
  win ← fromJust <$> (liftIO $ GLFW.createWindow width height "a simple holotype" Nothing Nothing)
  liftIO $ GLFW.makeContextCurrent (Just win)

  HOS.unbufferStdout

  let settingsV@Settings{..} = defaultSettings
  portV@Port{..}         ← portCreate win settingsV

  GL.glDisable GL.GL_DEPTH_TEST
  GL.glDisable GL.GL_STENCIL_TEST

  tokenV           ← newId "desc"
  let rectD         = di 200 200
      valD@(Holo.Rect _ _)
                    = Holo.Rect rectD (co 1 0 0 1)
      holoD         = Holo.leaf tokenV defStyle valD & size.~(Just∘fromPU <$> rectD)

  -- 0. size requests
  holoIOE          ← (flip $ Holo.queryHoloitem portV) [] holoD

  -- 1. final size allocation & positioning
  let tree          = layout (Size $ fromPU <$> scrDi) holoIOE
  
  -- 2. create visuals
  tree'            ← Holo.ensureHolotreeVisualBacking portV tree

  -- 3. draw into visuals
  Holo.renderHolotreeVisuals portV tree'

  -- 4. compose scene from visuals
  f                ← portNextFrame portV
  Holo.drawHolotreeVisuals f tree'

  -- 5. draw
  let peek x y = do
        p  ← liftIO $ pickFrameBuffer scrDi $ floor <$> po x y
        liftIO $ printf "%d:%d: %x\n" (floor x ∷ Int) (floor y ∷ Int) p

  portDrawFrame  portPort PipePickU
  peek 399 299
  peek 401 301

  portDrawFrame  portPort PipePickF
  peek 399 299
  peek 401 301

  GLFW.swapBuffers win

{-# LANGUAGE Arrows #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}

module Holotype where

-- Basis
import           Prelude                           hiding ((.), id)
import           Prelude.Unicode

-- Generic
import           Data.Maybe
import           Data.Vect
import           Control.Monad                            (when, unless)
import           Control.Monad.IO.Class                   (MonadIO, liftIO)

-- System
import qualified System.IO                         as Sys

-- Wires
import           Control.Wire
import           Control.Wire.Controller

-- Window system (..hello WIndowSys..)
import "GLFW-b"  Graphics.UI.GLFW                  as GLFW

-- LambdaCube
import qualified GameEngine.Data.Material          as Q3
import qualified GameEngine.Utils                  as Q3
import qualified LambdaCube.GL                     as GL

-- Local imports
import Flatland
import HoloCanvas
import WindowSys


holotype ∷ (MonadIO m) ⇒ Wire m a (Event ())
holotype = proc _ → do
  initial -< liftIO $ do
    Sys.hSetBuffering Sys.stdout Sys.NoBuffering
    win ← makeGLWindow "holotype"

    let cvObjStream = "canvasStream"
        cvTexture   = "canvasMtl"

    let schema = pipelineSchema cvObjStream cvTexture

    storage <- GL.allocStorage schema

    cv0 <- renderCanvasInitial storage cvObjStream cvTexture
           (CanvasRequest (sGrowS 2 $ sGrowS 5 $ sGrowS 2 $ sGrowS 16 $ sArea $ fromIntegral . ceiling <$> di2goldX 256)
             "yayyity"
             (coGray 0.8 1) (coOpaq 0.1 0.1 0.5) (coGray 1 1) (coGray 0.5 1) (coGray 0.1 0.5) terminusFontDesc)
    cv1 <- renderCanvasInitial storage cvObjStream cvTexture
           (CanvasRequest (sGrowS 2 $ sGrowS 5 $ sGrowS 2 $ sGrowS 16 $ sArea $ fromIntegral . ceiling <$> di2goldX 256)
             "indeed, lollage"
             (coGray 0.8 1) (coOpaq 0.5 0.1 0.1) (coGray 1 1) (coGray 0.5 1) (coGray 0.1 0.5) terminusFontDesc)

    let pipelineJSON = "Holotype.json"
    success ← compilePipeline pipelineJSON
    unless success $
      fail "FATAL: failed to compile the GPU pipeline."
    renderer' ← bindPipeline storage pipelineJSON
    unless (isJust renderer') $
      fail "FATAL: failed to bind the compiled GPU pipeline."
    let renderer = fromJust renderer'

    GL.setStorage renderer storage

    GLFW.pollEvents

    let slotU           = GL.uniformSetter storage
        overbrightBits  = 0
    GL.uniformFloat "identityLight" slotU $ 1 / (2 ^ overbrightBits) -- used by lc:mkColor

    (screenW, screenH) ← GLFW.getFramebufferSize win
    let toScreen = screenM screenW screenH
        cvpos    = Vec3 (-0.25) (-0.2) (0)
    GL.uniformM44F "viewProj" (GL.objectUniformSetter . cvGPU $ cv0) $
      Q3.mat4ToM44F $! toScreen .*. (fromProjective $! Data.Vect.translation $ cvpos &+ Vec3 0   0.3  0)
    GL.uniformM44F "viewProj" (GL.objectUniformSetter . cvGPU $ cv1) $
      Q3.mat4ToM44F $! toScreen .*. (fromProjective $! Data.Vect.translation $ cvpos &+ Vec3 0 (-0.3) 0)

    GL.setScreenSize storage (fromIntegral screenW) (fromIntegral screenH)

    GL.renderFrame renderer
    GLFW.swapBuffers win
    GLFW.pollEvents

    pure ()

  id -< never


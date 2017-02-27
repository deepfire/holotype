{-# LANGUAGE Arrows #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Holotype where

-- Basis
import           Prelude                           hiding ((.), id)
import           Prelude.Unicode

-- Generic
import           Data.Function                     hiding (id)
import           Data.Maybe
import           Data.MeasuredMonoid
import           Data.Vect
import           Control.Monad                            (when, unless)
import           Control.Monad.IO.Class                   (MonadIO, liftIO)
import           Text.Printf                              (printf)

-- Algebra
import           Linear

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
import HoloFont
import WindowSys


holotype ∷ (MonadIO m) ⇒ Wire m a (Event ())
holotype = proc _ → do
  initial -< liftIO $ do
    Sys.hSetBuffering Sys.stdout Sys.NoBuffering

    -- surface gen
    let dπ        = 96
        fontPrefs =
          [ FontReq "Aurulent Sans" "Regular" $ FSROutline (PUs 12)
          , FontReq "Terminus"      "Regular" $ FSRBitmap  (PUs 12) LT ]
        fm        = fmDefault
    (mFont ∷ Maybe (Font True PU), failures) ← chooseFont fm fontPrefs
    let font = mFont &
          fromMaybe (error $ printf "FATAL: no suitable font among requested: %s.  Failures: %s" (show fontPrefs) (show failures))

    textSettings ← makeTextSettings fm dπ font
    let text = Text textSettings $ coGray 1 1
        rect = RRect
               { rrCLBezel = coGray 1 1, rrCDBezel = coGray 0.1 0.5, rrCBorder = coGray 0.5 1, rrCBG = coOpaq 0.1 0.1 0.5
               , rrThBezel = 2, rrThBorder = 5, rrThPadding = 16 }

    cText ← fill text "lollestry" (Wi 256)
    let tS   = spaceRequest cText
        -- tPS  = sPin (po 0 0) tS

    cRect ← fill rect cText ()
    let rS   = spaceRequest cRect
        rPS  = sPin (po 0 0) rS
        tPS  = (⊥)
        -- tPS  = (case rPS of
        --            (Spc _ (Spc _ (Spc _  (Spc _ x)))) → x) ∷ Space True Double 1

    --
    win ← makeGLWindow "holotype"

    let objStreamN = "canvasStream" ∷ ObjArrayNameS
        textureN   = "canvasMtl"    ∷ UniformNameS

    let schema = pipelineSchema objStreamN textureN

    storage ← GL.allocStorage schema
    let stream = ObjectStream storage objStreamN textureN

    let pipelineJSON = "Holotype.json"
    success ← compilePipeline pipelineJSON
    unless success $
      fail "FATAL: failed to compile the GPU pipeline."
    renderer' ← bindPipeline storage pipelineJSON
    unless (isJust renderer') $
      fail "FATAL: failed to bind the compiled GPU pipeline."
    let renderer = fromJust renderer'
    GL.setStorage renderer storage

    canvas ← makeCanvas stream (sDim rPS)
    pText ← bind cText canvas tPS =<< (makeTextContext textSettings $ cGIC canvas)
    pRect ← bind cRect canvas rPS ()
    -- render pText
    render pRect

    -- surface upload
    canvasContentToGPU canvas

    GLFW.pollEvents

    let slotU           = GL.uniformSetter storage
        overbrightBits  = 0
    GL.uniformFloat "identityLight" slotU $ 1 / (2 ^ overbrightBits) -- used by lc:mkColor

    (screenW, screenH) ← GLFW.getFramebufferSize win
    canvasPosition canvas (Di $ V2 screenW screenH) (Po $ V2 (-0.25) (-0.2))

    GL.setScreenSize storage (fromIntegral screenW) (fromIntegral screenH)

    GL.renderFrame renderer
    GLFW.swapBuffers win
    GLFW.pollEvents

    pure ()

  id -< never


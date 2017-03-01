{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}
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
import qualified LambdaCube.GL                     as GL

-- Local imports
import Flatland
import HoloCanvas
import HoloCube
import HoloFont
import WindowSys


holotype ∷ (MonadIO m) ⇒ Wire m a (Event ())
holotype = proc _ → do
  initial -< liftIO $ do
    Sys.hSetBuffering Sys.stdout Sys.NoBuffering

    -- Once: make RC
    win ← makeGLWindow "holotype"
    let streamDesc = ("canvasStream", "canvasMtl") ∷ (ObjArrayNameS, UniformNameS)
    r' ← makeRenderer [streamDesc]
    let r@Renderer{..} = case r' of
                           Left failure → error $ printf "FATAL: failed to create a renderer: %s" failure
                           Right r → r
        stream = rStream r streamDesc & fromMaybe (error $ "Silly invariant #1 failure.")

    -- Once: define the visual style
    let dπ        = 96
        fontPrefs =
          [ FontReq "Aurulent Sans" "Regular" $ FSROutline (PUs 12)
          , FontReq "Terminus"      "Regular" $ FSRBitmap  (PUs 12) LT ]
        fm        = fmDefault
    (mFont ∷ Maybe (Font True PU), failures) ← chooseFont fm fontPrefs
    let font = mFont &
          fromMaybe (error $ printf "FATAL: no suitable font among requested: %s.  Failures: %s" (show fontPrefs) (show failures))
    textSettings ← makeTextSettings fm dπ font
    --
    let style = In (CanvasS textSettings)
                   (In (RRectS { rrCLBezel = coGray 1 1, rrCDBezel = coGray 0.1 0.5, rrCBorder = coGray 0.5 1, rrCBG = coOpaq 0.1 0.1 0.5
                               , rrThBezel = 2, rrThBorder = 5, rrThPadding = 16 })
                       (TextS textSettings $ coGray 1 1))
    -- *XXX
    -- - font defaulting   → make something a Monoid?
    -- - concern isolation → make it a DSL?

    -- Once: fill the visual structure with data
    let content = "Process intero killed\
                  Starting:\
                  stack ghci --with-ghc intero '--docker-run-args=--interactive=true --tty=false' --no-build --no-load --ghci-options -odir=/home/deepfire/src/mood/.stack-work/intero/intero17462TiM --ghci-options -hidir=/home/deepfire/src/mood/.stack-work/intero/intero17462TiM mood\
                  Intero 0.1.20 (GHC 8.0.1)\
                  Type :intro and press enter for an introduction of the standard commands."

    w ← assemble stream style ("lollestry", Wi 256)
    render w

    -- Frame: GL setup
    GLFW.pollEvents
    let slotU           = GL.uniformSetter rGLStorage
        overbrightBits  = 0
    GL.uniformFloat "identityLight" slotU $ 1 / (2 ^ overbrightBits) -- used by lc:mkColor
    (screenW, screenH) ← GLFW.getFramebufferSize win
    drawablePosition (drawableOf w) (Di $ V2 screenW screenH) (Po $ V2 (-0.25) (-0.2))
    GL.setScreenSize rGLStorage (fromIntegral screenW) (fromIntegral screenH)
    GL.renderFrame rGLRenderer
    GLFW.swapBuffers win
    GLFW.pollEvents

    pure ()

  id -< never

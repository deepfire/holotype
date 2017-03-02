{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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
import qualified System.Clock                      as Sys
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
import HoloSettings
import WindowSys


holotype ∷ (MonadIO m) ⇒ Wire m a (Event ())
holotype = proc _ → do
  initial -< liftIO $ do
    Sys.hSetBuffering Sys.stdout Sys.NoBuffering

    -- Once: make RC
    win ← makeGLWindow "holotype"

    (renderer, stream) ← makeSimpleRenderedStream win (("canvasStream", "canvasMtl") ∷ (ObjArrayNameS, UniformNameS))

    settings@Settings{..} ← defaultSettings

    let style = In (CanvasS @PU "default")
                   (In (RRectS { rrCLBezel = coGray 1 1, rrCDBezel = coGray 0.1 0.5, rrCBorder = coGray 0.5 1, rrCBG = coOpaq 0.1 0.1 0.5
                               , rrThBezel = 2, rrThBorder = 5, rrThPadding = 16 })
                       (TextS @PU "default" 7 $ coGray 1 1)) -- XXX/typing: Lines!

    let content = "Process intero killed\
                   Starting:\
                   stack ghci --with-ghc intero '--docker-run-args=--interactive=true --tty=false' --no-build --no-load --ghci-options -odir=/home/deepfire/src/mood/.stack-work/intero/intero17462TiM --ghci-options -hidir=/home/deepfire/src/mood/.stack-work/intero/intero17462TiM mood\
                   Intero 0.1.20 (GHC 8.0.1)\
                   Type :intro and press enter for an introduction of the standard commands."
        content2 = "Type :intro and press enter for an introduction of the standard commands."
    c ← assemble settings stream style (content, Wi 256)
    render c

    screenDim@(Di (V2 screenW screenH)) ← rendererSetupFrame renderer

    drawablePosition (drawableOf c) (Di $ V2 screenW screenH) (Po $ V2 (-0.25) (-0.2))

    rendererFinaliseFrame renderer

    GLFW.pollEvents

    pure ()

  id -< never

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
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Holotype where

-- Basis
import           Prelude                           hiding ((.), id)
import           Prelude.Unicode

-- Generic
import qualified Control.Concurrent.Chan           as CH
import qualified Control.Concurrent.STM            as STM (TQueue, atomically, newTQueueIO, readTQueue, writeTQueue)
import           Data.Foldable
import           Data.Function                     hiding ((.), id)
import           Data.Maybe
import           Data.MeasuredMonoid
import           Data.Profunctor
import qualified Data.Sequence as Seq
import qualified Data.Text                         as T
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
import HoloInput
import HoloSettings
import WindowSys


holotype ∷ (MonadIO m) ⇒ Wire m a (Event ())
holotype = proc _ → do
  ()            ← initial        -< liftIO $ Sys.hSetBuffering Sys.stdout Sys.NoBuffering

  winV          ← initial        -< makeGLWindow "holotype"
  (rendererV, streamV)
                ← initial        -< makeSimpleRenderedStream winV (("canvasStream", "canvasMtl") ∷ (ObjArrayNameS, UniformNameS))
  settingsV@Settings{..}
                ← initial        -< defaultSettings
  styleV        ←
    hold (In (CanvasS @PU "default")
             (In (RRectS { rrCLBezel = coGray 1 1, rrCDBezel = coGray 0.1 0.5, rrCBorder = coGray 0.5 1, rrCBG = coOpaq 0.1 0.1 0.5
                         , rrThBezel = 2, rrThBorder = 5, rrThPadding = 16 })
                 (TextS @PU "default" 7 $ coGray 1 1))) -< never
  contentV      ←
    hold (T.unlines
           [ "Press 'q' to quit.\n\n"
           , "Process intero killed"
           , "Starting:"
           , "stack ghci --with-ghc intero '--docker-run-args=--interactive=true --tty=false' --no-build --no-load --ghci-options -odir=/home/deepfire/src/mood/.stack-work/intero/intero17462TiM --ghci-options -hidir=/home/deepfire/src/mood/.stack-work/intero/intero17462TiM mood"
           , "Intero 0.1.20 (GHC 8.0.1)"
           , "Type :intro and press enter for an introduction of the standard commands." ]) -< never
  canvasV       ← initial        -< makeCanvas settingsV streamV styleV (contentV, Wi 256)
  ()            ← initial        -< renderCanvas canvasV

  -- The dynamic part.
  timeDeltaE    ← newTickWE      -< ()
  fpsV          ← hold 0 . (fmap recip <$> averageEWE 25) -< timeDeltaE

  frameE        ← newFrameVWE    -< rendererV
  eventChannelV ← initial        -< makeEventChannel winV
  inputE        ← inputEventsVWE -< eventChannelV
  worldE        ← id             -< translateEvent canvasV <$> inputE

  newWorld ← scan Void -< worldMergeEvent <$> worldE

  onEvent -< producePicture newWorld <$> frameE

  onEvent -< closeHolotype <$> (rendererV <$ filterE (\case Shutdown → True; _ → False) worldE)

worldMergeEvent ∷ WorldEvent → World → World
worldMergeEvent NonEvent = id
worldMergeEvent Shutdown = const Void
worldMergeEvent Move{..} =
  \w@Singleton{..}→ w { posn = posn ^+^ δ }
worldMergeEvent (Spawn c) =
  const $ Singleton c $ po (-0.25) (-0.2)

data WorldEvent where
  Move ∷
    { δ ∷ Po Double
    } → WorldEvent
  Spawn ∷
    { c ∷ CanvasW
    } → WorldEvent
  Shutdown ∷
    WorldEvent
  NonEvent ∷
    WorldEvent

translateEvent ∷ CanvasW → InputEvent → WorldEvent
translateEvent _ (EventChar _ 'w') = Move $ po   0    (0.05)
translateEvent _ (EventChar _ 's') = Move $ po   0   (-0.05)
translateEvent _ (EventChar _ 'a') = Move $ po (-0.05) 0
translateEvent _ (EventChar _ 'd') = Move $ po  (0.05) 0
translateEvent _ (EventChar _ 'q') = Shutdown
translateEvent c (EventChar _ 'c') = Spawn c
translateEvent _ _                 = NonEvent


newFrameVWE ∷ (MonadIO m) ⇒ Wire m Renderer (Event Frame)
newFrameVWE = proc renderer → do
  newEvent -< Just <$> rendererFinaliseToNewFrame renderer

producePicture ∷ (MonadIO m) ⇒ World → Frame → m ()
producePicture Void _ = pure ()
producePicture Singleton{..} frame = do
  placeCanvas canvas frame posn

inputEventsVWE ∷ (MonadIO m) ⇒ Wire m (CH.Chan InputEvent) (Event InputEvent)
inputEventsVWE = proc eventChan →
  newEvent -< liftIO $ do
    b ← CH.isEmptyChan eventChan
    if b then pure Nothing else Just <$> CH.readChan eventChan

data World where
  Void ∷ World
  Singleton ∷
    { canvas ∷ CanvasW
    , posn   ∷ Po Double
    } → World
instance Monoid World where
  mempty = Void
  mappend Void x = x
  mappend x Void = x
  mappend x _    = x -- XXX: a violation, indeed

closeHolotype ∷ (MonadIO m) ⇒ Renderer → m ()
closeHolotype Renderer{..} = liftIO $ do
  GL.disposeRenderer rGLRenderer
  GLFW.destroyWindow rWindow
  pure ()

newTickWE ∷ (MonadIO m) ⇒ Wire m a (Event Double)
newTickWE = proc _ -> do
    times ← newEvent -< Just <$> getT
    withM_ unfoldE getT -< fmap (\t t' → (secs (t - t'), t)) times

    where
    secs = (/ 1000000000) ∘ fromInteger ∘ Sys.toNanoSecs
    getT = liftIO (Sys.getTime Sys.Monotonic)

-- | Average of the given event's payload over the last given number of
-- occurrences.
averageEWE ∷ (Fractional a, Monad m) ⇒ Int → Wire m (Event a) (Event a)
averageEWE n = lmap (fmap go) (unfoldE Seq.empty)
    where
    go x xs' =
        let xs = Seq.take n (x Seq.<| xs')
        in (foldl' (+) 0 xs / fromIntegral (Seq.length xs),
            xs)


-- * Efficient input handling in GLFW
-- setCharCallback :: Window -> Maybe CharCallback -> IO ()
-- setMouseButtonCallback :: Window -> Maybe MouseButtonCallback -> IO ()
-- setScrollCallback :: Window -> Maybe ScrollCallback -> IO ()
-- setDropCallback :: Window -> Maybe DropCallback -> IO ()
-- getClipboardString :: Window -> IO (Maybe String)
-- GLFW.windowShouldClose win

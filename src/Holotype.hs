{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
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
import           Control.Category
import           Control.Lens

-- Generic
import qualified Control.Concurrent.Chan           as CH
import qualified Control.Concurrent.STM            as STM (TQueue, atomically, newTQueueIO, readTQueue, writeTQueue)
import           Data.Foldable
import           Data.Function                     hiding ((.), id)
import           Data.Functor
import           Data.Hashable
import qualified Data.HashMap.Lazy                 as HM
import qualified Data.HashSet                      as HS
import           Data.Maybe
import           Data.MeasuredMonoid
import           Data.Profunctor
import qualified Data.Sequence                     as Seq
import qualified Data.Text                         as T
import           Data.Vect
import           Control.Monad                            (join, when, unless)
import           Control.Monad.Fix                        (MonadFix)
import           Control.Monad.IO.Class                   (MonadIO, liftIO)
import           Text.Printf                              (printf)

-- Algebra
import           Linear

-- System
import qualified GHC.Stats                         as Sys
import qualified System.Clock                      as Sys
import qualified System.IO                         as Sys
import qualified System.Mem                        as Sys

-- Dirty stuff
import qualified Data.IORef                        as IO

-- Reflex
import           Reflex
import           Reflex.GLFW
import           Reflex.Random
import qualified Debug.Trace                       as D

-- Window system (..hello WIndowSys..)
import "GLFW-b"  Graphics.UI.GLFW                  as GLFW

-- LambdaCube
import qualified LambdaCube.GL                     as GL

-- Text editing
import           Control.Lens.Text                 as TL
import qualified Data.Text.Zipper                  as T

-- Local imports
import Flatland
import Holo
import HoloCanvas hiding (Text)
import qualified HoloCanvas                        as H
import HoloCube
import HoloFont
import HoloSettings

dasContent =
  (T.unlines
    [ "Press 'q' to quit.\n\n"
    , "Process intero killed"
    , "Starting:"
    , "stack ghci --with-ghc intero '--docker-run-args=--interactive=true --tty=false' --no-build --no-load --ghci-options -odir=/home/deepfire/src/mood/.stack-work/intero/intero17462TiM --ghci-options -hidir=/home/deepfire/src/mood/.stack-work/intero/intero17462TiM mood"
    , "Intero 0.1.20 (GHC 8.0.1)"
    , "Type :intro and press enter for an introduction of the standard commands." ])
dasStyle =
  (In (CanvasS @PU "default")
      (In (RRectS { rrCLBezel = coGray 1 1, rrCDBezel = coGray 0.1 0.5, rrCBorder = coGray 0.5 1, rrCBG = coOpaq 0.1 0.1 0.5
                  , rrThBezel = 2, rrThBorder = 5, rrThPadding = 16 })
          (TextS @PU "default" 7 $ coGray 1 1)))


data SystemStats where
  SystemStats ∷
    { statsTimeSecs ∷ Double
    , statsMem      ∷ Integer
    } → SystemStats
deriving instance Show SystemStats

timespecToSecs ∷ Sys.TimeSpec → Double
timespecToSecs = (/ 1000000000.0) ∘ fromIntegral ∘ Sys.toNanoSecs

systemStats ∷ (MonadIO m) ⇒ m SystemStats
systemStats = liftIO $ do
  -- Sys.performGC -- sloow with intero loaded..
  statsMem      ← (`div` 1024) ∘ fromIntegral ∘ Sys.currentBytesUsed <$> Sys.getGCStats
  statsTimeSecs ← timespecToSecs <$> Sys.getTime Sys.Monotonic
  pure SystemStats{..}

newFrame ∷ ReflexGLFWCtx t m ⇒ Event t Renderer → m (Event t Frame)
newFrame rendererFrameE = performEvent $ rendererFrameE <&>
  \r@Renderer{..} → do
    rendererDrawFrame r
    rendererSetupFrame r

holotype ∷ ReflexGLFW t m
holotype win setupE windowFrameE inputE = do
  liftIO $ Sys.hSetBuffering Sys.stdout Sys.NoBuffering
  (rendererV, streamV)
                ← makeSimpleRenderedStream win (("canvasStream", "canvasMtl") ∷ (ObjArrayNameS, UniformNameS))
  rendererDrawFrame rendererV
  settingsV@Settings{..} ← liftIO $ defaultSettings
  -- secs = (/ 1000000000) ∘ fromInteger ∘ Sys.toNanoSecs
  -- getT = liftIO (Sys.getTime Sys.Monotonic)
  -- fpsE          ← (T.pack ∘ show ∘ recip <$>) <$> averageEWE 25 timeDeltaE
  frameE           ← newFrame $ rendererV <$ windowFrameE
  frameTimeE       ← performEvent $ fmap (\_ → liftIO $ timespecToSecs <$> Sys.getTime Sys.Monotonic) frameE
  frameDeltaD      ← foldDyn (flip (-)) (0 ∷ Double) frameTimeE
  let worldE    = translateEvent <$> inputE
      spawnE    = ffilter (\case Spawn    → True; _ → False) worldE
      editE     = ffilter (\case Edit{..} → True; _ → False) worldE

  let driverE   = spawnE
      screenA   = Parea (di 1.5 1.5) (po (-0.85) (-0.5))
      widgetLim = Parea (di 0.2 0.2) (po 0 0)
  randomAreaE      ← foldRandomRs 0 (screenA, widgetLim) $ () <$ driverE
  holoAreaE        ← performEvent $ randomAreaE <&> (\area → liftIO $ do
                                                        s ← systemStats
                                                        pure ∘ (,area)  ∘ zft $ [T.pack $ printf "Press 'Esc' to quit.\n\n%s" $ show s])
  holosomAreaE     ← visual settingsV streamV dasStyle holoAreaE
  holosomAreaD     ← foldDyn (\x (n, xs)→ (n+1, (n,x):xs)) (0, []) $ holosomAreaE

  let drawReqE   = attachPromptlyDyn holosomAreaD frameE
  _                ← performEvent $ drawReqE <&>
                     \((_, cs), f@Frame{..}) →
                       forM_ cs $ \(n, (h@Holosome{..}, a@Parea{..} ∷ S Area True Double)) → do
                         placeCanvas holoVisual f _paNWp

  let topsomD    = ffor holosomAreaD (\case (_,[]) → Nothing
                                            (_,(_,h):_) → Just h)
      editReqE   = attachPromptlyDyn topsomD editE
  _                ← performEvent $ editReqE <&>
                     \case
                       (Nothing, _)→ pure ()
                       (Just (h,_), Edit{..}) →
                         update settingsV h weEdit

  hold False ((\case Shutdown → True; _ → False)
              <$> worldE)

instance Holo (T.TextZipper T.Text) where
  type Visual (T.TextZipper T.Text) = Canvas (RRect H.Text)
  visualise stts strea sty tzip = do
    vis ← assemble stts strea sty $ T.unlines $ T.getText tzip
    render vis
    pure vis
  updateVisual _ _ canvas tzip = do
    let H.Text{..} = (innerOf ∘ innerOf) canvas
    liftIO $ IO.writeIORef tTextRef $ T.unlines $ T.getText tzip
    render canvas

worldMergeEvent ∷ WorldEvent → World → World
worldMergeEvent NonEvent = id
worldMergeEvent Shutdown = const Void
worldMergeEvent Move{..} =
  \case
    w@Singleton{..}→ w { posn = posn ^+^ weΔ }
    _→Void
-- worldMergeEvent (Spawn c) =
--   const $ Singleton (po (-0.25) (-0.2)) $ zft
--   [ "press 'q' to quit"
--   , ""
--   , "this is our world now.. haha.."]
worldMergeEvent Edit{..} =
  \case
    w@Singleton{..}→ w { tz = weEdit tz }
    x→x

data WorldEvent where
  Move ∷
    { weΔ ∷ Po Double
    } → WorldEvent
  Edit ∷
    { weEdit ∷ T.TextZipper T.Text → T.TextZipper T.Text
    } → WorldEvent
  Spawn    ∷ WorldEvent
  Shutdown ∷ WorldEvent
  NonEvent ∷ WorldEvent

translateEvent ∷ Input → WorldEvent
translateEvent (EventChar _ c)                                            = Edit $ T.insertChar c
translateEvent (EventKey  _ GLFW.Key'Enter     _ GLFW.KeyState'Pressed _) = Edit $ T.breakLine
translateEvent (EventKey  _ GLFW.Key'Backspace _ GLFW.KeyState'Pressed _) = Edit $ T.deletePrevChar
translateEvent (EventKey  _ GLFW.Key'Delete    _ GLFW.KeyState'Pressed _) = Edit $ T.deleteChar
translateEvent (EventKey  _ GLFW.Key'Left      _ GLFW.KeyState'Pressed _) = Edit $ T.moveLeft
translateEvent (EventKey  _ GLFW.Key'Up        _ GLFW.KeyState'Pressed _) = Edit $ T.moveUp
translateEvent (EventKey  _ GLFW.Key'Right     _ GLFW.KeyState'Pressed _) = Edit $ T.moveRight
translateEvent (EventKey  _ GLFW.Key'Down      _ GLFW.KeyState'Pressed _) = Edit $ T.moveDown
translateEvent (EventKey  _ GLFW.Key'Home      _ GLFW.KeyState'Pressed _) = Edit $ T.gotoBOL
translateEvent (EventKey  _ GLFW.Key'End       _ GLFW.KeyState'Pressed _) = Edit $ T.gotoEOL
translateEvent (EventKey  _ GLFW.Key'Enter     _ GLFW.KeyState'Repeating _) = Edit $ T.breakLine
translateEvent (EventKey  _ GLFW.Key'Backspace _ GLFW.KeyState'Repeating _) = Edit $ T.deletePrevChar
translateEvent (EventKey  _ GLFW.Key'Delete    _ GLFW.KeyState'Repeating _) = Edit $ T.deleteChar
translateEvent (EventKey  _ GLFW.Key'Left      _ GLFW.KeyState'Repeating _) = Edit $ T.moveLeft
translateEvent (EventKey  _ GLFW.Key'Up        _ GLFW.KeyState'Repeating _) = Edit $ T.moveUp
translateEvent (EventKey  _ GLFW.Key'Right     _ GLFW.KeyState'Repeating _) = Edit $ T.moveRight
translateEvent (EventKey  _ GLFW.Key'Down      _ GLFW.KeyState'Repeating _) = Edit $ T.moveDown
translateEvent (EventKey  _ GLFW.Key'Home      _ GLFW.KeyState'Repeating _) = Edit $ T.gotoBOL
translateEvent (EventKey  _ GLFW.Key'End       _ GLFW.KeyState'Repeating _) = Edit $ T.gotoEOL
-- how to process key chords?
translateEvent (EventKey  _ GLFW.Key'Insert    _ GLFW.KeyState'Pressed _) = Spawn
translateEvent (EventKey  _ GLFW.Key'Escape    _ GLFW.KeyState'Pressed _) = Shutdown
translateEvent _                                                          = NonEvent


data Viewport a where
  Viewport ∷
    { vpCanvas ∷ Canvas a
    } → Viewport a

updateViewport ∷ (MonadIO m) ⇒ World → Viewport (Canvas (RRect H.Text)) → m ()
updateViewport Void = const $ pure ()
updateViewport Singleton{..} =
  \v@Viewport{..}→ wtextSetText (innerOf $ innerOf $ cInner vpCanvas)
                   (T.unlines $T.getText tz)


data World where
  Void ∷ World
  Singleton ∷
    { posn   ∷ Po Double
    , tz     ∷ T.TextZipper T.Text
    } → World
instance Monoid World where
  mempty = Void
  mappend Void x = x
  mappend x Void = x
  mappend x _    = x -- XXX: a violation, indeed

zft ∷ [T.Text] → T.TextZipper T.Text
zft = flip T.textZipper Nothing


-- Time & math
-- Average of the given event's payload over the last given number of
-- occurrences.
-- averageEWE ∷ (Fractional a, Monad m) ⇒ Int → Event t a → m (Event t a)
-- averageEWE n = lmap (fmap go) (unfoldE Seq.empty)
--     where
--     go x xs' =
--         let xs = Seq.take n (x Seq.<| xs')
--         in (foldl' (+) 0 xs / fromIntegral (Seq.length xs),
--             xs)


-- GLFW tips & tricks:
--
-- getClipboardString :: Window -> IO (Maybe String)
-- GLFW.windowShouldClose win

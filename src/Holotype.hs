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
import           Data.List
import           Data.Maybe
import           Data.MeasuredMonoid               hiding ((<>))
import           Data.Profunctor
import           Data.Semigroup
import           Data.String
import qualified Data.Sequence                     as Seq
import qualified Data.Text                         as T
import           Data.Vect
import           Control.Monad                            (join, when, unless)
import           Control.Monad.Fix                        (MonadFix)
import           Control.Monad.IO.Class                   (MonadIO, liftIO)
import           Text.Printf                              (printf)
import           Text.Show.Pretty                         (ppShow)

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
import qualified "GLFW-b" Graphics.UI.GLFW         as GLFW

-- LambdaCube
import qualified LambdaCube.GL                     as GL

-- Text editing
import           Control.Lens.Text                 as TL
import qualified Data.Text.Zipper                  as T

-- Local imports
import Flatland                                    hiding ((<>))
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

-- * Elsewhere
zipperText ∷ T.TextZipper T.Text → T.Text
zipperText = T.dropEnd 1 ∘ T.unlines ∘ T.getText

simpler ∷ Reflex t ⇒ Event t a → Event t ()
simpler = (() <$)

someFire ∷ Reflex t ⇒ Event t a → Event t b → Event t ()
someFire a b = simpler a <> simpler b


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

type Avg a = (Int, Int, [a])
avgStep ∷ Fractional a ⇒ a → (a, Avg a) → (a, Avg a)
avgStep x (_, (lim, cur, xs)) =
  let (ncur, nxs) = if cur < lim
                    then (cur + 1, x:xs)
                    else (lim,     x:init xs)
  in ((sum nxs) / fromIntegral ncur, (lim, ncur, nxs))

average ∷ (Fractional a, ReflexGLFWCtx t m) ⇒ Int → Event t a → m (Dynamic t a)
average n e = (fst <$>) <$> foldDyn avgStep (0, (n, 0, [])) e

holotype ∷ ReflexGLFW t m
holotype win setupE windowFrameE inputE = do
  liftIO $ Sys.hSetBuffering Sys.stdout Sys.NoBuffering
  (rendererV, streamV)
                ← makeSimpleRenderedStream win (("canvasStream", "canvasMtl") ∷ (ObjArrayNameS, UniformNameS))
  rendererDrawFrame rendererV
  settingsV@Settings{..} ← liftIO $ defaultSettings

  -- INPUT
  let worldE    = translateEvent <$> inputE
      spawnE    = ffilter (\case Spawn    → True; _ → False) worldE
      togglE    = ffilter (\case Pause    → True; _ → False) worldE
      editE     = ffilter (\case Edit{..} → True; _ → False) worldE
  frameE           ← newFrame $ rendererV <$ windowFrameE

  -- DATA
  enabledD         ← toggle True togglE
  let frameInE  = gate (current $ enabledD) frameE
      driverE   = simpler frameInE <> simpler spawnE
      screenA   = Parea (di 1.5 1.5) (po (-0.85) (-0.5))
      widgetLim = Parea (di 0.2 0.2) (po 0 0)
      text n    = [ printf "Object #%d:" n
                  , "  Esc:           quit"
                  , "  Pause:         toggle per-frame object stream"
                  , "  Editing keys:  edit"
                  , ""
                  , "Yay!"]
  holosomCountD    ← count driverE
  randomPreHoloE   ← foldRandomRs 0 ((screenA,   An 0.005)
                                    ,(widgetLim, An 0.01)) $ () <$ driverE
  preHoloE         ← performEvent $ attachPromptlyDyn holosomCountD randomPreHoloE
                     <&> (\(n, pre) → do; pure ∘ (,pre) ∘ zft $ T.pack <$> text n)
  holosomE         ← visual settingsV streamV dasStyle preHoloE
  holosomD         ← foldDyn (\x (n, xs)→ (n+1, (n,x):xs)) (0, []) $ holosomE

  -- UI
  frameMomentE     ← performEvent $ fmap (\_ → liftIO $ timespecToSecs <$> Sys.getTime Sys.Monotonic) frameE
  frameΔD          ← (fst <$>) <$> foldDyn (\y (_,x)->(y-x,y)) (0,0) frameMomentE
  avgFrameΔD       ← average 20 $ updated frameΔD
  let fpsD      = (floor ∘ recip) <$> avgFrameΔD
      fpsArea   = Parea (di 256 256) (po (-1) (1))
  let holoFPSDataE = attachPromptlyDyn (zipDyn fpsD holosomCountD) frameE <&>
                     \((fps ∷ Int, objects ∷ Int),_) →
                       zft [T.pack $ printf "%3d fps, %5d objects" fps objects]
  holosomFPSE      ← visual settingsV streamV dasStyle
                     (setupE <&> const (zft ["1000 fps, 10000 objects"], fpsArea))
  holosomFPSD      ← foldDyn (const ∘ Just) Nothing holosomFPSE

  -- SCENE COMPOSITION
  let allDrawablesD = zipDyn holosomFPSD holosomD
      drawReqE   = attachPromptlyDyn allDrawablesD frameE
  _                ← performEvent $ drawReqE <&>
                     \((mfps, (_, cs)), f@Frame{..}) → do
                       case mfps of
                         Nothing  → pure ()
                         Just (Holosome{..}, Parea{..}) → placeCanvas holoVisual f _paNWp
                       forM_ cs $ \(n, (h@Holosome{..}
                                       ,(Parea{..} ∷ S Area True Double
                                        ,angVel    ∷ An Double))) → do
                         placeCanvas holoVisual f _paNWp

  -- UI & DATA MUTATION
  let topsomD    = ffor holosomD (\case (_,[]) → Nothing
                                        (_,(_,h):_) → Just h)
      editReqE   = attachPromptlyDyn topsomD editE
  _                ← performEvent $ editReqE <&>
                     \case
                       (Nothing, _)→ pure ()
                       (Just (h,_), Edit{..}) →
                         update settingsV h weEdit
  let fpsUpdateE = attachPromptlyDyn holosomFPSD holoFPSDataE
  _                ← performEvent $ fpsUpdateE <&>
                     \case (Nothing, _)→ pure ()
                           (Just (h, _), fps)→
                             update settingsV h (const fps)

  hold False ((\case Shutdown → True; _ → False)
              <$> worldE)

instance Holo (T.TextZipper T.Text) where
  type Visual (T.TextZipper T.Text) = Canvas (RRect H.Text)
  visualise stts strea sty tzip = do
    vis ← assemble stts strea sty $ zipperText tzip
    render vis
    pure vis
  updateVisual _ _ vis tzip = do
    let H.Text{..} = (innerOf ∘ innerOf) vis
    liftIO $ IO.writeIORef tTextRef $ zipperText tzip
    render vis

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
  Spawn       ∷ WorldEvent
  Pause       ∷ WorldEvent
  Shutdown    ∷ WorldEvent
  NonEvent    ∷ WorldEvent

translateEvent ∷ InputU → WorldEvent
translateEvent (U (EventChar _ c))                                              = Edit $ T.insertChar c
translateEvent (U (EventKey  _ GLFW.Key'Enter     _ GLFW.KeyState'Pressed   _)) = Edit $ T.breakLine
translateEvent (U (EventKey  _ GLFW.Key'Backspace _ GLFW.KeyState'Pressed   _)) = Edit $ T.deletePrevChar
translateEvent (U (EventKey  _ GLFW.Key'Delete    _ GLFW.KeyState'Pressed   _)) = Edit $ T.deleteChar
translateEvent (U (EventKey  _ GLFW.Key'Left      _ GLFW.KeyState'Pressed   _)) = Edit $ T.moveLeft
translateEvent (U (EventKey  _ GLFW.Key'Up        _ GLFW.KeyState'Pressed   _)) = Edit $ T.moveUp
translateEvent (U (EventKey  _ GLFW.Key'Right     _ GLFW.KeyState'Pressed   _)) = Edit $ T.moveRight
translateEvent (U (EventKey  _ GLFW.Key'Down      _ GLFW.KeyState'Pressed   _)) = Edit $ T.moveDown
translateEvent (U (EventKey  _ GLFW.Key'Home      _ GLFW.KeyState'Pressed   _)) = Edit $ T.gotoBOL
translateEvent (U (EventKey  _ GLFW.Key'End       _ GLFW.KeyState'Pressed   _)) = Edit $ T.gotoEOL
translateEvent (U (EventKey  _ GLFW.Key'Enter     _ GLFW.KeyState'Repeating _)) = Edit $ T.breakLine
translateEvent (U (EventKey  _ GLFW.Key'Backspace _ GLFW.KeyState'Repeating _)) = Edit $ T.deletePrevChar
translateEvent (U (EventKey  _ GLFW.Key'Delete    _ GLFW.KeyState'Repeating _)) = Edit $ T.deleteChar
translateEvent (U (EventKey  _ GLFW.Key'Left      _ GLFW.KeyState'Repeating _)) = Edit $ T.moveLeft
translateEvent (U (EventKey  _ GLFW.Key'Up        _ GLFW.KeyState'Repeating _)) = Edit $ T.moveUp
translateEvent (U (EventKey  _ GLFW.Key'Right     _ GLFW.KeyState'Repeating _)) = Edit $ T.moveRight
translateEvent (U (EventKey  _ GLFW.Key'Down      _ GLFW.KeyState'Repeating _)) = Edit $ T.moveDown
translateEvent (U (EventKey  _ GLFW.Key'Home      _ GLFW.KeyState'Repeating _)) = Edit $ T.gotoBOL
translateEvent (U (EventKey  _ GLFW.Key'End       _ GLFW.KeyState'Repeating _)) = Edit $ T.gotoEOL
-- how to proce(U ss key chords?
translateEvent (U (EventKey  _ GLFW.Key'Pause     _ GLFW.KeyState'Pressed   _)) = Pause
translateEvent (U (EventKey  _ GLFW.Key'Insert    _ GLFW.KeyState'Pressed   _)) = Spawn
translateEvent (U (EventKey  _ GLFW.Key'Escape    _ GLFW.KeyState'Pressed   _)) = Shutdown
translateEvent _                                                                = NonEvent


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

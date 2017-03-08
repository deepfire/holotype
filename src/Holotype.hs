{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
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
import qualified System.Clock                      as Sys
import qualified System.IO                         as Sys

-- Dirty stuff
import qualified Data.IORef                        as IO

-- Reflex
import           Reflex
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
import HoloFlex
import HoloInput
import HoloSettings
import WindowSys

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


initialiser ∷ ReflexGLFW t m
initialiser streamV initiallyE frameE inputE = do
  stts             ← liftIO $ defaultSettings
  holotype streamV initiallyE frameE inputE stts

holotype ∷ ReflexGLFWCtx t m
            ⇒ ObjectStream
            → Event t ()
            → Event t Frame
            → Event t InputEvent
            → Settings PU
            → m (Behavior t Bool)
holotype streamV initiallyE frameE inputE settingsV@Settings{..} = do
  -- fpsE             ← (T.pack ∘ show ∘ recip <$>) <$> averageEWE 25 timeDeltaE
  frameTimeE       ← performEvent $ fmap (\_ → liftIO $ Sys.getTime Sys.Monotonic) frameE

  let worldE   = translateEvent <$> inputE
      spawnE   = ffilter (\case Spawn    → True; _ → False) worldE
      editE    = ffilter (\case Edit{..} → True; _ → False) worldE
  holoE            ← performEvent $ spawnE $> (liftIO $ do
                                                  t ← Sys.getTime Sys.Monotonic
                                                  pure $ zft $ [T.pack $ printf "Press 'Esc' to quit.\n\n%s" $ show t])
  holosomE         ← visual settingsV streamV dasStyle holoE
  holosomD         ← foldDyn (\x (n, xs)→ (n+1, (n,x):xs)) (0, []) $ holosomE

  let drawReqE   = attachPromptlyDyn holosomD frameE
  _                ← performEvent $ drawReqE <&>
                     \((_, cs), f@Frame{..}) →
                       forM_ cs $ \(n, h@Holosome{..}) → do
                         placeCanvas holoVisual f ((po (-0.5) (0.5)) ^+^ (po 0.1 (-0.1)) ^* n)

  let topsomD    = ffor holosomD (\case (_,[]) → Nothing
                                        (_,(_,h):_) → Just h)
      editReqE   = attachPromptlyDyn topsomD editE
  _                ← performEvent $ editReqE <&>
                     \case
                       (Nothing, _)→ pure ()
                       (Just h, Edit{..}) →
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

translateEvent ∷ InputEvent → WorldEvent
translateEvent (EventChar _ c)                        = Edit $ T.insertChar c
translateEvent (EventKey  _ GLFW.Key'Enter _ _ _)     = Edit $ T.breakLine
translateEvent (EventKey  _ GLFW.Key'Backspace _ _ _) = Edit $ T.deletePrevChar
translateEvent (EventKey  _ GLFW.Key'Delete _ _ _)    = Edit $ T.deleteChar
translateEvent (EventKey  _ GLFW.Key'Left _ _ _)      = Edit $ T.moveLeft
translateEvent (EventKey  _ GLFW.Key'Up _ _ _)        = Edit $ T.moveUp
translateEvent (EventKey  _ GLFW.Key'Right _ _ _)     = Edit $ T.moveRight
translateEvent (EventKey  _ GLFW.Key'Down _ _ _)      = Edit $ T.moveDown
-- how to process key chords?
translateEvent (EventKey  _ GLFW.Key'Insert _ _ _) = Spawn
translateEvent (EventKey  _ GLFW.Key'Escape _ _ _) = Shutdown
translateEvent _                                   = NonEvent


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
--
-- newTickWE ∷ (MonadIO m) ⇒ Wire m a (Event Double)
-- newTickWE = proc _ -> do
--     times ← newEvent -< Just <$> getT
--     withM_ unfoldE getT -< fmap (\t t' → (secs (t - t'), t)) times

--     where
--     secs = (/ 1000000000) ∘ fromInteger ∘ Sys.toNanoSecs
--     getT = liftIO (Sys.getTime Sys.Monotonic)

-- | Average of the given event's payload over the last given number of
-- occurrences.
-- averageEWE ∷ (Fractional a, Monad m) ⇒ Int → Event t a → m (Event t a)
-- averageEWE n = lmap (fmap go) (unfoldE Seq.empty)
--     where
--     go x xs' =
--         let xs = Seq.take n (x Seq.<| xs')
--         in (foldl' (+) 0 xs / fromIntegral (Seq.length xs),
--             xs)


-- * Efficient input handling in GLFW
-- setCharCallback :: Window -> Maybe CharCallback -> IO ()
-- setMouseButtonCallback :: Window -> Maybe MouseButtonCallback -> IO ()
-- setScrollCallback :: Window -> Maybe ScrollCallback -> IO ()
-- setDropCallback :: Window -> Maybe DropCallback -> IO ()
-- getClipboardString :: Window -> IO (Maybe String)
-- GLFW.windowShouldClose win

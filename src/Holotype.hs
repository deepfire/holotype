{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE CPP #-}
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
  initialWorldV    ← sample $ constant (Singleton (po 0 0) $ zft ["Press 'q' to quit.", "", "..."])

  -- timeDeltaE       ← newTickWE
  -- fpsE             ← (T.pack ∘ show ∘ recip <$>) <$> averageEWE 25 timeDeltaE
  frameTimeE       ← performEvent $ fmap (\_ → liftIO $ Sys.getTime Sys.Monotonic) frameE
  initialCanvasV   ← liftIO $ assemble settingsV streamV dasStyle "Press 'q' to quit."
  -- canvasE          ← visual settingsV streamV dasStyle $ T.pack ∘ show <$> frameTimeE

  -- CRISIS: we need a /persistent/ canvas, sorry
  -- Hmm, let's see how horribly it'll break with a non-persistent one, first?
  -- ..it does break horribly, see https://github.com/lambdacube3d/lambdacube-gl/issues/9
  let worldE   = translateEvent <$> inputE
      spawnE   = ffilter (\case Spawn → True; _ → False) worldE
  canvassesE       ← performEvent $ spawnE $> (liftIO $ do
                                                  t ← Sys.getTime Sys.Monotonic
                                                  c ← assemble settingsV streamV dasStyle
                                                    (T.pack $ printf "Press 'Esc' to quit.\n\n%s" $ show t)
                                                  render c
                                                  pure c)
  canvassesD       ← foldDyn (\x (n, xs)→ (n+1, (n,x):xs)) (0, []) $ canvassesE
  let drawReqE = attachPromptlyDyn canvassesD frameE
  _                ← performEvent $ drawReqE <&>
                     \((_, cs), f@Frame{..})→
                       forM_ cs $ \(n, c) → do
                         placeCanvas c f ((po (-0.5) (0.5)) ^+^ (po 0.1 (-0.1)) ^* n)

  -- newWorldE        ← scanE Void     -< worldMergeEvent <$> worldEventE
  -- ()               ← initial        -< renderCanvas canvasV

  -- onEvent -< producePicture initialCanvasV newWorldE <$> frameE

  -- onEvent -< closeHolotype <$> (rendererV <$ filterE (\case Shutdown → True; _ → False) worldEventE)

  hold False ((\case (EventKey  _ GLFW.Key'Escape _ _ _) → True
                     _ → False)
              <$> inputE)

instance Holo T.Text where
  type Visual T.Text = Canvas (RRect H.Text)
  -- visualise x = do
  --   assemble

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
-- how to process key chords?
translateEvent (EventKey  _ GLFW.Key'Insert _ _ _) = Spawn
translateEvent (EventKey  _ GLFW.Key'Escape _ _ _) = Shutdown
translateEvent _                                   = NonEvent


-- newFrameVWE ∷ (MonadIO m) ⇒ Wire m Renderer (Event Frame)
-- newFrameVWE = proc renderer → do
--   newEvent -< Just <$> rendererFinaliseToNewFrame renderer

-- producePicture ∷ (MonadIO m, Widget a) ⇒ Canvas a → World → Frame → m ()
-- producePicture canvas Void _ = pure ()
-- producePicture canvas Singleton{..} frame = do
--   placeCanvas canvas frame posn


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


-- 09:05 <deepfire> ryantrinkle: trying to figure out how to implement
--                  PerformEvent instances, but can't find a single example..
-- 09:07 <ryantrinkle> deepfire: Reflex.PerformEvent.Base ?
-- 09:08 <ryantrinkle> you should be able to reuse that, most likely
-- 09:08 <ryantrinkle> rather than needing to implement it yourself
-- 09:08 <ryantrinkle> if you want to implement a passthrough instance, take a
--                     look at, e.g., Reflex.PerformEvent.Class
-- 09:08 <ryantrinkle> which has an instance for ReaderT
-- 09:08 <deepfire> ryantrinkle: thank you, looking!
-- 09:08 <ryantrinkle> np :
-- 09:10 <deepfire> turns out Reflex.PerformEvent.Base was what I was staring at :
--                  -
-- 09:10 <ryantrinkle> haha nice
-- 09:10 <ryantrinkle> really, it's just a weird specialization of Requester
-- 09:10 <ryantrinkle> Requester is where the real action is :
-- 09:14 <deepfire> hmm, I guess I'm conceptually stumped.. I'm not sure what type
--                  I should have PerformEvent implemented for
-- 09:15 <deepfire> I have added a PerformEvent t m => context to the application
--                  type, but there seems to be no instance for PerformEvent
--                  (SpiderTimeline Global) (SpiderHostFrame Global)
-- 09:16 <deepfire> ..and that sounds like a fundamental enough type that a
--                  pass-through instance for it likely won't go?
-- 09:18 <ryantrinkle> you just need to call hostPerformEventT
-- 09:18 <ryantrinkle> it'll wrap SpiderHostFrame in a PerformEventT
-- 09:28 <deepfire> ryantrinkle: so what hostPerformEventT does is to extract the
--                  requested IO execution events from a single frame run, so that
--                  the host can execute them separately?
-- 09:29 <deepfire> ryantrinkle: so what hostPerformEventT does is extracting the
--                  requested IO execution events from a single frame run, so that
--                  the host can execute them separately?
-- 09:29 <deepfire> ..into a FireCommand
-- 09:30 <ryantrinkle> deepfire: well, it also runs the IO actions
-- 09:30 <ryantrinkle> but yes, that's right
-- 09:31 <deepfire> wait, it does run them itself?
-- 09:31 <ryantrinkle> (that's on the line that binds 'responses')
-- 09:31 <deepfire> does my host implementation need to do anything with the
--                  returned FireCommand?
-- 09:32 <ryantrinkle> yes: it should use that, instead of fireEvents, to fire
--                     events
-- 09:32 <ryantrinkle> otherwise, the events won't get performed
-- 09:33 <deepfire> I see, it it so that it can "thread" the IO action events with
--                  the main events, right?
-- 09:34 <deepfire> well, "main", "usual" etc -- the idea is to allow it to handle
--                  interdependencies
-- 09:34 <deepfire> ..interdependencies between the usual events and IO action
--                  events, that is
-- 09:36 <deepfire> hmm..
-- 09:36 <deepfire> now the question is about monadic ordering
-- 09:38 <deepfire> the develop/examples/host.hs example does the event firing
--                  /before/ it runs the FRP network with runHostFrame
-- 09:39 <ryantrinkle> runHostFrame doesn't do much
-- 09:39 <ryantrinkle> it's just a way of doing certain things, such as running
--                     'hold', while events aren't propagating
-- 09:40 <ryantrinkle> fireEvents* is what actually runs the FRP network
-- 09:40 <deepfire> ..but still, it wants the event variables to pass to the FRP
--                  network..
-- 09:40 <ryantrinkle> oh, you mean in hostPerformEventT?
-- 09:41 <ryantrinkle> yeah, that *particular* runHostFrame is producing values
--                     that will be fired in events
-- 09:41 <ryantrinkle> bu tthe actual firing happens in the fireEventsAndRead
--                     above that
-- 09:58 <ryantrinkle> deepfire: i'm signing off; i'll be around tomorrow if you
--                     have any questions!
-- 09:59 <deepfire> ryantrinkle: one last question -- runHostFrame now asks for a
--                  MonadReflexHost instance
-- 10:01 <deepfire> ..in  hostPerformEventT $ runHostFrame $ sample b -- where b =
--                  runHostFrame $ myGuest stream e -- from examples/host.hs

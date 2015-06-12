{-# LANGUAGE Arrows #-}
-- {-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UnicodeSyntax #-}

import Prelude hiding ((.), id, null, filter)

import Control.Concurrent (threadDelay)
import Control.Exception
--import Control.Monad (liftM)
import Control.Wire (mkPure, mkId, stepWire, NominalDiffTime, mkSFN)
import Control.Wire.Unsafe.Event (onEventM, Event(..))

import Data.Maybe (fromMaybe)
import Data.Set (Set, empty, elems, insert, delete, null, filter, intersection, fromList)

import FRP.Netwire hiding (empty)

import Linear hiding (trace)
import Linear.Affine

import Text.Printf (printf)

import qualified SDL as SDL

import Debug.Trace (trace)
import System.Exit


import Types
import Utils
screenWidth, screenHeight   ∷ _
(screenWidth, screenHeight) = (640, 480)


main :: IO ()
main = do
  SDL.initialize [SDL.InitEverything]
  win  ← SDL.createWindow
           "Mood"
           SDL.defaultWindow {SDL.windowSize = V2 screenWidth screenHeight }
  rend ← catchSDLFatally $
         SDL.createRenderer
            win
            (-1)
            (SDL.RendererConfig
                    { SDL.rendererAccelerated = False
                    , SDL.rendererSoftware = False
                    , SDL.rendererTargetTexture = False
                    , SDL.rendererPresentVSync = False
                    })
  SDL.showWindow win

  catchSDLFatally $ (simulator win rend (initialScene, Inputs empty) clockSession_ simulation)
  -- catchSDLFatally $ (stepper win rend ("<init>", Inputs empty) clockSession_ test)

  SDL.destroyRenderer rend
  SDL.destroyWindow win
  SDL.quit


newtype Inputs   = Inputs (Set SDL.Scancode)
newtype World    = World (Inputs, Scene)
instance Sim World Inputs Scene where
    inputsOf     (World (x, _)) = x
    sceneOf      (World (_, x)) = x
    nextWorld     i s           = World (i, s)
    trackKeyDown (Inputs s) k   = Inputs $ insert k s
    trackKeyUp   (Inputs s) k   = Inputs $ delete k s
    someKeyDown  (Inputs s)     = not $ null s
    keyDown       k (Inputs s)  = not $ null $ filter ((== k)) s


class Renderable a where
    dim    ∷ a → V2 Double
    render ∷ SDL.Renderer → a → V2 Int → IO ()

drawBB ∷ ∀ a . Renderable a ⇒ SDL.Renderer → a → V2 Int → IO ()
drawBB rend rra pos =
  SDL.renderDrawRect rend $ SDL.Rectangle (P $ fmap fromIntegral pos) $ fmap floor $ dim rra

newtype ReBox = ReBox String
instance Renderable ReBox where
    dim    _ = V2 20.0 20.0
    render   = drawBB

data Scene where
    Arena ∷ Renderable a ⇒ Double → [a] → Scene

update_scene ∷ Scene → Double → Scene
update_scene scene x = case scene of
                         Arena _ xs → Arena x xs

initialScene ∷ Scene
initialScene = Arena 0 [ReBox "lol", ReBox "yay"]

render_scene ∷ SDL.Renderer → Scene → IO ()
render_scene rend scene = do

  SDL.setRenderDrawColor rend (V4 0 0 0 255)
  SDL.renderClear rend

  SDL.setRenderDrawColor rend (V4 255 255 255 255)
  case scene of
    Arena x rras → mapM_ (\(i, rra) → drawBB rend rra $ V2 (floor x + 50) $ 30 + i * 30) (zip [1..] rras) -- (\case x → drawBB rend x) We need the case to discharge the GADT

  SDL.renderPresent rend


simulator ∷ SDL.Window → SDL.Renderer → (Scene, Inputs) → Session IO SimTime → SimWire (Scene, Inputs) (Bool, Scene) → IO ()
simulator win rend (preScene, preKeysDown) sesn wire = do
  keysDown <- parseEvents preKeysDown
  (nextScene, nextSesn, nextWire) ← do
    (st , sesn') ← stepSession sesn
    (ret, wire') ← stepWire wire st $ Right (preScene, keysDown)
    case ret of
      Left  _              → error $ printf "stepWire: inhibited (got a Left)"
      Right (False, _)     → terminate True "user requested end of simulation"
      Right (True, scene') → return (scene', sesn', wire')
  render_scene rend nextScene
  simulator win rend (nextScene, keysDown) nextSesn nextWire

-- switch :: (Monad m, Monoid s) => Wire s e m a (b, Event (Wire s e m a b)) -> Wire s e m a b
simulation :: SimWire (Scene, Inputs) (Bool, Scene)
simulation = arena_sim . when (not . keyDown SDL.ScancodeQ . snd)
             --> pure (False, undefined)

-- input_events :: (MonadFix m, HasTime t s, Monoid e) => Wire s e m Inputs Double
-- input_events = (-20) . edge (keyDown SDL.ScancodeLeft) <&
--                  20  . edge (keyDown SDL.ScancodeRight)
-- spin :: (HasTime t s, Monad m) => Wire s e m a Double
-- spin = integral 0 . spinSpeed
--   where
--     spinSpeed =  5 . trigger SDL.ScancodeF -->
--                 -5 . trigger SDL.ScancodeF -->
--                 spinSpeed

arena_sim ∷ SimWire (Scene, Inputs) (Bool, Scene)
arena_sim =
    proc (scene, inputs) → do
      accel ← arrows_to_double -< inputs
      rec (x, coll) ← velocity_to_coords -< v
          v ← velocity -< (accel, coll)
      returnA -< (True, update_scene scene x)

velocity :: SimWire (Double, Bool) Double
velocity = integralWith bounce 0
    where bounce collisions v | collisions = -v
                              | otherwise  = v

velocity_to_coords :: SimWire Double (Double, Bool)
velocity_to_coords = integralWith' clamp 0
           where
             clamp p | p < 0 || p > 150 = (max 1 (min 149 p), True)
                     | otherwise        = (p, False)

arrows_to_double :: SimWire Inputs Double
arrows_to_double = (   (pure (-20)) . when (keyDown SDL.ScancodeLeft)
                   <|> (pure   20)  . when (keyDown SDL.ScancodeRight)
                   <|> (pure    0))
---
--- The following are equivalent, as per http://hub.darcs.net/ertes/netwire/browse/README.md
---
-- integral 5 . time
--
-- proc _ -> do
--   t <- time -< ()
--   integral 5 -< t

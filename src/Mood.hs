{-# LANGUAGE Arrows #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
-- {-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import Data.Set (empty, elems, insert, delete, null, filter, intersection, fromList)
import qualified Data.Set as DS

import FRP.Netwire hiding (empty)

import Linear hiding (trace)
import Linear.Affine

import Text.Printf (printf)

import qualified SDL as SDL

import Debug.Trace (trace)
import System.Exit


-- Local imports
import Types
import Utils


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

  catchSDLFatally $ (simulator win rend initialWireInput clockSession_ simulation)
  -- catchSDLFatally $ (stepper win rend ("<init>", Inputs empty) clockSession_ test)

  SDL.destroyRenderer rend
  SDL.destroyWindow win
  SDL.quit


-- GFX primitives
screenWidth, screenHeight   ∷ _
(screenWidth, screenHeight) = (640, 480)


newtype Inputs = Inputs (DS.Set SDL.Scancode)
data World where
    World ∷ {
              wInputs   ∷ Inputs
            , wControls ∷ Controls
            , wTotality ∷ Totality
            , wEngi     ∷ Engine
            } → World

construct_initial_world ∷ Controls → World
construct_initial_world (ctls@Controls{..}) =
    let
    in World (Inputs empty) ctls (Totality []) $ erect_engi $ elect_engi cEngiPref (Just Carousel)

initialWireInput ∷ (Inputs, World)
initialWireInput = (Inputs empty, construct_initial_world initial_controls)

instance Sim World Inputs where
    inputsOf     (World x _ _ _)   = x
    trackKeyDown (Inputs s) k      = Inputs $ insert k s
    trackKeyUp   (Inputs s) k      = Inputs $ delete k s
    someKeyDown  (Inputs s)        = not $ null s
    keyDown       k (Inputs s)     = not $ null $ filter ((== k)) s

render_world ∷ SDL.Renderer → World → IO ()
render_world rend (World _ Controls{..} _ _) = do
  SDL.setRenderDrawColor rend (V4 0 0 0 255)
  SDL.renderClear rend
  SDL.setRenderDrawColor rend (V4 255 255 255 255)
  
  SDL.renderPresent rend


simulator ∷ SDL.Window → SDL.Renderer → (Inputs, World) → Session IO SimTime → SimWire (Inputs, World) (Bool, World) → IO ()
simulator win rend (oldKeysDown, prevWorld) sesn wire = do
  nowKeysDown <- parseEvents oldKeysDown
  (nextWorld, nextSesn, nextWire) ← do
    (st , sesn') ← stepSession sesn
    (ret, wire') ← stepWire wire st $ Right (nowKeysDown, prevWorld)
    case ret of
      Left  _              → error $ printf "stepWire: inhibited (got a Left)"
      Right (False, _)     → terminate True "user requested end of simulation"
      Right (True, world)  → return (world, sesn', wire')
  render_world rend nextWorld
  simulator win rend (nowKeysDown, nextWorld) nextSesn nextWire

-- switch :: (Monad m, Monoid s) => Wire s e m a (b, Event (Wire s e m a b)) -> Wire s e m a b
simulation :: SimWire (Inputs, World) (Bool, World)
simulation = undefined . when (not . keyDown SDL.ScancodeQ . fst)
             --> pure (False, undefined)

-- arena_sim ∷ SimWire (Scene, Inputs) (Bool, Scene)
-- arena_sim =
--     proc (scene@(Arena (Posn (V2 _ y)) _ _), inputs) → do
--       accel ← arrows_to_double -< inputs
--       rec (x, coll) ← velocity_to_coords -< v
--           v ← velocity -< (accel, coll)
--       returnA -< (True, update_scene scene (Posn (V2 x y)))

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

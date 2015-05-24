{-# LANGUAGE Arrows #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UnicodeSyntax #-}

import Prelude hiding ((.), id, null, filter)
import Control.Monad (void)
import Control.Monad.Fix (MonadFix)
import Control.Wire hiding (empty)
import Control.Wire.Session()
import Text.Printf (printf)
import FRP.Netwire hiding (empty)
import Data.Monoid (Monoid)
import Data.Set (Set, empty, insert, delete, null, filter)
import Linear
import Linear.Affine
import qualified SDL as SDL

import Control.Exception
import Foreign.C.Types (CInt)
import System.Exit
import System.IO.Error
 
screenWidth, screenHeight   ∷ _
(screenWidth, screenHeight) = (640, 480)

terminate ∷ Bool → String → IO a
terminate successp reason = do
  printf "%s: %s\n" (if successp then "Exiting" else "FATAL" ∷ String) reason
  SDL.quit
  exitWith (if successp then ExitSuccess else ExitFailure 1)

catchSDLFatally ∷ IO a -> IO a
catchSDLFatally = (flip catch) (\e -> terminate True $ show (e :: SDL.SDLException))

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
  
  catchSDLFatally $ simulator win rend (Inputs empty) clockSession_ simulation

  SDL.destroyRenderer rend
  SDL.destroyWindow win
  SDL.quit

integralWith' ::
    (Fractional a, HasTime t s)
    => (a -> (a, o))  -- Function for potentially limiting the integration
                      -- and producing a secondary output.
    -> a              -- Integration constant (aka start value).
    -> Wire s e m a (a, o)
integralWith' correct = loop'
  where
    loop' x' =
        mkPure $ \ds dx ->
            let dt = realToFrac (dtime ds)
                (x,b)  = correct (x' + dt*dx)
            in x' `seq` (Right (x', b), loop' x)


newtype Inputs = Inputs (Set SDL.Keysym)

parseEvents :: Inputs -> IO Inputs
parseEvents (Inputs keysDown) = do
  event ← SDL.pollEvent
  case event of
    Nothing
      → return $ Inputs keysDown
    Just e | (SDL.KeyboardEvent{..}) ← SDL.eventPayload e
      → parseEvents $ Inputs $ if keyboardEventKeyMotion == SDL.KeyDown
                               then (insert keyboardEventKeysym keysDown)
                               else (delete keyboardEventKeysym keysDown)
    _
      → parseEvents $ Inputs keysDown

keyDown :: SDL.Scancode -> Inputs -> Bool
keyDown k (Inputs ks) = not $ null $ filter ((== k) . SDL.keysymScancode) ks


newtype Scene = Scene (Bool, Double)

render_scene ∷ SDL.Renderer → Scene → IO ()
render_scene rend scene = do
  let Scene (exit, x) = scene
  
  SDL.setRenderDrawColor rend (V4 0 0 0 255)
  SDL.renderClear rend

  SDL.setRenderDrawColor rend (V4 255 255 255 255)
  SDL.renderDrawRect rend
         (SDL.Rectangle (P $ V2 (screenWidth `div` 6 + floor x) (screenHeight `div` 6))
                        (V2 (screenWidth * 2 `div` 3) (screenHeight * 2 `div` 3)))

  SDL.renderPresent rend

simulator ∷ SDL.Window → SDL.Renderer → Inputs → Session IO step → Wire step block IO Inputs Scene → IO ()
simulator win rend preKeysDown sesn wire = do
  keysDown <- parseEvents preKeysDown
  (x, nextSesn, nextWire) ← do
    (st , sesn') ← stepSession sesn
    (wt', wire') ← stepWire wire st $ Right keysDown
    case wt' of
      Left  _   → error $ printf "stepWire: got Left"
      Right (Scene (False, _))
            → terminate True "user requested end of simulation"
      Right x'  → return (x', sesn', wire')
  render_scene rend x
  simulator win rend keysDown nextSesn nextWire

simulation :: (HasTime t s, MonadFix m) => Wire s String m Inputs Scene
simulation = proc keysDown -> do
  accel ← acceleration -< keysDown
  rec (position, collisions) ← position -< velocity
      velocity ← velocity -< (accel, collisions)
  returnA -< position

acceleration :: (HasTime t s, MonadFix m, Monoid e) => Wire s e m Inputs Scene
acceleration =     pure (Scene (True,  -20)) . when (keyDown SDL.ScancodeLeft)
               <|> pure (Scene (True,   20)) . when (keyDown SDL.ScancodeRight)
               <|> pure (Scene (False,  20)) . when (keyDown SDL.ScancodeQ)
               <|> pure (Scene (True,    0))

---
--- The following are equivalent, as per http://hub.darcs.net/ertes/netwire/browse/README.md
---
-- integral 5 . time
--
-- proc _ -> do
--   t <- time -< ()
--   integral 5 -< t
                        
velocity :: (HasTime t s, Monad m) ⇒ Wire s e m (Scene, Bool) Scene
velocity = proc (Scene (endp, x), b) → do
             x' ← integralWith bounce 0 -< (x, b)
             returnA -< Scene (endp, x')
    where bounce collisions v | collisions = -v
                              | otherwise  = v

position :: (MonadFix m, HasTime t s, Monoid e) => Wire s e m Scene (Scene, Bool)
position = proc (Scene (endp, x)) → do
             (x', b) ← integralWith' clamp 0 -< x
             returnA -< (Scene (endp, x'), b)
           where
             clamp p | p < 0 || p > 150 = (max 1 (min 149 p), True)    
                     | otherwise        = (p, False)


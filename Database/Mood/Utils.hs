{-# LANGUAGE Arrows #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}

module Utils
    (
     -- * Arrow

     -- * Netwire
      redge, fedge
    -- , stepper
    , justFirst, justSecond
    , integralWith'

    -- * SDL
    , parseEvents
    , terminate, catchSDLFatally
    , someKeyDown, keyDown
    , produceOnKey, inhibitOnKey
    ) where

import Prelude hiding ((.), id, null, filter)

import Control.Concurrent (threadDelay)
import Control.Exception (catch)
--import Control.Monad (liftM)
import Control.Wire (stepWire, mkPure, mkSFN)
import Control.Wire.Unsafe.Event (Event(..) )

-- import Data.Set (Set, insert, delete, null, filter)

import FRP.Netwire hiding (empty)

import Text.Printf (printf)

import qualified SDL as SDL

import System.Exit

import Types


--- Arrow
---
justFirst  ∷ Arrow a ⇒ a (b, c) b
justFirst  = arr $ \(b, _) → b

justSecond ∷ Arrow a ⇒ a (b, c) c
justSecond = arr $ \(_, c) → c


--- Netwire
---
-- | Like 'edge', but only produce an event on the rising edge.
redge :: (a -> Bool) -> Wire s e m a (Event a)
redge p = off
    where
    off = mkSFN $ \x -> if (p x) then (Event x, on) else (NoEvent, off)
    on  = mkSFN $ \x -> if (p x) then (NoEvent, on) else (NoEvent, off)

-- | Like 'edge', but only produce an event on the falling edge.
fedge :: (a -> Bool) -> Wire s e m a (Event a)
fedge p = off
    where
    off = mkSFN $ \x -> if (p x) then (NoEvent, on) else (NoEvent, off)
    on  = mkSFN $ \x -> if (p x) then (NoEvent, on) else (Event x, off)

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


--- SDL
---
parseEvents :: Sim w i ⇒ i -> IO i
parseEvents i = do
  event ← SDL.pollEvent
  case event of
    Nothing
      → return i
    Just e | (SDL.KeyboardEvent{..}) ← SDL.eventPayload e
      → parseEvents $ if keyboardEventKeyMotion == SDL.KeyDown
                      then trackKeyDown i $ SDL.keysymScancode keyboardEventKeysym
                      else trackKeyUp   i $ SDL.keysymScancode keyboardEventKeysym
    _
      → parseEvents $ i

inhibitOnKey ∷ Sim w i ⇒ SDL.Scancode → SimWire i i
inhibitOnKey key = when (not . keyDown key)

produceOnKey ∷ Sim w i ⇒ SDL.Scancode → SimWire i i
produceOnKey key = when (keyDown key)

terminate ∷ Bool → String → IO a
terminate successp reason = do
  printf "%s: %s\n" (if successp then "Exiting" else "FATAL" ∷ String) reason
  SDL.quit
  exitWith (if successp then ExitSuccess else ExitFailure 1)

catchSDLFatally ∷ IO a -> IO a
catchSDLFatally = (flip catch) (\e -> terminate True $ show (e ∷ SDL.SDLException))

-- onWorldInput ∷ Sim w i ⇒ SimWire i i → SimWire w w
-- onWorldInput wire = proc w → do
--                       outs ← wire -< inputsOf w
--                       returnA -< w { wInputs = outs }

-- stepper ∷ Sim w i ⇒ SDL.Window → SDL.Renderer → w → w → Session IO SimTime → SimWire w (Bool, s) → IO ()
-- stepper win rend world w0 sesn wire = do
--   keysDown <- parseEvents (inputsOf world)
--   let wWire = (wire . (onWorldInput $ inhibitOnKey  SDL.ScancodeQ)
--               --> pure (False, undefined))
--   (nextScene, nextSesn, nextWire) ← do
--     (st , sesn') ← stepSession sesn
--     (ret, wire') ← stepWire wWire st $ Right w0
--     case ret of
--       Left  iv              → error $ printf "stepWire: inhibited (got a %s)" iv
--       Right (False, _)      → terminate True "user requested end of simulation"
--       Right (True, scene')  → return (scene', sesn', wire')
--   -- putStrLn $ show nextScene
--   threadDelay 100000
--   stepper win rend (nextWorld keysDown) w0 nextSesn nextWire

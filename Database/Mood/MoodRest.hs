{-# LANGUAGE Arrows #-}
-- {-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
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


stepper ∷ SDL.Window → SDL.Renderer → (String, Inputs) → Session IO SimTime → SimWire World (Bool, String) → IO ()
stepper win rend (preScene, preKeysDown) sesn wire = do
  keysDown <- parseEvents preKeysDown
  let wWire = (wire . (first $ inhibitOnKey SDL.ScancodeQ)
              --> pure (False, undefined))
  (nextScene, nextSesn, nextWire) ← do
    (st , sesn') ← stepSession sesn
    (ret, wire') ← stepWire wWire st $ Right (keysDown, "<start>")
    case ret of
      Left  iv              → error $ printf "stepWire: inhibited (got a %s)" iv
      Right (False, _)      → terminate True "user requested end of simulation"
      Right (True, scene')  → return (scene', sesn', wire')
  putStrLn $  nextScene
  threadDelay 100000
  stepper win rend (nextScene, keysDown) nextSesn nextWire

mkLoop ∷ String → SimWire World World
mkLoop x = loo
    where loo = for 0.2  . second (pure $ "..." ++ x ++ "!") -->
                loo

inputsToScene ∷ Inputs → (SimWire World World)
inputsToScene (Inputs ks) =
    let kmap = [ (SDL.ScancodeY, mkLoop "Yay")
               , (SDL.ScancodeL, mkLoop "Lol")
               , (SDL.ScancodeW, mkLoop "Whew")]
        ix   = intersection ks (fromList $ map fst kmap)
    in if null ix
       then mkId
       else fromMaybe (mkLoop "canthappen") $
            lookup (head $ elems ix) kmap

addWireEvent ∷ SimWire World (World, Event (SimWire World World))
addWireEvent = proc w → do
               ev ← ((onEventM (\ks → return $ inputsToScene ks) .
                     redge someKeyDown)
                    ) -< worldInputs w
               returnA -< (w, ev)

test ∷ SimWire World (Bool, String)
test = loop >>^ (\(_, x) → (True, x))
  where
    loop ∷ SimWire World World
    loop = rSwitch  (mkLoop "<rswitch0>") . addWireEvent -->
           loop
    -- loop = for 2 . plug --> loop
    -- loop = for 2 .                 second "Once upon a time..." -->
    --        for 3 .                 second "... games were completely imperative..." -->
    --        trigger SDL.ScancodeF . second "... but then..." -->
    --        for 10 .                second ("Netwire 5! " <> anim) -->
    --        loop
    -- anim = proc s → do
    --          holdFor 1 . periodic 2 . "Hoo..." <|> "...ray!" -< s

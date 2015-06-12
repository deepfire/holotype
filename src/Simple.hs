{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnicodeSyntax #-}

import Prelude hiding ((.), id, null, filter)

import Control.Concurrent (threadDelay)
import Control.Wire (stepWire)
import Control.Wire.Unsafe.Event (onEventM)
import FRP.Netwire hiding (empty)
import Text.Printf (printf)

type TestWire s a b = ∀ t . (HasTime t s, Fractional t) ⇒ Wire s String IO a b

main :: IO ()
main = triv_stepper "" clockSession_ experiment

triv_stepper ∷ HasTime t s ⇒ Fractional t ⇒ String → Session IO s → TestWire s String String → IO ()
triv_stepper _ sesn wire = do
  (nextScene, nextSesn, nextWire) ← do
    (st , sesn') ← stepSession sesn
    (ret, wire') ← stepWire wire st $ Right "<start>"
    case ret of
      Left  iv         → error $ printf "stepWire: inhibited (got a %s)" iv
      Right scene'     → return (scene', sesn', wire')
  putStrLn $  nextScene
  threadDelay 100000
  triv_stepper nextScene nextSesn nextWire

mklo ∷ String → TestWire s String String
mklo x = loo
    where loo = for 1 . (pure $ "..." ++ x ++ "!") -->
                loo

experiment ∷ TestWire s String String
experiment = proc ins → do
         x ← loo -< ins
         returnA -< x
  where
    loo ∷ TestWire s String String
    loo = rSwitch (mklo "<rswitch-base>") .
          (proc i → do
             ev ← (-- (fmap (show . const . arr))
                   onEventM (\x → return $ arr $ const (show x))
                   . edge (\x →
                           odd $ floor (1 * x))
                  <|> never) . timeF -< ()
             returnA -< (i, ev)) -->
          for 2 . mklo "rSwitch inhibited " -->
          loo

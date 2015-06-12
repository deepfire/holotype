{-# LANGUAGE Arrows #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UnicodeSyntax #-}

module Types
    (
    -- Core wires
      SimTime, SimWire

    -- Types
    , Sim(..)
    , onWorldInput
    ) where

import Control.Wire
import SDL (Scancode)

type SimTime     = (Timed NominalDiffTime ())
type SimWire a b = Wire SimTime String IO a b

class Sim w i s | w → i, i → w, w → s where
    inputsOf        ∷ w → i
    sceneOf         ∷ w → s
    nextWorld       ∷ i → s → w
    --
    trackKeyDown    ∷ i → SDL.Scancode → i
    trackKeyUp      ∷ i → SDL.Scancode → i
    someKeyDown     ∷ i → Bool
    keyDown         ∷ SDL.Scancode → i → Bool

onWorldInput ∷ Sim w i s ⇒ SimWire i i → SimWire w w
onWorldInput wire = proc w → do
                      outs ← wire -< inputsOf w
                      returnA -< nextWorld outs (sceneOf w)

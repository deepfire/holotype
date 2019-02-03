{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Reflex.Random
  ( module Control.Monad.Random
  , module System.Random
  , foldGen
  , foldRandoms
  , foldRandomRs
  , foldRand
  )

where
import           ExternalImports

-- | Reflex.Gloss.Random
-- A module for using random number state with reflex.
-- Cannibalised from https://hackage.haskell.org/package/reflex-gloss-scene-0.1.2/docs/src/Reflex-Gloss-Random.html#foldGen

-- These should really be in base!
instance (Random a, Random b) ⇒ Random (a, b) where
  randomR ((a, b), (a', b'))  = runState (liftA2 (,) (state $ randomR (a, a')) (state $ randomR (b, b')))
  random        = runState (liftA2 (,) (state random) (state random))

instance (Random a, Random b, Random c) ⇒ Random (a, b, c) where
  randomR ((a, b, c), (a', b', c'))  = runState (liftA3 (,,) (state $ randomR (a, a')) (state $ randomR (b, b')) (state $ randomR (c, c')))
  random        = runState (liftA3 (,,) (state random) (state random) (state random))

instance (Random a, Random b, Random c, Random d) ⇒ Random (a, b, c, d) where
  randomR ((a, b, c, d), (a', b', c', d'))  = runState ((,,,) <$> state (randomR (a, a')) <*> state (randomR (b, b'))  <*> state (randomR (c, c')) <*> state (randomR (d, d')))
  random        = runState ((,,,) <$> state random  <*> state random <*> state random <*> state random)


foldGen ∷ (Reflex t, MonadHold t m, MonadFix m) ⇒ s → (s → (a, s)) → Event t () → m (Event t a)
foldGen initialState f input = do
  rec
    curState <- hold initialState newState
    let (outputs, newState) = splitE $ f <$> tag curState input
  return outputs


foldRandoms ∷ (Reflex t, MonadHold t m, MonadFix m, Random a) ⇒ Int → Event t () → m (Event t a)
foldRandoms seed = foldGen (mkStdGen seed) random

foldRandomRs ∷ (Reflex t, MonadHold t m, MonadFix m, Random a) ⇒ Int → (a, a) → Event t () → m (Event t a)
foldRandomRs seed range = foldGen (mkStdGen seed) (randomR range)

foldRand ∷ (Reflex t, MonadHold t m, MonadFix m, RandomGen g) ⇒ g → Rand g a → Event t () → m (Event t a)
foldRand g f = foldGen g (runRand f)

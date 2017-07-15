{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}

-- Thanks to Christian Baaij, and everyone involved, to make this possible!

module Data.MeasuredMonoid
  ( MeasuredMonoid(..)
  )
where

-- Type-level
import           GHC.TypeLits

class MeasuredMonoid a where
  mmempty  ∷ a 0
  mmappend ∷ a n → a m → a (n + m)

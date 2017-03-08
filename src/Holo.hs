{-# LANGUAGE AllowAmbiguousTypes, ExplicitForAll, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RankNTypes, UndecidableInstances #-}
{-# LANGUAGE GADTs, TypeFamilies, TypeFamilyDependencies, TypeInType #-}
{-# LANGUAGE LambdaCase, OverloadedStrings, PartialTypeSignatures, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TupleSections, TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}

module Holo
  ( -- * Basis
    Holo
  , Visual
  , visualise, updateVisual
  -- * Wires
  , visual
  )
where

import           GHC.Types
import           GHC.Stack
import           Prelude                           hiding ((.), id)
import           Prelude.Unicode

import           Control.Monad.IO.Class                   (MonadIO, liftIO)

import           Reflex

-- Local imports
import Flatland (Unit(..))
import HoloCanvas
import HoloCube (ObjectStream)
import HoloFlex
import HoloSettings (Settings)


class (WDrawable (Visual a), Content (Visual a) ~ a) ⇒ Holo a where
  type Visual a = (r ∷ Type) | r → a
  visualise    ∷ (MonadIO m) ⇒ a → m (Visual a)        -- ^ Produce an initial visualisation of 'a'.
  updateVisual ∷ (MonadIO m) ⇒ a →   (Visual a) → m () -- ^ Update a visualisation of 'a'.

-- | A non-caching wire from a 'Holo' to its 'Visual'.
visual ∷ (HasCallStack, ReflexGLFWCtx t m, Holo a) ⇒ Settings PU → ObjectStream → StyleOf (Visual a) → Event t a → m (Event t (Visual a))
visual settings stream style holoE =
  performEvent $ (liftIO ∘ assemble settings stream style) <$> holoE

{-# LANGUAGE AllowAmbiguousTypes, ExplicitForAll, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RankNTypes, UndecidableInstances #-}
{-# LANGUAGE GADTs, TypeFamilies, TypeFamilyDependencies, TypeInType #-}
{-# LANGUAGE LambdaCase, OverloadedStrings, PartialTypeSignatures, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TupleSections, TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}

module Holo
  (
  -- * Define this:
    Holo, Visual
  , visualise, updateVisual
  -- * Get that:
  , Holosome(..)
  , visual
  , update
  )
where

import           GHC.Types
import           Prelude                           hiding ((.), id)
import           Prelude.Unicode
import           Control.Lens
import           Data.Function
import           Data.Functor

import           Control.Monad.IO.Class                   (MonadIO, liftIO)
import qualified Data.IORef                        as IO

import           Reflex

-- Local imports
import Flatland (Unit(..))
import HoloCanvas
import HoloCube (ObjectStream)
import HoloFlex
import HoloSettings (Settings)


-- | 'Holo': anything visualisable.
class (WDrawable (Visual a)) ⇒ Holo a where
  type Visual a ∷ Type
  visualise     ∷ (MonadIO m) ⇒ Settings PU → ObjectStream → StyleOf (Visual a) → a → m (Visual a) -- ^ Produce an initial visualisation of 'a'.
  updateVisual  ∷ (MonadIO m) ⇒ Settings PU → ObjectStream →         (Visual a) → a → m ()         -- ^ Update a visualisation of 'a'.


-- | 'Holosome':  pair a 'Holo' with its visualisation context.
data Holosome a where
  Holosome ∷ (Holo a, WDrawable (Visual a)) ⇒
    { holoRef    ∷ IO.IORef a
    , holoStyle  ∷ StyleOf (Visual a)
    , holoStream ∷ ObjectStream
    , holoVisual ∷ Visual a
    } → Holosome a

visual ∷ (ReflexGLFWCtx t m, Holo a) ⇒ Settings PU → ObjectStream → StyleOf (Visual a) → Event t a → m (Event t (Holosome a))
visual stts holoStream holoStyle holoE =
  performEvent $ (holoE <&> (\holo → liftIO $ do
                                holoVisual ← visualise stts holoStream holoStyle holo
                                render holoVisual
                                holoRef ← IO.newIORef holo
                                pure Holosome{..}))

update ∷ (MonadIO m, Holo a) ⇒ Settings PU → Holosome a → (a → a) → m ()
update stts Holosome{..} f = do
  old ← liftIO $ IO.readIORef holoRef
  let new = f old
  liftIO $ IO.writeIORef holoRef new
  updateVisual stts holoStream holoVisual new

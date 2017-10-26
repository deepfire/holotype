{-# LANGUAGE AllowAmbiguousTypes, ExplicitForAll, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RankNTypes, UndecidableInstances #-}
{-# LANGUAGE GADTs, TypeFamilies, TypeFamilyDependencies, TypeInType #-}
{-# LANGUAGE LambdaCase, OverloadedStrings, PartialTypeSignatures, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TupleSections, TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors #-}
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
import           Control.Lens
import           Text.Printf                              (printf)

import           Control.Monad.IO.Class                   (MonadIO, liftIO)
import qualified Data.IORef                        as IO

import           Reflex
import           Reflex.GLFW

-- Local imports
import Flatland
import HoloCanvas
import HoloCube (ObjectStream)
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
    -- , holoPosRef ∷ IO.IORef (Po (Dim PU))
    } → Holosome a

instance Show (Holosome a) where
  show Holosome{..} = printf "Holo { style = %s }" (show holoStyle)

visual ∷ (ReflexGLFWCtx t m, Holo a) ⇒ Settings PU → ObjectStream → StyleOf (Visual a) → Event t (a, b) → m (Event t (Holosome a, b))
visual stts holoStream holoStyle holoE =
  performEvent (holoE <&> ((\(holo, x) → liftIO $ do
                               -- XXX/expressivity:  this threading of 'x' is..
                               holoVisual ← visualise stts holoStream holoStyle holo
                               holoRef    ← IO.newIORef holo
                               -- holoPosRef ← IO.newIORef pos
                               pure (Holosome{..}, x))
                          ))

update ∷ (MonadIO m, Holo a) ⇒ Settings PU → Holosome a → (a → a) → m ()
update stts Holosome{..} f = do
  old ← liftIO $ IO.readIORef holoRef
  let new = f old
  liftIO $ IO.writeIORef holoRef new
  updateVisual stts holoStream holoVisual new


-- class HoloSpace a where
--   position ∷ a → Holosome h → Po (Dim PU)


-- class HoloSpace a ⇒ HoloPlace a where
--   renderer ∷ Renderer
--   streamO  ∷ ObjectStream
--   settings ∷ ReflexGLFWCtx t m ⇒ a → m (Dynamic t (Settings PU))


-- data RandomLayout where
--   RandomLayout ∷
--     { rlArea ∷ Area (Dim PU)
--     } → RandomLayout

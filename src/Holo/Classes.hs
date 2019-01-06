{-# OPTIONS_GHC -Weverything #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors -Wno-missing-import-lists -Wno-implicit-prelude -Wno-monomorphism-restriction -Wno-name-shadowing -Wno-all-missed-specialisations -Wno-unsafe -Wno-missing-export-lists -Wno-type-defaults -Wno-partial-fields -Wno-missing-local-signatures -Wno-orphans #-}
module Holo.Classes
  ( As(..)
  --
  , Mutable(..)
  --
  , Named(..)
  --
  , Widgety(..)
  , HGLFW, API, APIt, APIm
  --
  , Interp(..)
  --
  , Present(..)
  )
where

import           Data.Typeable                            (Typeable)
import           Generics.SOP                             (Proxy)
import           Reflex                                   (Event, Dynamic)
import           Reflex.GLFW                              (RGLFW)

-- Local imports
import           Graphics.Flatland

import           Holo.Input
import           Holo.Instances.Mutable
import           Holo.Name
import           Holo.Port                                (IdToken, Drawable)
import           Holo.Prelude
import           Holo.Widget


-- | As -- assigning representation
--
class Typeable r  ⇒ As r where
  type         Denoted r ∷ Type
  type             Sty r ∷ Type
  type instance    Sty r = ()
  type          IStruc r ∷ Type
  type instance IStruc r = ()
  type             Vis r ∷ Type
  type instance    Vis r = ()
  defAs       ∷               Proxy r             → r
  defSty      ∷               Proxy r             → Sty r
  compSty     ∷                     r             → Sty r
  compSty                           x             = defSty (proxy x)
  sizeRequest ∷ MonadIO m ⇒ VPort → r → Denoted r → Sty r → m (IStruc r, Di (Maybe Double))
  setupVis    ∷ MonadIO m ⇒ VPort → r → Denoted r → Sty r → IStruc r → Area'LU Double → Drawable → m (Vis r)
  render      ∷ MonadIO m ⇒ VPort → r → Denoted r → Sty r → IStruc r → Po Double      → Drawable → Vis r → m () -- ^ Update visual.
  freeVis     ∷ MonadIO m ⇒   Proxy r                                                            → Vis r → m ()
  freeVis                           _                                                                  _ = pure ()


-- | Mutability: making values dynamic and declare their relevant InputEvent's.
--
class Mutable a where
  subscription ∷                    IdToken → Proxy a → Subscription
  subscription = const mempty         -- declare ignorance..
  mutate       ∷ (RGLFW t m) ⇒ a → Event t InputEvent → m (Dynamic t a)
  mutate       = immutable            -- ..then effectuate it


-- | Named, choice of presentation
--
class Named a where
  compName ∷ (As n, Denoted n ~ a) ⇒ Proxy a → IdToken → n → Name n
  compName = defStyGeoName


-- | Widgety: turn values into interactive widgets.
--
class (Typeable a) ⇒ Widgety (i ∷ Type) (a ∷ Type) where
  dynWidget'   ∷ (HGLFW i t m, Typeable a, Named a, Widgety i a, HasCallStack)
               ⇒         IdToken → Vocab i (Present i) → Dynamic t a → m (Widget i a)
  widget       ∷ (HGLFW i t m, Typeable a, HasCallStack)
               ⇒ InputEventMux t → Vocab i (Present i) →           a → m (Widget i a)
  --
  default dynWidget'
               ∷ (HGLFW i t m, Mutable a, Named a)
               ⇒         IdToken → Vocab i (Present i) → Dynamic t a → m (Widget i a)
  dynWidget'   = dynWidgetStaticSubs
  widget       = newMutatedSeedWidget


-- | Interp: assigning interpretation
--
class Interp (a ∷ Type) (b ∷ Type) where
  interp        ∷ a → Maybe b
  forget        ∷ b → a


-- | Present:  Widgety with added interpretation
--
class (Typeable a) ⇒ Present (i ∷ Type) (a ∷ Type) where
  dynPresent    ∷ (HGLFW i t m, HasCallStack)
                ⇒ InputEventMux t → Vocab i (Present i) → Dynamic t a → m (Widget i a)
  present       ∷ (HGLFW i t m, HasCallStack)
                ⇒ InputEventMux t → Vocab i (Present i) →           a → m (Widget i a)
  --
  dynPresent    = dynPresentDef
  present       = presentDef

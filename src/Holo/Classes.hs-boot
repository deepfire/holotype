module Holo.Classes
where

import           Control.Monad.IO.Class                   (MonadIO)
import           Data.Proxy                               (Proxy)
import           Data.Typeable                            (Typeable)
import           GHC.Types                                (Type)
import           GHC.Stack                                (HasCallStack)
import           Reflex                                   (Event, Dynamic)
import           Reflex.GLFW                              (RGLFW)

import           Graphics.Flatland

import           Holo.Input
import {-# SOURCE #-}
                 Holo.Name
import           Holo.Port
import {-# SOURCE #-}
                 Holo.Widget

class Typeable r ⇒ As r where
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
  freeVis     ∷ MonadIO m ⇒   Proxy r                                                         → Vis r → m ()
  freeVis                           _                                                               _ = pure ()

class Mutable a where
  subscription ∷                    IdToken → Proxy a → Subscription
  subscription = const mempty         -- declare ignorance..
  mutate       ∷ (RGLFW t m) ⇒ a → Event t InputEvent → m (Dynamic t a)
  mutate       = immutable            -- ..then effectuate it

class Named a where
  compName ∷ (As n, Denoted n ~ a) ⇒ Proxy a → IdToken → n → Name n
  compName = defStyGeoName

class (Typeable a) ⇒ Widgety (i ∷ Type) (a ∷ Type) where
  dynWidget'   ∷ (HGLFW i t m, Typeable a, Named a, Widgety i a, HasCallStack)
               ⇒ LBinds → Input t → IdToken → Vocab i (Present i) → Dynamic t a → m (Widget i a)
  widget       ∷ (HGLFW i t m, Typeable a, HasCallStack)
               ⇒ LBinds → Input t           → Vocab i (Present i) →           a → m (Widget i a)
  --
  default dynWidget'
               ∷ (HGLFW i t m, Mutable a, Named a)
               ⇒ LBinds → Input t → IdToken → Vocab i (Present i) → Dynamic t a → m (Widget i a)
  dynWidget'   = dynWidget'Def
  widget       = widgetDef

class Interp (a ∷ Type) (b ∷ Type) where
  interp        ∷ a → Maybe b
  forget        ∷ b → a

class (Typeable a) ⇒ Present (i ∷ Type) (a ∷ Type) where
  dynPresent    ∷ (HGLFW i t m, HasCallStack)
                ⇒ LBinds → Input t          → Vocab i (Present i) → Dynamic t a → m (Widget i a)
  present       ∷ (HGLFW i t m, HasCallStack)
                ⇒ LBinds → Input t          → Vocab i (Present i) →           a → m (Widget i a)
  --
  dynPresent    = dynPresentDef
  present       = presentDef

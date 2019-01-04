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
                 Holo.Widget2

class Typeable r ⇒ As r where
  type         Denoted r ∷ Type

class Mutable a where
  mutate       ∷ (RGLFW t m) ⇒ a → Event t InputEvent → m (Dynamic t a)

class Named a where

class (Typeable a) ⇒ Widgety (i ∷ Type) (a ∷ Type) where
  dynWidget'   ∷ (Typeable a, Named a, Widgety i a, HasCallStack)
               ⇒         IdToken → Vocab i (Present i) → Dynamic t a → m (Widget i a)

class Interp (a ∷ Type) (b ∷ Type) where

class (Typeable a) ⇒ Present i a

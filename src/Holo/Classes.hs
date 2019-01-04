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
import           Holo.Widget2


class Typeable r  ⇒ As r where
  type         Denoted r ∷ Type

class Mutable a where

class Named a where

class (Typeable a) ⇒ Widgety (i ∷ Type) (a ∷ Type) where

class Interp (a ∷ Type) (b ∷ Type) where

class (Typeable a) ⇒ Present i a where

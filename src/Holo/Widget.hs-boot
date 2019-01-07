module Holo.Widget
where

import           Data.Kind                                (Type)
import qualified Data.TypeMap.Dynamic              as TM
import           Generics.SOP                             (Top)
import           Generics.SOP.Monadic                     (Result)
import           GHC.TypeLits
import           Reflex                                   (Dynamic)
import           Reflex.GLFW                              (RGLFW)

import           Holo.Input                               (AElt, SemSubs, Subscription)
import {-# SOURCE #-}
                 Holo.Item


type role Definition nominal nominal
data Definition  (i ∷ Type) (a ∷ Type)

newtype Vocab i c = Vocab (TM.TypeMap (HoloTag i))
data                                   HoloTag (i ∷ Type)
type role HoloTag phantom

type Blank   i     = Item Top PBlank

type HGLFW i t m   = (t ~ (APIt i), m ~ (APIm i), RGLFW t m)

type role API phantom phantom
data API (t ∷ Type) (m ∷ Type → Type)

type family APIt a ∷ Type where
  APIt (API t _) = t
  APIt _         = TypeError ('Text "APIt on non-API.")

type family APIm a ∷ (Type → Type) where
  APIm (API _ m) = m
  APIm _         = TypeError ('Text "APIm on non-API.")

type WH      i   = (AElt, SemSubs → Dynamic (APIt i) Subscription, Dynamic (APIt i) (Blank i))
type WF      i b = (AElt, SemSubs → Dynamic (APIt i) Subscription, Dynamic (APIt i) (Blank i), Dynamic (APIt i) b)

type Widget  i b = Result i b

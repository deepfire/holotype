module Holo.Widget2
where

import           Data.Kind                                (Type)
import qualified Data.TypeMap.Dynamic              as TM
import           GHC.Types                                (Constraint)
import           Generics.SOP                             (Top)
import           Generics.SOP.Monadic                     (Result)
import           Reflex                                   (Dynamic)
import           Reflex.GLFW                              (RGLFW)

import           Holo.Input                               (Subscription)
import {-# SOURCE #-}
                 Holo.Item


data Definition i a

newtype Vocab (i ∷ Type) (c ∷ Type → Constraint) = Vocab (TM.TypeMap (HoloTag i))
data                                   HoloTag i

type Blank   i     = Item Top PBlank

type HGLFW i t m   = (t ~ (APIt i), m ~ (APIm i), RGLFW t m)

data API t m

type family APIt a ∷ Type
type family APIm a ∷ Type → Type

type WH        i   = (Dynamic (APIt i) Subscription, Dynamic (APIt i) (Blank i))
type WF        i b = (Dynamic (APIt i) Subscription, Dynamic (APIt i) (Blank i), Dynamic (APIt i) b)

type Widget    i b = Result i b

module Holo.Widget2
where

import           Data.Kind                                (Type)
import qualified Data.TypeMap.Dynamic              as TM
import           Generics.SOP                             (Top)
import           Generics.SOP.Monadic                     (Result)
import           Reflex                                   (Dynamic)
import           Reflex.GLFW                              (RGLFW)

import           Holo.Input                               (Subscription)
import {-# SOURCE #-}
                 Holo.Item


data Definition i a

newtype Vocab i c = Vocab (TM.TypeMap (HoloTag i))
data                                   HoloTag i

type Widget    i b = Result i b

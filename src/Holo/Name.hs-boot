module Holo.Name
where

import           Data.Kind                                (Type)

import qualified Holo.Port                         as Port


data      Name (a ∷ Type)
type role Name nominal

data      Visual (a ∷ Type)
type role Visual nominal

type VPort = Port.Port Visual
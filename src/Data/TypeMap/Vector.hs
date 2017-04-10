module Data.TypeMap.Vector
  ( TypeVector
  , empty
  , index
  , cons
  , (<|)
  , snoc
  , (|>)
  , toList
  ) where

import qualified Data.Vector as V
import Data.TypeMap.Internal.List (TypeList(..))
import Data.TypeMap.Internal.Vector

-- | Convert from a vector to a list.
toList :: TypeVector d -> TypeList l
toList (TypeVector v) = TypeList (V.toList v)

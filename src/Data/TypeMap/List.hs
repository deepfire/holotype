{-# LANGUAGE PolyKinds #-}

module Data.TypeMap.List
  ( TypeList
  , empty
  , index
  , cons
  , snoc
  , toVector
  ) where

import qualified Data.Vector as V
import Data.TypeMap.Internal.List
import Data.TypeMap.Internal.Vector (TypeVector(..))

toVector :: TypeList d -> TypeVector d
toVector (TypeList l) = TypeVector (V.fromList l)

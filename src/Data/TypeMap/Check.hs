{-# LANGUAGE MagicHash #-}

-- | An invisible module checking that the representation of `Typeable` has
-- not changed too much.
module Data.TypeMap.Check where

import Data.Typeable.Internal
import GHC.Prim (Proxy#(..))

typeRep_ :: Typeable a => Proxy# a -> TypeRep
typeRep_ = typeRep#

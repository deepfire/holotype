{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}

-- | An invisible module checking that the representation of `Typeable` has
-- not changed too much.
module Data.TypeMap.Check where

#if !MIN_VERSION_base(4,10,0)
import Data.Typeable.Internal (Typeable(..), TypeRep)
import GHC.Prim (Proxy#)

typeRep_ :: Typeable a => Proxy# a -> TypeRep
typeRep_ = typeRep#
#endif

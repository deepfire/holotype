{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.TypeMap.Internal.Vector where

import Data.Vector (Vector)
import qualified Data.Vector as Vector

import Data.TypeMap.Internal.Unsafe

-- | Vector-backed type-map.
newtype TypeVector d = TypeVector (Vector Any)

empty :: TypeVector '[]
empty = TypeVector Vector.empty

index
  :: forall a d
  .  KnownNat (Index a d)
  => TypeVector d -> Lookup a d
index = unsafeIndex @a (Vector.!)

cons
  :: forall a d b
  . b -> TypeVector d -> TypeVector ('(a, b) ': d)
cons = unsafeCons Vector.cons

snoc
  :: forall a d b
  .  TypeVector d -> b -> TypeVector (Snoc d '(a, b))
snoc = unsafeSnoc @a Vector.snoc

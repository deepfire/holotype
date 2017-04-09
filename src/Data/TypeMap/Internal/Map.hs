{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.TypeMap.Internal.Map where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Data.TypeMap.Internal.Unsafe

-- | IntMap-backed type-map.
data TypeMap d = TypeMap !Int !(TypeMap' d) !Int

newtype TypeMap' d = TypeMap' (IntMap Any)

index
  :: forall a d
  .  KnownNat (Index a d)
  => TypeMap d -> Lookup a d
index (TypeMap inf m _) = unsafeIndex @a @d index' m
  where
    index' :: forall c. IntMap c -> Int -> c
    index' m n = m IntMap.! (inf + n)

cons
  :: forall a d b
  .  b -> TypeMap d -> TypeMap ('(a, b) ': d)
cons b (TypeMap inf m sup) =
  TypeMap (inf - 1) (unsafeCons cons' b m) sup
  where
    cons' :: forall c. c -> IntMap c -> IntMap c
    cons' = IntMap.insert (inf - 1)

snoc
  :: forall a d b
  .  TypeMap d -> b -> TypeMap (Snoc d '(a, b))
snoc (TypeMap inf m sup) b = TypeMap inf (unsafeSnoc @a @d @b snoc' m b) (sup + 1)
  where
    snoc' :: forall c. IntMap c -> c -> IntMap c
    snoc' m b = IntMap.insert sup b m

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.TypeMap.Internal.List where

import Data.TypeMap.Internal.Unsafe

-- | List-backed type-map.
newtype TypeList d = TypeList [Any]

index
  :: forall a d
  .  KnownNat (Index a d)
  => TypeList d -> Lookup a d
index = unsafeIndex @a (!!)

cons
  :: forall a d b
  .  b -> TypeList d -> TypeList ('(a, b) ': d)
cons = unsafeCons (:)

snoc
  :: forall a d b
  .  TypeList d -> b -> TypeList (Snoc d '(a, b))
snoc = unsafeSnoc @a (\xs x -> xs ++ [x])

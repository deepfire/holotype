{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.TypeMap.Internal.Unsafe
  ( module Data.TypeMap.Internal.Unsafe
  , module Unsafe
  ) where

import Data.Coerce
import Data.Proxy as Unsafe (Proxy(..))
#if MIN_VERSION_base(4,10,0)
import GHC.Exts as Unsafe (Any)
#else
import GHC.Prim as Unsafe (Any)
#endif
import GHC.TypeLits as Unsafe (KnownNat)
import GHC.TypeLits (type (+), natVal)
import Unsafe.Coerce as Unsafe

-- | Index of type key @a@ in association list @d@.
type family Index (a :: k) (d :: [(k, *)]) where
  Index a ('(a, _) ': _) = 0
  Index a (_ ': d) = 1 + Index a d

-- | Type associated with @a@ in @d@. If the key @a@ occurs multiple times,
-- the first one is used.
type family Lookup (a :: k) (d :: [(k, *)]) where
  Lookup a ('(a, b) ': _) = b
  Lookup a (_ ': d) = Lookup a d

-- | Append a type to a list.
type family Snoc (d :: [k]) (a :: k) where
  Snoc '[] a = '[a]
  Snoc (x ': d) a = x ': Snoc d a

-- | Last element of a list.
type family Last (d :: [k]) where
  Last (x ': '[]) = x
  Last (_ ': d) = Last d

-- | All elements except the last one.
type family Init (d :: [k]) where
  Init (_ ': '[]) = '[]
  Init (x ': d) = x ': Init d

-- | Helper to define @index@ functions.
unsafeIndex
  :: forall a d f m
  .  (KnownNat (Index a d), Coercible (f Any) (m d))
  => (forall c. f c -> Int -> c)
  -> m d -> Lookup a d
unsafeIndex index = unsafeCoerce (flip index na)
  where
    na = fromInteger (natVal (Proxy @(Index a d))) :: Int

-- | Helper to define @cons@ functions.
unsafeCons
  :: forall a d b f m
  .  (Coercible (f Any) (m d), Coercible (f Any) (m ('(a, b) ': d)))
  => (forall c. c -> f c -> f c)
  -> b -> m d -> m ('(a, b) ': d)
unsafeCons = unsafeCoerce

-- | Helper to define @snoc@ functions.
unsafeSnoc
  :: forall a d b f m
  .  (Last d ~ '(a, b), Coercible (f Any) (m (Init d)), Coercible (f Any) (m d))
  => (forall c. f c -> c -> f c)
  -> m (Init d) -> b -> m d
unsafeSnoc = unsafeCoerce

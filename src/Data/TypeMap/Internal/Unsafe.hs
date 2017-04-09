{-# LANGUAGE AllowAmbiguousTypes #-}
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
  ( Index
  , Lookup
  , Snoc
  , unsafeIndex
  , unsafeCons
  , unsafeSnoc

  , Proxy(..)
  , Any
  , Nat
  , KnownNat
  , natVal
  ) where

import Data.Coerce
import Data.Proxy
import GHC.Prim (Any )
import GHC.TypeLits
import Unsafe.Coerce

type family Index (a :: k) (d :: [(k, *)]) where
  Index a ('(a, _) ': _) = 0
  Index a (_ ': d) = 1 + Index a d

type family Lookup (a :: k) (d :: [(k, *)]) where
  Lookup a ('(a, b) ': _) = b
  Lookup a (_ ': d) = Lookup a d

type family Snoc (d :: [k]) (a :: k) = d' where
  Snoc '[] a = '[a]
  Snoc (x ': d) a = x ': Snoc d a

unsafeIndex
  :: forall a d f m
  .  (KnownNat (Index a d), Coercible (f Any) (m d))
  => (forall c. f c -> Int -> c)
  -> m d -> Lookup a d
unsafeIndex index m = unsafeCoerce (flip index na)
  where
    na = fromInteger (natVal (Proxy @(Index a d)))

unsafeCons
  :: forall a d b f m
  .  (Coercible (f Any) (m d), Coercible (f Any) (m ('(a, b) ': d)))
  => (forall c. c -> f c -> f c)
  -> b -> m d -> m ('(a, b) ': d)
unsafeCons = unsafeCoerce

unsafeSnoc
  :: forall a d b f m
  .  (Coercible (f Any) (m d), Coercible (f Any) (m (Snoc d '(a, b))))
  => (forall c. f c -> c -> f c)
  -> m d -> b -> m (Snoc d '(a, b))
unsafeSnoc = unsafeCoerce

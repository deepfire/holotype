{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Data.TypeMap.TypeApplications.Internal where

import Data.Typeable
import GHC.Prim (Proxy#)
import Unsafe.Coerce
import qualified Data.Map as Map

import Data.TypeMap.Internal
  (TypeMap(..), Item, Typed, UnTyped, ItemFun, ItemKleisli)

map
  :: forall x y. (forall t. Typeable t => Item x t -> Item y t)
  -> TypeMap x -> TypeMap y
map f (TypeMap m) = TypeMap (Map.mapWithKey f' m)
  where
    f' = withTypeRep @(ItemFun x y)
      (Typed_ (f @t) :: forall t. Typeable t => Typed_ (ItemFun x y) t)

traverse
  :: forall f x y
  .  Applicative f
  => (forall t. Typeable t => Item x t -> f (Item y t))
  -> TypeMap x -> f (TypeMap y)
traverse f (TypeMap m) = TypeMap <$> Map.traverseWithKey f' m
  where
    f' = withTypeRep @(ItemKleisli f x y)
      (Typed_ (f @t) :: forall t. Typeable t => Typed_ (ItemKleisli f x y) t)

-- * Unsafe internals

newtype Typed_ x t = Typed_ (Typed x t)

newtype WithTypeable x
  = WithTypeable (forall t. Typeable t => Typed_ x t)

withTypeable
  :: WithTypeable x -> (Proxy# t -> TypeRep) -> UnTyped x
withTypeable = unsafeCoerce

withTypeRep
  :: forall x
  .  (forall t. Typeable t => Typed_ x t)
  -> TypeRep -> UnTyped x
withTypeRep f rep =
  withTypeable (WithTypeable f :: WithTypeable x) (\_ -> rep)

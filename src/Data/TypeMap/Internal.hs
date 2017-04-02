{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.TypeMap.Internal where

import Data.Map (Map)
import Data.Proxy (Proxy(..))
import Data.Typeable
import GHC.Prim (Any, Proxy#)
import Unsafe.Coerce

import qualified Data.Map as Map

-- * Exposed functions

-- | Map from types @t@ of kind @*@ to values of type @Item x t@.
newtype TypeMap x = TypeMap (Map TypeRep Any)

type family Item x t

data OfType a
type instance Item (OfType a) t = a

null :: TypeMap x -> Bool
null (TypeMap m) = Map.null m

insert
  :: forall t x proxy
  .  Typeable t => proxy t -> Item x t -> TypeMap x -> TypeMap x
insert t v (TypeMap m) = TypeMap (Map.insert (typeRep t) (coerce v) m)
  where
    coerce :: Item x t -> Any
    coerce = unsafeCoerce

lookup
  :: forall t x proxy
  .  Typeable t => proxy t -> TypeMap x -> Maybe (Item x t)
lookup t (TypeMap m) = coerce (Map.lookup (typeRep t) m)
  where
    coerce :: Maybe Any -> Maybe (Item x t)
    coerce = unsafeCoerce

map
  :: forall x y
  .  (forall t. Typeable t => Proxy t -> Item x t -> Item y t)
  -> TypeMap x -> TypeMap y
map f (TypeMap m) = TypeMap (Map.mapWithKey f' m)
  where f' = withTypeRep f (Proxy :: Proxy (ItemFun x y))

traverse
  :: forall f x y
  .  Applicative f
  => (forall t. Typeable t => Proxy t -> Item x t -> f (Item y t))
  -> TypeMap x -> f (TypeMap y)
traverse f (TypeMap m) = TypeMap <$> Map.traverseWithKey f' m
  where f' = withTypeRep f (Proxy :: Proxy (ItemKleisli f x y))

-- * Unsafe internals

type family Typed x t
type family UnTyped x

type instance Typed (OfType a) t = a
type instance UnTyped (OfType a) = a

data ItemFun x y
type instance Typed (ItemFun x y) t = Item x t -> Item y t
type instance UnTyped (ItemFun x y) = Any -> Any

data ItemKleisli (f :: * -> *) x y
type instance Typed (ItemKleisli f x y) t = Item x t -> f (Item y t)
type instance UnTyped (ItemKleisli f x y) = Any -> f Any

newtype WithTypeable x
  = WithTypeable (forall t. Typeable t => Proxy t -> Typed x t)

withTypeable :: WithTypeable x -> (Proxy# a -> TypeRep) -> Proxy () -> UnTyped x
withTypeable = unsafeCoerce

withTypeRep
  :: forall x proxy
  .  (forall t. Typeable t => Proxy t -> Typed x t)
  -> proxy x
  -> TypeRep -> UnTyped x
withTypeRep f _ rep =
  withTypeable (WithTypeable f :: WithTypeable x) (\_ -> rep) Proxy

{-# LANGUAGE CPP #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.TypeMap.Internal.Dynamic where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (Applicative, (<$>))
#endif
import Data.Map (Map)
import Data.Proxy (Proxy(..))
import Data.Typeable
#if MIN_VERSION_base(4,10,0)
import GHC.Exts (Any)
import qualified Type.Reflection as T
#else
import GHC.Prim (Any, Proxy#)
#endif
import Unsafe.Coerce

import qualified Data.Map as Map

-- * Exposed functions

-- | Map from types @t@ of kind @*@ to values of type @Item x t@.
newtype TypeMap x = TypeMap (Map TypeRep Any)

-- | An extensible type family mapping types (as keys) to types of values,
-- parameterized by types @x@.
type family Item x t

-- | A constant mapping to type @a@. @'TypeMap' ('OfType' a)@ is the type of
-- maps from types to values of type @a@.
data OfType a
type instance Item (OfType a) t = a

-- | Whether the map is empty.
null :: TypeMap x -> Bool
null (TypeMap m) = Map.null m

-- | The number of elements in the map.
size :: TypeMap x -> Int
size (TypeMap m) = Map.size m

-- | Empty type map.
empty :: TypeMap x
empty = TypeMap Map.empty

-- | Insert an element indexed by type @t@.
insert
  :: forall t x proxy
  .  Typeable t => proxy t -> Item x t -> TypeMap x -> TypeMap x
insert t v (TypeMap m) = TypeMap (Map.insert (typeRep t) (coerce v) m)
  where
    coerce :: Item x t -> Any
    coerce = unsafeCoerce

-- | Update an element indexed by type @t@.
update
  :: forall t x proxy
  .  Typeable t => proxy t -> (Item x t -> Maybe (Item x t)) -> TypeMap x -> TypeMap x
update t f (TypeMap m) = TypeMap (Map.update (coerceA . f . coerceB) (typeRep t) m)
  where
    coerceB :: Any -> Item x t
    coerceB = unsafeCoerce
    coerceA :: Maybe (Item x t) -> Maybe Any
    coerceA = unsafeCoerce

-- | Lookup an element indexed by type @t@.
lookup
  :: forall t x proxy
  .  Typeable t => proxy t -> TypeMap x -> Maybe (Item x t)
lookup t (TypeMap m) = coerce (Map.lookup (typeRep t) m)
  where
    coerce :: Maybe Any -> Maybe (Item x t)
    coerce = unsafeCoerce

-- | Delete a key and its value from the map.
-- Does nothing if the key does not exist.
delete
  :: forall t x proxy
  .  Typeable t => proxy t -> TypeMap x -> TypeMap x
delete t (TypeMap m) = TypeMap (Map.delete (typeRep t) m)

-- | Left-biased union of two maps; it keeps the first key if duplicates are found.
union
  :: forall x. TypeMap x -> TypeMap x -> TypeMap x
union (TypeMap m) (TypeMap n) = TypeMap (Map.union m n)

-- | Difference of two maps; keep elements of the first map which are not in the second.
difference
  :: forall x. TypeMap x -> TypeMap x -> TypeMap x
difference (TypeMap m) (TypeMap n) = TypeMap (Map.difference m n)

-- | Intersection of two maps; keep elements of the first map which are also in the second.
intersection
  :: forall x y. TypeMap x -> TypeMap y -> TypeMap x
intersection (TypeMap m) (TypeMap n) = TypeMap (Map.intersection m n)

-- | Map a function on all elements.
map
  :: forall x y
  .  (forall t. Typeable t => Proxy t -> Item x t -> Item y t)
  -> TypeMap x -> TypeMap y
map f (TypeMap m) = TypeMap (Map.mapWithKey f' m)
  where f' = withTypeRep f (Proxy :: Proxy (ItemFun x y))

-- | Traverse the map.
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

withTypeRep
  :: forall x proxy
  .  (forall t. Typeable t => Proxy t -> Typed x t)
  -> proxy x
  -> TypeRep -> UnTyped x
#if MIN_VERSION_base(4,10,0)
withTypeRep f _ rep =
  case rep of
    T.SomeTypeRep (rep :: T.TypeRep t) ->
      -- We still need to unsafely coerce the kind of t to Type
      -- and Typed to UnTyped
      (unsafeCoerce
        ((\rep -> T.withTypeable rep (f (Proxy :: Proxy a)))
          :: forall a. T.TypeRep a -> Typed x a)
        :: T.TypeRep t -> UnTyped x) rep
#else
withTypeRep f _ rep =
  withTypeable (WithTypeable f :: WithTypeable x) (\_ -> rep) Proxy

newtype WithTypeable x
  = WithTypeable (forall t. Typeable t => Proxy t -> Typed x t)

withTypeable :: WithTypeable x -> (Proxy# () -> TypeRep) -> Proxy () -> UnTyped x
withTypeable = unsafeCoerce
#endif

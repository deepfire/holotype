{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Data.TypeMap.Internal.Dynamic.Alt where

import Data.Typeable
#if MIN_VERSION_base(4,10,0)
import GHC.Exts (Any)
import qualified Type.Reflection as T
#else
import GHC.Prim (Any, Proxy#)
#endif
import Unsafe.Coerce
import qualified Data.Map as Map

import Data.TypeMap.Internal.Dynamic
  (TypeMap(..), Item, Typed, UnTyped, ItemFun, ItemKleisli)

-- | Insert an element indexed by type @t@.
insert
  :: forall t x proxy
  .  Typeable t => Item x t -> TypeMap x -> TypeMap x
insert v (TypeMap m) = TypeMap (Map.insert (typeRep (Proxy @t)) (coerce v) m)
  where
    coerce :: Item x t -> Any
    coerce = unsafeCoerce

-- | Lookup an element indexed by type @t@.
lookup
  :: forall t x proxy
  .  Typeable t => TypeMap x -> Maybe (Item x t)
lookup (TypeMap m) = coerce (Map.lookup (typeRep (Proxy @t)) m)
  where
    coerce :: Maybe Any -> Maybe (Item x t)
    coerce = unsafeCoerce

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

withTypeRep
  :: forall x
  .  (forall t. Typeable t => Typed_ x t)
  -> TypeRep -> UnTyped x
#if MIN_VERSION_base(4,10,0)
withTypeRep f rep =
  case rep of
    T.SomeTypeRep (rep :: T.TypeRep t) ->
      -- We still need to unsafely coerce the kind of t to Type
      -- and Typed to UnTyped
      (unsafeCoerce
        ((\rep -> T.withTypeable rep f) :: forall a. T.TypeRep a -> Typed_ x a)
        :: T.TypeRep t -> UnTyped x) rep
#else
withTypeRep f rep =
  withTypeable (WithTypeable f :: WithTypeable x) (\_ -> rep)

newtype WithTypeable x
  = WithTypeable (forall t. Typeable t => Typed_ x t)

withTypeable
  :: WithTypeable x -> (Proxy# () -> TypeRep) -> UnTyped x
withTypeable = unsafeCoerce
#endif

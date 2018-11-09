{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeInType #-}

module Data.TypeMap.Constrained
  ( CTypeMap(..)
  , empty
  , singleton
  , insert
  , Data.TypeMap.Constrained.lookup
  )
where

import           GHC.Exts                             (Any)
import           GHC.Types                            (Constraint, Type)
import           Data.Proxy                           (Proxy(..))
import           Data.Typeable                        (Typeable, typeRep)
import qualified Data.Map                          as Map
import qualified Data.TypeMap.Internal.Dynamic     as TM
import           Unsafe.Coerce                        (unsafeCoerce)

-- | Map from types @t@ of kind @*@ to values of type @Item x t@.
newtype CTypeMap x = TypeMap (Map TypeRep (Any))

-- | Empty map over types constrained by @c@.
empty :: forall x. CTypeMap x
empty = CTypeMap (TM.TypeMap Map.empty)

-- | Construct a singleton map indexed by type @t@, that is constrained by @c@.
singleton
  :: forall (c :: Type -> Constraint) (t :: Type) x proxy
  .  (Typeable t, c t) => proxy c -> proxy t -> TM.Item x t -> CTypeMap x
singleton c t v = CTypeMap $ TM.TypeMap (Map.singleton (typeRep t) (coerce v))
  where
    coerce :: TM.Item x t -> Any
    coerce = unsafeCoerce

-- | Insert an element indexed by type @t@, that is constrained by @c@.
insert
  :: forall c t x proxy
  .  (Typeable t, c t) => proxy t -> TM.Item x t -> CTypeMap x -> CTypeMap x
insert t v (CTypeMap (c, TM.TypeMap m)) = CTypeMap . (c,) $ TM.TypeMap (Map.insert (typeRep t) (coerce v) m)
  where
    coerce :: TM.Item x t -> Any
    coerce = unsafeCoerce

-- | Lookup an element indexed by type @t@.
lookup
  :: forall c t x proxy
  .  (Typeable t, c t) => proxy t -> CTypeMap x -> Maybe (TM.Item x t)
lookup t (CTypeMap (c, TM.TypeMap m)) = coerce (Map.lookup (typeRep t) m)
  where
    coerce :: Maybe Any -> Maybe (TM.Item x t)
    coerce = unsafeCoerce

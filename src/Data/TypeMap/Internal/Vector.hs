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

-- | Empty vector.
empty :: TypeVector '[]
empty = TypeVector Vector.empty

-- | Access an element indexed by type @a@. /O(1)/
--
-- If @a@ is associated to @b@ in the type list @d@:
--
-- @
-- 'index' @a (v :: 'TypeVector' d) :: b
-- @
--
-- >>> let v = (0 :: Int) <| True <| "Hello" <| empty :: TypeVector '[ '("a", Int), '("b", Bool), '("c", String)]
-- >>> index @"c" v
-- "Hello"
index
  :: forall a d
  .  KnownNat (Index a d)
  => TypeVector d -> Lookup a d
index = unsafeIndex @a (Vector.!)

-- | Add an element to the beginning of a vector. /O(n)/
cons
  :: forall a d b
  .  b -> TypeVector d -> TypeVector ('(a, b) ': d)
cons = unsafeCons Vector.cons

-- | Synonym of 'cons'.
(<|)
  :: forall a d b
  .  b -> TypeVector d -> TypeVector ('(a, b) ': d)
(<|) = cons

infixr 5 <|, `cons`

-- | Add an element to the end of a vector. /O(n)/
snoc
  :: forall a d b
  .  (Last d ~ '(a, b))
  => TypeVector (Init d) -> b -> TypeVector d
snoc = unsafeSnoc @a Vector.snoc

-- | Synonym of 'snoc'.
(|>)
  :: forall a d b
  .  (Last d ~ '(a, b))
  => TypeVector (Init d) -> b -> TypeVector d
(|>) = snoc

infixr 5 |>, `snoc`

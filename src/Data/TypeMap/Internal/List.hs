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

-- | Empty list.
empty :: TypeList '[]
empty = TypeList []

-- | Access an element indexed by its type @a@. /O(n)/
--
-- If @a@ is associated to @b@ in the type list @d@:
--
-- @
-- 'index' @a (l :: 'TypeList' d) :: b
-- @
--
-- >>> let l = (0 :: Int) <| True <| "Hello" <| empty :: TypeList '[ '("a", Int), '("b", Bool), '("c", String)]
-- >>> index @"c" l
-- "Hello"
index
  :: forall a d
  .  KnownNat (Index a d)
  => TypeList d -> Lookup a d
index = unsafeIndex @a (!!)

-- | Add an element to the beginning of a list. /O(1)/
cons
  :: forall a d b
  .  b -> TypeList d -> TypeList ('(a, b) ': d)
cons = unsafeCons (:)

-- | Synonym of 'cons'.
(<|)
  :: forall a d b
  .  b -> TypeList d -> TypeList ('(a, b) ': d)
(<|) = cons

infixr 5 <|, `cons`

-- | Add an element to the end of a list. /O(n)/
snoc
  :: forall a d b
  .  (Last d ~ '(a, b))
  => TypeList (Init d) -> b -> TypeList d
snoc = unsafeSnoc @a (\xs x -> xs ++ [x])

-- | Synonym of 'snoc'.
(|>)
  :: forall a d b
  .  (Last d ~ '(a, b))
  => TypeList (Init d) -> b -> TypeList d
(|>) = snoc

infixr 5 |>, `snoc`

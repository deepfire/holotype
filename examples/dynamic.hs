-- A quick example of using @Data.TypeMap.Dynamic.Alt@, which requires
-- @TypeApplications@. There is another API in @Data.TypeMap.Dynamic@ which
-- doesn't need that extension.

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

import Data.TypeMap.Dynamic (TypeMap, Item)
import qualified Data.TypeMap.Dynamic.Alt as TM

-- At a high-level, values of type @TypeMap t@ are partial dependent maps,
-- intuitively of type @pi (a :: Type). Maybe (t' a)@, where the function @t'@ is
-- represented by @t@ as we are about to see.

-- More prosaically, the simplest example is a map that associates a type to a
-- value of that type.

-- A function is first given by its domain and codomain.
-- The domain is simply the kind of types @Type@ (other kinds are also possible;
-- just ask if you need that as a native feature; a wrapper @MyKind -> *@ might
-- also work).
-- The codomain may depend on the input. Here, given a type @t@, the codomain
-- will be @t@ itself (i.e., the value associated to @t@ will have type @t@).

-- Declare a symbol for a new type of map as some arbitrary datatype.
data S

-- Extend the type family @Item@, such that @Item S t@ is the codomain
-- associated with @t@.
type instance Item S t = t

-- Start with the empty map.
emptyS :: TypeMap S
emptyS = TM.empty

-- 0. We can add elements to it in this way...

-- 1. Given a type @T@, for example...
type T = Bool

-- 2. And some value of type @T@, for example...
t :: T
t = False

-- 3. We map the key @T@ to the value @t@ as follows.
one :: TypeMap S
one = TM.insert @T t emptyS
-- one : Bool -> False

-- Let us add more things.
three :: TypeMap S
three = TM.insert @Integer 33 . TM.insert @String "cat" $ one
-- three :
--   Bool    -> False
--   String  -> "cat"
--   Integer -> 33

-- If a key exists, inserting it again overwrites it.
four :: TypeMap S
four = TM.insert @String "lynx" three
-- four :
--   Bool    -> False
--   String  -> "lynx"
--   Integer -> 33

-- We can then lookup values by type.
main :: IO ()
main = do
  print (TM.lookup @Bool one)      -- Just False
  print (TM.lookup @String one)    -- Nothing
  print (TM.lookup @String three)  -- Just "cat"
  print (TM.lookup @String four)   -- Just "lynx"

-- @map@ and @traverse@ are also defined but I don't know how they can be useful.
-- If you have some idea, I'd be interested to hear about it!
-- https://github.com/Lysxia/type-map

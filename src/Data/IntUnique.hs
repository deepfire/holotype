{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE MagicHash #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.IntUnique
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- An abstract interface to a unique symbol generator, like Data.Unique, but
-- backed by Int, instead of Integer.  Which allows for better performance [1], but
-- vaguely threatens with overflows from distance.
--
-- 1. We can use IntMap.
--
-----------------------------------------------------------------------------

module Data.IntUnique (
   -- * Unique objects
   Unique,
   newUnique,
   hashUnique
 ) where

import System.IO.Unsafe (unsafePerformIO)

import GHC.Base
import GHC.Num
import Data.IORef

 -- | An abstract unique object.  Objects of type 'Unique' may be
 -- compared for equality and ordering and hashed into 'Int'.
 --
 -- >>> :{
 -- do x <- newUnique
 --    print (x == x)
 --    y <- newUnique
 --    print (x == y)
 -- :}
 -- True
 -- False

newtype Unique = Unique Int deriving (Eq,Ord)

uniqSource :: IORef Int
uniqSource = unsafePerformIO (newIORef 0)
{-# NOINLINE uniqSource #-}

-- | Creates a new object of type 'Unique'.  The value returned will
-- not compare equal to any other value of type 'Unique' returned by
-- previous calls to 'newUnique'.  The Int overflow is the limit on
-- the number of valid 'newUnique' calls.
newUnique :: IO Unique
newUnique = do
  r <- atomicModifyIORef' uniqSource $ \x -> let z = x+1 in (z,z)
  return (Unique r)

-- | Compatibiliy only, really.
hashUnique :: Unique -> Int
hashUnique (Unique i) = i
{-# INLINE hashUnique #-}

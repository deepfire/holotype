{-# OPTIONS_HADDOCK hide, prune, ignore-exports, not-home,  show-extensions #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Ground
    (
    -- Ground types
      Scale(..), Posn(..), Aspect(..), Dim(..)
    )
where

import Control.Applicative         (liftA2)
import GHC.Generics                (Generic)
import Data.Data                   (Data, Typeable)
import Data.Hashable
import Data.Semigroup              ((<>))
import Data.Semigroup.Foldable     (Foldable1, foldMap1)
import Linear.Vector               (Additive(..))
import Linear.V2                   (V2(..))


newtype Scale       = Scale        Double                deriving (Eq, Num, Show)
newtype Posn        = Posn        (V2 Double)            deriving (Eq, Num, Show)
newtype Aspect      = Aspect       Double                deriving (Eq, Num, Show, Floating, Fractional, Ord, Real, RealFrac, RealFloat)
data Dim a
    =                 DimS        (V2 a)                                      -- * Screen-space: range 0.0 - (1.0, 1.0)
    |                 DimP        (V2 a)                                      -- * Proportional: range 0.0 - (1.0, 1.0 / Aspect)
    deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

instance Functor Dim where
  fmap f (DimS (V2 a b)) = DimS $ V2 (f a) (f b)
  fmap f (DimP (V2 a b)) = DimP $ V2 (f a) (f b)
  {-# INLINE fmap #-}
  a <$ _ = DimS (V2 a a)
  {-# INLINE (<$) #-}

instance Foldable Dim where
  foldMap f (DimS (V2 a b)) = f a `mappend` f b
  foldMap f (DimP (V2 a b)) = f a `mappend` f b
  {-# INLINE foldMap #-}

-- instance Traversable Dim where
--   traverse f (DimS (V2 a b)) = (DimS Prelude.. V2) <$> f a <*> f b
--   traverse f (DimP (V2 a b)) = DimP $ V2 <$> f a <*> f b
--   {-# INLINE traverse #-}

instance Foldable1 Dim where
  foldMap1 f (DimS (V2 a b)) = f a <> f b
  foldMap1 f (DimP (V2 a b)) = f a <> f b
  {-# INLINE foldMap1 #-}

-- instance Traversable1 Dim where
--   traverse1 f (DimS (V2 a b)) = DimS (V2 <$> f a <.> f b)
--   traverse1 f (DimP (V2 a b)) = DimP <$> f a <.> f b
--   {-# INLINE traverse1 #-}

-- instance Apply Dim where -- ???
--   Dim a b <.> Dim d e = Dim (a d) (b e)
--   {-# INLINE (<.>) #-}

instance Applicative Dim where
  pure a = DimS (V2 a a)
  {-# INLINE pure #-}
  (DimS (V2 a b)) <*> (DimS (V2 d e)) = DimS (V2 (a d) (b e))
  (DimP (V2 a b)) <*> (DimP (V2 d e)) = DimP (V2 (a d) (b e))
  {-# INLINE (<*>) #-}

instance Hashable a => Hashable (Dim a) where
  hashWithSalt s (DimS (V2 a b)) = s `hashWithSalt` a `hashWithSalt` b
  hashWithSalt s (DimP (V2 a b)) = s `hashWithSalt` a `hashWithSalt` b
  {-# INLINE hashWithSalt #-}

instance Additive Dim where
  zero = pure 0
  {-# INLINE zero #-}
  liftU2 = liftA2
  {-# INLINE liftU2 #-}
  liftI2 = liftA2
  {-# INLINE liftI2 #-}

-- instance Bind Dim where
--   Dim a b >>- f = Dim a' b' where
--     Dim a' _ = f a
--     Dim _ b' = f b
--   {-# INLINE (>>-) #-}

-- instance Monad Dim where
--   return a = Dim a a
--   {-# INLINE return #-}
--   Dim a b >>= f = Dim a' b' where
--     Dim a' _ = f a
--     Dim _ b' = f b
--   {-# INLINE (>>=) #-}

instance Num a => Num (Dim a) where
  (+) = liftA2 (+)
  {-# INLINE (+) #-}
  (-) = liftA2 (-)
  {-# INLINE (-) #-}
  (*) = liftA2 (*)
  {-# INLINE (*) #-}
  negate = fmap negate
  {-# INLINE negate #-}
  abs = fmap abs
  {-# INLINE abs #-}
  signum = fmap signum
  {-# INLINE signum #-}
  fromInteger = pure Prelude.. fromInteger
  {-# INLINE fromInteger #-}

instance Fractional a => Fractional (Dim a) where
  recip = fmap recip
  {-# INLINE recip #-}
  (/) = liftA2 (/)
  {-# INLINE (/) #-}
  fromRational = pure Prelude.. fromRational
  {-# INLINE fromRational #-}

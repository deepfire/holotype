{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UnicodeSyntax #-}

module Types
    (
    -- Core wires
      SimTime, SimWire

    -- Simulation
    , Sim(..)
    , onWorldInput

    -- Rendering
    , RenderContext(..)

    -- Model
    , Totality(..)
    , Selector(..)
    , Selection(..)
    , LayEng(..), Viewport(..), Boundary(..)
    , ViewArgs(..), MinSize(..), Granularity(..)
    -- , GraphLayEng(..), DagLayEng(..), TreeLayEng(..), SetLayEng(..)
    , View(..)

    -- LayEngs
    , SideGraph(..)
    , DownGraph(..)
    , TreeList(..)
    , TreeGrid(..)
    , TreeSpace(..)
    , Carousel(..)
    , Grid(..)
    , List(..)
    ) where

import Control.Wire hiding (Category)
import SDL (Scancode)

import Data.Hashable
import qualified Data.HashSet as HS

import Linear hiding (trace)
import Linear.Affine


-- Imports for DimS | DimP
import GHC.Generics                (Generic)
import Data.Data                   (Data, Typeable)
import Data.Functor.Apply          ((<.>))
import Data.Semigroup.Foldable     (Foldable1, foldMap1)
import Data.Semigroup.Traversable  (Traversable1, traverse1)


-- | Simulation
type SimTime     = (Timed NominalDiffTime ())
type SimWire a b = Wire SimTime String IO a b

class Sim w i s | w → i, i → w, w → s where
    inputsOf        ∷ w → i
    sceneOf         ∷ w → s
    nextWorld       ∷ i → s → w
    --
    trackKeyDown    ∷ i → SDL.Scancode → i
    trackKeyUp      ∷ i → SDL.Scancode → i
    someKeyDown     ∷ i → Bool
    keyDown         ∷ SDL.Scancode → i → Bool

onWorldInput ∷ Sim w i s ⇒ SimWire i i → SimWire w w
onWorldInput wire = proc w → do
                      outs ← wire -< inputsOf w
                      returnA -< nextWorld outs (sceneOf w)


-- | Model
newtype Totality
    = Totality ()

class ElementAPI e where

-- data Element = ∀ e . ElementAPI e ⇒ Element e -- equivalent to the below:
data Element where
    Element ∷ (ElementAPI e, Hashable e) ⇒ e → Element
instance (Hashable Element) where
    hashWithSalt s (Element e)  = s `hashWithSalt` (hash e)

class Category a where
    data Selector  a ∷ *
    data Selection a ∷ *
    data View      a ∷ *
    select           ∷ Category a ⇒ Totality → Selector a → Selection a

data Graph
instance Category Graph where
    data Selector  Graph = GraphSelector  
    data Selection Graph = GraphSelection 
    data View      Graph = GraphView      

data Dag
instance Category Dag where
    data Selector  Dag   = DagSelector    
    data Selection Dag   = DagSelection   
    data View      Dag   = DagView        

data Tree
instance Category Tree where
    data Selector  Tree  = TreeSelector   
    data Selection Tree  = TreeSelection  
    data View      Tree  = TreeView       

data Set
instance Category Set where
    data Selector  Set   = SetSelector    String
    data Selection Set   = SetSelection   [Element]
    data View      Set   = SetView        [Element]
    select _ (SetSelector str) =
        SetSelection $ [Element $ StringElt str]


-- | Position
newtype Granularity = Granularity  Int                   deriving (Num, Show)
newtype MinSize     = MinSize      Double                deriving (Num, Show)
newtype ViewArgs    = ViewArgs    (Granularity, MinSize) deriving (Show)
newtype Scale       = Scale        Double                deriving (Eq, Num)
newtype Posn        = Posn        (V2 Double)            deriving (Eq, Num)
newtype Aspect      = Aspect       Double                deriving (Eq, Num, Floating, Fractional, Ord, Real, RealFrac, RealFloat)
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


-- | Layout engine
class Category cat ⇒ LayEng cat leng where
    data Viewport  leng ∷ *
    data Boundary  leng ∷ *
    data Layout    leng ∷ *
    data Ephemeral leng ∷ *
    cullSelection       ∷ leng → Selection cat → ViewArgs → Viewport leng → (View cat, Boundary leng)
    layout              ∷ leng → (View cat, Boundary leng) → (Layout leng, Ephemeral leng)
    render              ∷ RenderContext ren ⇒ ren → (View cat, Boundary leng) → (Layout leng, Ephemeral leng) → IO ()


-- | Visualisation
class RenderContext a where
    toPixels ∷ a → Posn → Dim Double → (V2 Int, V2 Int)


-- | Layout engine instances

--- Not sure what these add..
-- class LayEng Graph a ⇒ GraphLayEng a where
-- class LayEng Dag   a ⇒   DagLayEng a where
-- class LayEng Tree  a ⇒  TreeLayEng a where
-- class LayEng Set   a ⇒   SetLayEng a where

-- | Graph, viewed from aside (Z axis)
data SideGraph
instance LayEng Graph SideGraph where

-- | Graph, arrow aligned weighted display partitioning
data DownGraph
instance LayEng Graph DownGraph where

-- | List entries, vertical scrolling
data TreeList
instance LayEng Tree TreeList where

-- | Icon grid, vertical scrolling
data TreeGrid
instance LayEng Tree TreeGrid where

-- | Z-oriented space partitioning, ala /Lamdu/, vertical scrolling
data TreeSpace
instance LayEng Tree TreeSpace where

sublis ∷ Int → Int → [a] → [a]
sublis from upto = take (upto - from) Prelude.. drop from

-- | A finite/infinite carousel with parallax
data Carousel = Carousel
instance LayEng Set Carousel where
    data Viewport  Carousel = CarouselPort Int
    data Boundary  Carousel = CarouselBoundary
    data Layout    Carousel = CarouselLayout
    data Ephemeral Carousel = CarouselEphemeral
    cullSelection Carousel (SetSelection xs) (ViewArgs (gran, mins)) (CarouselPort n) =
        -- XXX: we need some way to turn GRAN and MINS into LIMIT
        let limit = 5                  -- must be odd!
            arm   = quot (limit - 1) 2 -- viewable, on either side
            got   = length xs
        in if
            -- can show in full?
            | limit > got    → (SetView xs,
                                CarouselBoundary)
            -- left arm crosses limit?
            | n - arm < 0    → (SetView undefined,
                                CarouselBoundary)
            -- right arm crosses limit?
            | n + arm >= got → (SetView undefined,
                                CarouselBoundary)
            -- convenient case -- taking from middle
            | otherwise      → (SetView (sublis (n - arm) (n + arm + 1) xs),
                                CarouselBoundary)
    layout Carousel (SetView xs, CarouselBoundary) =
        (CarouselLayout, CarouselEphemeral)
    -- render rctx
    --     ∷ RenderContext ren ⇒ ren → (View cat, Boundary leng) → (Layout leng, Ephemeral leng) → IO ()

-- | Yay grids
data Grid
instance LayEng Set Grid where

-- | Yay lists
data List
instance LayEng Set List where


-- An attempt at use..
newtype FullTextQuery = FullTextQuery String
newtype StringElt = StringElt String deriving (Hashable)
instance ElementAPI StringElt where

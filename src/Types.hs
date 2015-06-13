{-# LANGUAGE Arrows #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
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

    -- Model
    , Totality(..)
    , Selector(..)
    , Selection(..)
    , Layout(..), Viewport(..), Boundary(..)
    , ViewArgs(..), MinSize(..), Granularity(..)
    , GraphLayout(..), DagLayout(..), TreeLayout(..), SetLayout(..)
    , View(..)

    -- Layouts
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
    data Selection Set   = SetSelection   (HS.HashSet Element)
    data View      Set   = SetView        (HS.HashSet Element)
    select _ (SetSelector str) =
        SetSelection $ HS.singleton $ Element $ StringElt str


-- | View
newtype Granularity = Granularity Int    deriving (Num, Show)
newtype MinSize     = MinSize     Double deriving (Num, Show)
newtype ViewArgs    = ViewArgs (Granularity, MinSize)


-- | Layouts
class Category cat ⇒ Layout cat l where
    data Viewport l ∷ *
    data Boundary l ∷ *
    cullSelection   ∷ l → Selection cat → ViewArgs → Viewport l → (View cat, Boundary a)

class Layout Graph a ⇒ GraphLayout a where
class Layout Dag   a ⇒   DagLayout a where
class Layout Tree  a ⇒  TreeLayout a where
class Layout Set   a ⇒   SetLayout a where

-- | Graph, viewed from aside (Z axis)
data SideGraph
instance Layout Graph SideGraph where

-- | Graph, arrow aligned weighted display partitioning
data DownGraph
instance Layout Graph DownGraph where

-- | List entries, vertical scrolling
data TreeList
instance Layout Tree TreeList where

-- | Icon grid, vertical scrolling
data TreeGrid
instance Layout Tree TreeGrid where

-- | Z-oriented space partitioning, ala /Lamdu/, vertical scrolling
data TreeSpace
instance Layout Tree TreeSpace where

-- | A finite/infinite carousel with parallax
data Carousel
instance Layout Set Carousel where
    data Viewport Carousel = CarouselPort Int
    data Boundary Carousel = CarouselBoundary

-- | Yay grids
data Grid
instance Layout Set Grid where

-- | Yay lists
data List
instance Layout Set List where


-- An attempt at use..
newtype FullTextQuery = FullTextQuery String
newtype StringElt = StringElt String deriving (Hashable)
instance ElementAPI StringElt where

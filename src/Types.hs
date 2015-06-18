{-# LANGUAGE Arrows #-}
-- {-# LANGUAGE DeriveAnyClass #-} -- this breaks deriving Num for trivial newtypes
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

module Types
    (
    -- Simulation
      SimTime, SimWire
    , Sim(..)

    -- Model
    , Totality(..)
    , Controls(..), initialControls
    , Selector(..)
    , Selection(..)
    , Engi(..), Viewport(..), Boundary(..)
    , ViewArgs(..), MinSize(..), Granularity(..)
    , View(..)

    -- Rendering
    , RenderContext(..)

    -- Engis
    , SideGraph(..)
    , DownGraph(..)
    , DagList(..)
    , DagGrid(..)
    , DagSpace(..)
    , Carousel(..)
    , Grid(..)
    , List(..)
    ) where

import Control.Monad (forM_)
import Control.Wire hiding (Category)
import qualified SDL

import GHC.Prim (Constraint)
import Data.Hashable
-- import qualified Data.HashSet      as HS
-- import qualified Data.HashMap.Lazy as HM

import Linear hiding (trace)
import Linear.Affine

import Text.Printf (printf)


-- Local imports
import Ground


-- | Simulation
type SimTime     = (Timed NominalDiffTime ())
type SimWire a b = Wire SimTime String IO a b

class Sim w i | w → i, i → w where
    inputsOf        ∷ w → i
    --
    trackKeyDown    ∷ i → SDL.Scancode → i
    trackKeyUp      ∷ i → SDL.Scancode → i
    someKeyDown     ∷ i → Bool
    keyDown         ∷ SDL.Scancode → i → Bool


-- | Model
newtype Totality
    = Totality [String]

class ElementAPI e where

-- data Element = ∀ e . ElementAPI e ⇒ Element e -- equivalent to the below:
data Element where
    Element ∷ (ElementAPI e, Hashable e) ⇒ e → Element
instance (Hashable Element) where
    hashWithSalt s (Element e)  = s `hashWithSalt` (hash e)

type family   C a :: Constraint
type instance C a = (Eq a, Show a)
class C (EngiName a) ⇒ Category a where
    data Selector   a ∷ *
    data Selection  a ∷ *
    data Focus      a ∷ *
    data View       a ∷ *
    data EngiName   a ∷ *
    -- emptySelector     ∷ Selector a
    select            ∷ Category a ⇒ Totality → Selector a → Selection a
data EPEntry where
    EPEntry ∷ Category a ⇒ (a, (EngiName a)) → EPEntry

data EngiPref where
    EngiPref ∷ [EPEntry] → EngiPref

data Controls where
    Controls ∷ Category cat ⇒ {
      cSelector    ∷ Selector cat
    , cEngiPref    ∷ EngiPref
    , cGranularity ∷ Granularity
    , cMinSize     ∷ MinSize
    , cFocus       ∷ Maybe (Focus cat)
    } → Controls

initialControls ∷ Controls
initialControls = Controls {
                    cSelector    = SetSelector ""
                  , cEngiPref    = EngiPref [ EPEntry (Graph, SideGraph)
                                            , EPEntry (Dag,   SideDag)
                                            , EPEntry (Set,   Carousel)]
                  , cGranularity = Granularity 3
                  , cMinSize     = MinSize 0.03
                  , cFocus       = Nothing
                  }


-- | Query system instances

newtype FullTextQuery = FullTextQuery String

newtype StringElt = StringElt String deriving (Hashable)
instance ElementAPI StringElt where

data Graph = Graph
instance Category Graph where
    data Selector   Graph = GraphSelector  
    data Selection  Graph = GraphSelection 
    data View       Graph = GraphView
    data EngiName   Graph = SideGraph | DownGraph deriving (Eq, Show)
    data Focus      Graph = GraphFocus Element

data Dag = Dag
instance Category Dag where
    data Selector   Dag   = DagSelector    
    data Selection  Dag   = DagSelection   
    data View       Dag   = DagView        
    data EngiName   Dag   = SideDag | DagList | DagGrid | DagSpace deriving (Eq, Show)
    data Focus      Dag   = DagFocus Element

data Set = Set
instance Category Set where
    data Selector   Set   = SetSelector    String
    data Selection  Set   = SetSelection   [Element]
    data View       Set   = SetView        [Element]
    data EngiName   Set   = Carousel | Grid | List deriving (Eq, Show)
    data Focus      Set   = SetFocus Element
    select _ (SetSelector str) =
        SetSelection $ [Element $ StringElt str]


-- | Position
newtype Granularity = Granularity  Int                   deriving (Num, Show)
newtype MinSize     = MinSize      Double                deriving (Num, Show)
newtype ViewArgs    = ViewArgs    (Granularity, MinSize) deriving (Show)


-- | Interaction
class InputSys a where



class Category cat ⇒ Engi cat eng where
    data Viewport  eng ∷ *
    data Boundary  eng ∷ *
    data Layout    eng ∷ *
    data Ephemeral eng ∷ *
    cullSelection      ∷ eng → Selection cat → ViewArgs → Viewport eng → (View cat, Boundary eng)
    layout             ∷ eng → (View cat, Boundary eng) → (Layout eng, Ephemeral eng)
    render             ∷ RenderContext ren ⇒ ren → (View cat, Boundary eng) → (Layout eng, Ephemeral eng) → IO ()
    interact           ∷ InputSys is ⇒ is → (View cat, Boundary eng) → Controls → Controls


-- | Visualisation
class RenderContext a where
    toPixels ∷ a → Posn → Dim Double → (V2 Integer, V2 Integer)
    drawRect ∷ a → Posn → Dim Double → IO ()


-- | UI backend instances
data SDLInput
    = SDLInput

instance InputSys SDLInput where

data SDLRenderer
    = SDLRenderer Aspect (V2 Double) SDL.Renderer

instance RenderContext SDLRenderer where
    toPixels (SDLRenderer aspect screen@(V2 scrW scrH) _) (Posn posn) size =
        ( fmap floor $ screen * posn
        , fmap floor $ case size of
                         DimS scrSize          → screen * scrSize
                         DimP (V2 propW propH) → V2 (scrW * propW) (scrH * propH * (realToFrac aspect)))
    drawRect r@(SDLRenderer _ _ rend) posn dim =
        do
          let (pixdim, pixpos) = toPixels r posn dim
          SDL.renderDrawRect rend $ SDL.Rectangle (P $ fmap fromIntegral pixpos) (fmap fromIntegral pixdim)


-- | Layout engine instances

-- | Graph, viewed from aside (Z axis)
data SideGraph
instance Engi Graph SideGraph where

-- | Graph, arrow aligned weighted display partitioning
data DownGraph
instance Engi Graph DownGraph where

-- | List entries, vertical scrolling
data DagList
instance Engi Dag DagList where

-- | Icon grid, vertical scrolling
data DagGrid
instance Engi Dag DagGrid where

-- | Z-oriented space partitioning, ala /Lamdu/, vertical scrolling
data DagSpace
instance Engi Dag DagSpace where

sublis ∷ Int → Int → [a] → [a]
sublis from upto = take (upto - from) Prelude.. drop from

-- (x - ox)² / a² + (y - oy)² / b² = 1
-- y = (1 - (x - ox)² / a²)^1/2 * b + oy
ellipse ∷ Posn → Dim Double → Double → Double
ellipse (Posn (V2 ox oy)) (DimS (V2 a b)) x = sqrt (1 - ((x - ox) / a) ** 2) * b + oy


-- | A finite/infinite carousel with parallax
data Carousel =
    CarouselEngi {
      carouselLooped ∷ Bool
    }

instance Engi Set Carousel where
    data Viewport  Carousel = CarouselPort Int
    data Boundary  Carousel = CarouselBoundary
    data Layout    Carousel = CarouselLayout [(Posn, Scale)]
    data Ephemeral Carousel = CarouselEphemeral
    cullSelection (CarouselEngi {..}) (SetSelection xs) (ViewArgs (gran, mins)) (CarouselPort n) =
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
    layout (CarouselEngi {..}) (SetView xs, CarouselBoundary) =
        ( CarouselLayout $ let got            = length xs
                               ellipse_width  = 0.7 ∷ Double
                               ellipse_height = 0.5 ∷ Double
                               (ox, oy)       = (0.5, 0.5)
                               (a, b)         = (ellipse_width / 2, ellipse_height / 2)
                               step           = ellipse_width / fromIntegral (got - 1)
                               ell            = ellipse (Posn (V2 ox oy)) (DimS (V2 a b))
                           in [ let y = ell x
                                in (Posn (V2 x y), Scale y)
                              -- The fraction subtraction is to avoid the range stepping beyond A, and so a NaN
                              | x ← [ox - a, ox - a + step .. ox + a - 0.0001] ]
        , CarouselEphemeral)
    render ctx (SetView xs, _) (CarouselLayout xposs, CarouselEphemeral) =
        do printf "-- frame start --\n"
           forM_ xposs $ \(pos, scale) →
               do printf "#<box %s-%s>   " (show pos) (show scale)
           printf "-- frame end --\n"

-- | Yay grids
data Grid
instance Engi Set Grid where

-- | Yay lists
data List
instance Engi Set List where

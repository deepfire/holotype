{-# LANGUAGE Arrows #-}
-- {-# LANGUAGE DeriveAnyClass #-} -- this breaks deriving Num for trivial newtypes
{-# LANGUAGE DataKinds #-}
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
    , ElementAPI(..), Element(..)
    , Category(..), EngiName(..), Engine(..)
    , Graph(..), Dag(..), Set(..)
    , Controls(..), initial_controls
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

import           Control.Monad (forM_)
import           Control.Wire hiding (Category)
import qualified SDL

import           GHC.Prim (Constraint)
import           Data.Hashable
import qualified Data.HashSet as HS
import qualified Data.HashMap.Lazy as HM
import           Data.List (find)

import qualified Data.Time.Clock as Time

import           Linear hiding (trace)
import           Linear.Affine

import           Text.Printf (printf)

import           Data.Typeable (cast)


-- Local imports
import           Ground


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

data CatName
    = Graph
    | Dag
    | Set
    deriving (Eq)

type family   C a ∷ Constraint
type instance C a = (Eq a, Show a)
class C (EngiName a) ⇒ Category (a ∷ CatName) where
    data Selector   a ∷ *
    data Selection  a ∷ *
    data Focus      a ∷ *
    data View       a ∷ *
    data EngiName   a ∷ *
    select            ∷ Totality → Selector a → Selection a
    -- elect_engi        ∷ a → EngiPref → Maybe (EngiName a) → Engine
    validity_limit    ∷ Selection a → Maybe Time.NominalDiffTime

prefer_engi ∷ Category c ⇒ c → EngiPref → EngiName c → EngiName c
prefer_engi cat (EngiPref xs) defname =
    case find (\(EPEntry (icat, ename)) → cat == icat) xs of
      Just (EPEntry (icat, ename)) → ename
      Nothing                      → defname

-- XXX:  extremely lousy!
-- x = elect_engi Set (EngiPref []) Nothing 

data EngiPref where
    EngiPref ∷ [EPEntry] → EngiPref
data EPEntry where
    EPEntry ∷ Category a ⇒ a → EngiName a → EPEntry

-- Q: Why do we need our layouts be a typeclass?
-- A: 1. Typeclass separation is a nice way to have methods.
--    2. We get associated type families
--      → probably a reason to look into regular type families

data Controls where
    Controls ∷ Category cat ⇒ {
      cSelector    ∷ Selector cat
    , cEngiPref    ∷ EngiPref
    , cGranularity ∷ Granularity
    , cMinSize     ∷ MinSize
    , cFocus       ∷ Maybe (Focus cat)
    } → Controls

initial_controls ∷ Controls
initial_controls = Controls {
                     cSelector    = SetSelector ""
                   , cEngiPref    = EngiPref [ EPEntry (Graph, SideGraphEN)
                                             , EPEntry (Dag,   SideDagEN)
                                             , EPEntry (Set,   CarouselEN)]
                   , cGranularity = Granularity 3
                   , cMinSize     = MinSize 0.03
                   , cFocus       = Nothing
                   }


-- | Query system instances

newtype FullTextQuery = FullTextQuery String

newtype StringElt = StringElt String deriving (Hashable)
instance ElementAPI StringElt where

instance Category 'Graph where
    data Selector   'Graph = GraphSelector  
    data Selection  'Graph = GraphSelection 
    data View       'Graph = GraphView
    data EngiName   'Graph = SideGraphEN | DownGraphEN deriving (Eq, Show)
    data Focus      'Graph = GraphFocus Element

instance Category 'Dag where
    data Selector   'Dag   = DagSelector    
    data Selection  'Dag   = DagSelection   
    data View       'Dag   = DagView        
    data EngiName   'Dag   = SideDagEN | DagListEN | DagGridEN | DagSpaceEN deriving (Eq, Show)
    data Focus      'Dag   = DagFocus Element

instance Category 'Set where
    data Selector   'Set   = SetSelector    String
    data Selection  'Set   = SetSelection   [Element]
    data View       'Set   = SetView        [Element]
    data EngiName   'Set   = CarouselEN | GridEN | EListEN deriving (Eq, Show)
    data Focus      'Set   = SetFocus Element
    select _ (SetSelector str) =
        SetSelection $ [Element $ StringElt str]
    -- elect_engi cat (EngiPref xs) mname =
    --     Engine cat (let name = case mname of
    --                              Just n  → n
    --                              -- Nothing → 
    --                 in undefined)


-- | Position
newtype Granularity = Granularity  Int                   deriving (Num, Show)
newtype MinSize     = MinSize      Double                deriving (Num, Show)
newtype ViewArgs    = ViewArgs    (Granularity, MinSize) deriving (Show)


-- | Interaction
class InputSys a where


-- | Layout engine
class Category cat ⇒ Engi (cat ∷ CatName) eng | eng → cat where
    data Cull      eng ∷ *
    data Viewport  eng ∷ *
    data Boundary  eng ∷ *
    data Layout    eng ∷ *
    data Ephemeral eng ∷ *
    erect_engi         ∷ EngiName cat → eng → Engine
    compute_cull       ∷ eng → (Granularity, MinSize) → Cull eng
    place_viewport     ∷ eng → Selection cat → Focus cat → Cull eng → Viewport eng
    cull_selection     ∷ eng → Selection cat → ViewArgs → Viewport eng → (View cat, Boundary eng)
    layout             ∷ eng → (View cat, Boundary eng) → (Layout eng, Ephemeral eng)
    render             ∷ RenderContext ren ⇒ ren → (View cat, Boundary eng) → (Layout eng, Ephemeral eng) → IO ()
    interact           ∷ InputSys is ⇒ is → (View cat, Boundary eng) → Controls → Controls

data Engine where
    Engine ∷ -- Engi c e ⇒ (c ∷ CatName) → e → 
             Engine

-- erect_engi ∷ Engi a b ⇒ EngiPref → Maybe (EngiName a) → Engine
-- erect_engi (EngiPref xs) mname =
--     case


-- | Visualisation
class RenderContext a where
    to_pixels ∷ a → Posn → Dim Double → (V2 Integer, V2 Integer)
    draw_rect ∷ a → Posn → Dim Double → IO ()


-- | UI backend instances
data SDLInput
    = SDLInput

instance InputSys SDLInput where

data SDLRenderer
    = SDLRenderer Aspect (V2 Double) SDL.Renderer

instance RenderContext SDLRenderer where
    to_pixels (SDLRenderer aspect screen@(V2 scrW scrH) _) (Posn posn) size =
        ( fmap floor $ screen * posn
        , fmap floor $ case size of
                         DimS scrSize          → screen * scrSize
                         DimP (V2 propW propH) → V2 (scrW * propW) (scrH * propH * (realToFrac aspect)))
    draw_rect r@(SDLRenderer _ _ rend) posn dim =
        do
          let (pixdim, pixpos) = to_pixels r posn dim
          SDL.renderDrawRect rend $ SDL.Rectangle (P $ fmap fromIntegral pixpos) (fmap fromIntegral pixdim)


-- | Layout engine instances

-- | Graph, viewed from aside (Z axis)
data SideGraph
instance Engi 'Graph SideGraph where

-- | Graph, arrow aligned weighted display partitioning
data DownGraph
instance Engi 'Graph DownGraph where

-- | List entries, vertical scrolling
data DagList
instance Engi 'Dag DagList where

-- | Icon grid, vertical scrolling
data DagGrid
instance Engi 'Dag DagGrid where

-- | Z-oriented space partitioning, ala /Lamdu/, vertical scrolling
data DagSpace
instance Engi 'Dag DagSpace where

sublis ∷ Int → Int → [a] → [a]
sublis from upto = take (upto - from) Prelude.. drop from

-- (x - ox)² / a² + (y - oy)² / b² = 1
-- y = (1 - (x - ox)² / a²)^1/2 * b + oy
ellipse ∷ Posn → Dim Double → Double → Double
ellipse (Posn (V2 ox oy)) (DimS (V2 a b)) x = sqrt (1 - ((x - ox) / a) ** 2) * b + oy


-- | A finite/infinite carousel with parallax
data Carousel = CarouselEngi

instance Engi 'Set Carousel where
    data Viewport  Carousel = CarouselPort Int
    data Boundary  Carousel = CarouselBoundary
    data Layout    Carousel = CarouselLayout [(Posn, Scale)]
    data Ephemeral Carousel = CarouselEphemeral
    cull_selection (CarouselEngi) (SetSelection xs) (ViewArgs (gran, mins)) (CarouselPort n) =
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
    layout (CarouselEngi) (SetView xs, CarouselBoundary) =
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
instance Engi 'Set Grid where

-- | Yay lists
data List
instance Engi 'Set List where

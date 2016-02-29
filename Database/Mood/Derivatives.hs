{-# LANGUAGE Arrows #-}
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
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE UnicodeSyntax #-}

module Database.Mood.Derivatives
    (
    -- Simulation
      SimTime, SimWire
    , Sim(..)
     )
where


-- Base imports


-- External imports
import           Control.Wire hiding (Category)

import qualified SDL


-- Local imports
import Database.Mood.Types


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



-- | 'StructureName' enumerates a closed universe of types and associated literals.
data StructureName
    -- = Graph
    -- | Dag
    = Set
    deriving (Eq)

type family   C c ∷ Constraint
type instance C c = (Eq c, Show c)

-- | Structure
class C (PresName sn) ⇒ Structure (sn ∷ StructureName) where
    data Selector   sn ∷ *
    data Selection  sn ∷ *
    data Focus      sn ∷ *
    data View       sn ∷ *
    data PresName   sn ∷ *
    select             ∷ SourceAPI a ⇒ a → Selector sn → Selection sn
    -- elect_presenter    ∷ StructureName → EngiPref → Maybe (PresName sn) → Engine
    validity_limit     ∷ Selection sn → Maybe Time.NominalDiffTime

prefer_engi ∷ Structure sn ⇒ StructureName → EngiPref → PresName sn → PresName sn
prefer_engi provided_cname (EngiPref xs) defname =
    (⊥)
    -- case find (\(EPEntry cname _) → cname == provided_cname) xs of
    --   Just (EPEntry cname ename) → ename
    --   Nothing                    → defname

-- XXX:  extremely lousy!
-- x = elect_engi Set (EngiPref []) Nothing 

data EngiPref where
    EngiPref ∷ [EPEntry] → EngiPref
data EPEntry where
    EPEntry ∷ Structure sn ⇒ StructureName → PresName sn → EPEntry

-- Q: Why do we need our layouts be a typeclass?
-- A: 1. Typeclass separation is a nice way to have methods.
--    2. We get associated type families
--      → probably a reason to look into regular type families

data Controls where
    Controls ∷ Structure sn ⇒ {
      cSelector    ∷ Selector sn
    , cEngiPref    ∷ EngiPref
    , cGranularity ∷ Granularity
    , cMinSize     ∷ MinSize
    , cFocus       ∷ Maybe (Focus sn)
    } → Controls

initial_controls ∷ Controls
initial_controls = Controls {
                     cSelector    = SetSelector ""
                   -- , cEngiPref    = EngiPref [ EPEntry (Graph, SideGraphEN)
                   --                           , EPEntry (Dag,   SideDagEN)
                   --                           , EPEntry (Set,   CarouselEN)]
                   , cGranularity = Granularity 3
                   , cMinSize     = MinSize 0.03
                   , cFocus       = Nothing
                   }


-- | Query system instances

newtype StringElt = StringElt String deriving (Hashable)
instance DatumAPI StringElt where

-- instance StructureAPI 'Graph where
--     data Selector   'Graph = GraphSelector  
--     data Selection  'Graph = GraphSelection 
--     data View       'Graph = GraphView
--     data PresName   'Graph = SideGraphEN | DownGraphEN deriving (Eq, Show)
--     data Focus      'Graph = GraphFocus Datum

-- instance StructureAPI 'Dag where
--     data Selector   'Dag   = DagSelector    
--     data Selection  'Dag   = DagSelection   
--     data View       'Dag   = DagView        
--     data PresName   'Dag   = SideDagEN | DagListEN | DagGridEN | DagSpaceEN deriving (Eq, Show)
--     data Focus      'Dag   = DagFocus Datum

instance Structure 'Set where
    data Selector   'Set   =                        SetSelector    String
    data Selection  'Set   = ∀ dat . DatumAPI dat ⇒ SetSelection   [dat]
    data View       'Set   = ∀ dat . DatumAPI dat ⇒ SetView        [dat]
    data PresName   'Set   =                        CarouselEN | GridEN | EListEN deriving (Eq, Show)
    data Focus      'Set   = ∀ dat . DatumAPI dat ⇒ SetFocus dat
    select _ (SetSelector str) =
        SetSelection $ [Datum $ StringElt str]
    -- elect_engi st (EngiPref xs) mname =
    --     (⊥)
        -- Engine st (let name = case mname of
        --                          Just n  → n
        --                          -- Nothing → 
        --             in undefined)


-- | Position
newtype Granularity = Granularity  Int                   deriving (Num, Show)
newtype MinSize     = MinSize      Double                deriving (Num, Show)
newtype ViewArgs    = ViewArgs    (Granularity, MinSize) deriving (Show)


-- | Interaction
class InputSys a where


-- | Layout engine
class Structure sn ⇒ PresenterAPI (sn ∷ StructureName) pres | pres → sn where
    data Cull      pres ∷ *
    data Viewport  pres ∷ *
    data Boundary  pres ∷ *
    data Layout    pres ∷ *
    data Ephemeral pres ∷ *
    erect_engi          ∷ PresName sn → pres → Engine
    compute_cull        ∷ pres → (Granularity, MinSize) → Cull pres
    place_viewport      ∷ pres → Selection sn → Focus sn → Cull pres → Viewport pres
    cull_selection      ∷ pres → Selection sn → ViewArgs → Viewport pres → (View st, Boundary eng)
    layout              ∷ pres → (View st, Boundary pres) → (Layout pres, Ephemeral pres)
    render              ∷ RenderContext ren ⇒ ren → (View sn, Boundary pres) → (Layout pres, Ephemeral pres) → IO ()
    interact            ∷ InputSys is ⇒ is → (View sn, Boundary pres) → Controls → Controls

data Engine where
    Engine ∷ -- Engi c e ⇒ (c ∷ StructureName) → e → 
             Engine

-- erect_engi ∷ Engi a b ⇒ EngiPref → Maybe (PresName a) → Engine
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
        let (pixdim, pixpos) = to_pixels r posn dim
        in SDL.drawRect rend $ Just $ SDL.Rectangle (P $ fmap fromIntegral pixpos) (fmap fromIntegral pixdim)


-- | Layout engine instances

-- | Graph, viewed from aside (Z axis)
-- data SideGraph
-- instance Engi 'Graph SideGraph where

-- | Graph, arrow aligned weighted display partitioning
-- data DownGraph
-- instance Engi 'Graph DownGraph where

-- | List entries, vertical scrolling
-- data DagList
-- instance Engi 'Dag DagList where

-- | Icon grid, vertical scrolling
-- data DagGrid
-- instance Engi 'Dag DagGrid where

-- | Z-oriented space partitioning, ala /Lamdu/, vertical scrolling
-- data DagSpace
-- instance Engi 'Dag DagSpace where

sublis ∷ Int → Int → [a] → [a]
sublis from upto = take (upto - from) Prelude.. drop from

-- (x - ox)² / a² + (y - oy)² / b² = 1
-- y = (1 - (x - ox)² / a²)^1/2 * b + oy
ellipse ∷ Posn → Dim Double → Double → Double
ellipse (Posn (V2 ox oy)) (DimS (V2 a b)) x = sqrt (1 - ((x - ox) / a) ** 2) * b + oy


-- | A finite/infinite carousel with parallax
data Carousel = CarouselEngi

instance PresenterAPI 'Set Carousel where
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
instance PresenterAPI 'Set Grid where

-- | Yay lists
data List
instance PresenterAPI 'Set List where

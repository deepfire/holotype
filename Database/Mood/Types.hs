{-# LANGUAGE DeriveGeneric #-}
{-|

  Module      : Database.Mood.Types
  Description : Type backbone of the Mood data modeling system.
  Copyright   : (c) Kosyrev Serge, 2015

  License     : AGPL-3
  Maintainer  : _deepfire@feelingofgreen.ru
  Stability   : experimental
  Portability : POSIX

  This is the type-level backbone of the Mood data modeling system.
-}

{-# LANGUAGE AllowAmbiguousTypes #-}
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

module Database.Mood.Types
    (
    -- * Introduction
    -- $intro

    -- * Abstraction of the data set
      SourceAPI(..), Query(..)

    -- * Data element
    , DatumAPI(..)
    , Datum(..)

    -- -- * Data representation
    , IKStructure(..), Structure(..)

    -- -- * Data view & interface
    , InterfaceAPI(..)
    , Presenter(..)
    -- , Graph(..) --, Dag(..), Set(..)
    -- , Controls(..), initial_controls
    -- , Selection(..)
    , Viewport(..)
    -- , Boundary(..)
    -- , ViewArgs(..), MinSize(..), Granularity(..)
    , View(..)

    -- -- * Rendering
    -- , RenderContext(..)

    -- -- Engis
    -- -- , SideGraph(..)
    -- -- , DownGraph(..)
    -- -- , DagList(..)
    -- -- , DagGrid(..)
    -- -- , DagSpace(..)
    -- , Carousel(..)
    -- , Grid(..)
    -- , List(..)
    ) where

{- $intro

= What

  /Mood/ is a textual/visual substrate for data manipulation.

= Where

  <https://github.com/deepfire/mood>

= Screenshot

  <<https://github.com/deepfire/mood pic or didn't happen>>

= Vocabulary

  [@Source@]
  An abstraction of an entity that can be /queried/ for /datums/.

  Examples:
      - search engine
      - file system
      - project documentation

  [@Query@]
  A request towards /source/ that yields /datums/.

  Comes as twin forms:
      [@Generic query@]
      A common query language that /Mood/ presents to the user.

      [@Source-specific query@]
      A necessary adaptation of the generic query to a specific source.

  [@Datum@]
  An atomic piece of data from a /data source/.

  [@Structure@]
  Currently one of 'Dag', 'Graph' or 'Set'.

  A structure capturing a particular, query-specific subset of relationships
  between /datums/, that exists within the /source/.

  [@Selection@] …
  [@Engine@] …
  [@Viewport@] …
  [@View@] …
  [@Layout@] …

= Visualisation pipeline

  Mood contains a model of data visualisation -- a pipeline:

    - data /source/ -- /a __temporary__ crude abstraction/ (via 'Source')
    - selection (via 'Selector', 'Selection')
    - layout engine choice (via 'PresName')
    - visibility constraint computation
    - viewport positioning
    - viewport cliping
    - layout (@some monospace text@)
    - rendering

= TODO

  1. Validate against <https://wiki.haskell.org/How_to_write_a_Haskell_program>.

= Unused Haddock reference material

@
  f x = x + x
@

> g x = x * 42

>>> fib 10
55

prop> a + b = b + a

 A supplemental "Ground" module is used.

-}


-- Base imports
import           Control.Monad            (forM_)
import           Data.Kind
import           Data.List                (find, intersect)

import           Data.Typeable            (cast)
import           Prelude.Unicode
import           Text.Printf              (printf)

import           GHC.Exts (Constraint)
import           GHC.Generics (Generic(..))


-- External imports
import           Control.Wire hiding (Structure)

import           Data.Data
import           Data.Hashable
import qualified Data.HashSet      as HS
import qualified Data.HashMap.Lazy as HM

import qualified SDL

import           Linear hiding (trace)
import           Linear.Affine


-- Local imports
import           Database.Mood.Ground



{-|  Generic query.

'Source'-agnostic query language. -}
data Query
    = FullTextQuery String
--  | ClosureQuery Op [Datum…]
--  | ???


{-|  Structure.

A closed universe of possible data representations:
 - 'Graph'
 - 'Dag'
 - 'Tree'
 - 'List'
 - 'Set' -}

data IKStructure = Graph | Dag | Tree | List | Set deriving (Eq, Show)
data Structure (struc ∷ IKStructure) where
    SGraph ∷ Structure Graph
    SDag   ∷ Structure Dag
    STree  ∷ Structure Tree
    SList  ∷ Structure List
    SSet   ∷ Structure Set
data WStruct where
    WStruct ∷ Structure struc → WStruct
instance Eq WStruct where
    (WStruct SGraph) == (WStruct SGraph) = True
    (WStruct SDag)   == (WStruct SDag)   = True
    (WStruct STree)  == (WStruct STree)  = True
    (WStruct SList)  == (WStruct SList)  = True
    (WStruct SSet)   == (WStruct SSet)   = True

class StructureAPI (struc ∷ IKStructure) where
    -- | Result.
    -- | Structure-specific datum positioning.
    data Result    (struc ∷ IKStructure) datum ∷ Type
    -- | Layout.
    -- | Structure-specific datum positioning.
    data Layout    (struc ∷ IKStructure) datum ∷ Type
    compute_structure_layout ∷ (DatumAPI datum)
                               ⇒ Result struc datum
                               → Layout struc datum


{-|  Source.

Data provider abstraction. -}
class DatumConstr (Datum src) ⇒ SourceAPI src where
    data Src                             src ∷ Type
    data SrcQuery  (struc ∷ IKStructure) src ∷ Type
    data Datum                           src ∷ Type
    -- | Given a query, return a list of possible structures,
    --   ordered from most to least representative.
    query_structures   ∷ ()
                         ⇒ Src src
                         → Query
                         → [WStruct]
    specialize_query   ∷ ()
                         ⇒ Structure struc
                         → Src src
                         → Query
                         → SrcQuery struc src
    run_query          ∷ (StructureAPI struc)
                         ⇒ Structure struc
                         → Src src
                         → SrcQuery struc src
                         → Result struc (Datum src)
{-*
1. ??? →
  'run_query' should be able to specify the result structure at the type level.
2. 1 →
   'Struc' needs to be parametrized by a type denoting the structure.
3. 2 →
   The result must be polymorphic on the type of the structure.
4. 3 →
   
-}

{-|  Clip.

'Structure'-specific visibility constraint specifier.

Is it also 'Presenter'-specific? -}
type family   ClipConstr c ∷ Constraint
type instance ClipConstr c = ClipAPI c

class ClipAPI clip where


{-|  Focus.

'Interface'-specific interaction focus marker -}
type family   FocusConstr c ∷ Constraint
type instance FocusConstr c = FocusAPI c

class FocusAPI clip where


-- | Viewport
type family   ViewportConstr c ∷ Constraint
type instance ViewportConstr c = ViewportAPI c

class ViewportAPI port where


-- | View
type family   ViewConstr c ∷ Constraint
type instance ViewConstr c = ViewAPI c

class ViewAPI view where


{-|  Interface.

Rendering substrate underlying the Presenter implementations. -}
class InterfaceAPI iface where
    data Interface                             iface        ∷ Type
    data Presenter       (struc ∷ IKStructure) iface        ∷ Type
    data PresenterConfig                       iface pres   ∷ Type
    data Clip            (struc ∷ IKStructure) iface        ∷ Type
    data Focus           (struc ∷ IKStructure) iface        ∷ Type
    data Viewport                              iface        ∷ Type
    data View                                  iface result ∷ Type
    presentable_strucs     ∷ ()
                             ⇒ Interface iface
                             → [WStruct]
    choose_presenter       ∷ ()
                             ⇒ Structure struc
                             → Interface iface
                             → Query
                             → Either String (Presenter struc iface)
    calculate_clip         ∷ (StructureAPI struc)
                             ⇒ Structure struc
                             → Presenter struc iface
                             → PresenterConfig iface (Presenter struc iface)
                             → Clip struc iface
    find_focus             ∷ (StructureAPI struc
                             ,FocusConstr (Focus struc iface)
                             ,DatumAPI datum)
                             ⇒ Structure struc
                             → Layout struc datum
                             → Maybe (Focus struc iface)
                             → Focus struc iface
    position_viewport      ∷ (StructureAPI struc
                             ,ViewportConstr (Viewport iface)
                             ,FocusConstr (Focus struc iface)
                             ,DatumAPI datum)
                             ⇒ Structure struc
                             → Clip struc iface
                             → Layout struc datum
                             → Viewport iface
                             → Focus struc iface
                             → Viewport iface
    compute_view           ∷ (StructureAPI struc
                             ,FocusConstr (Focus struc iface)
                             ,ViewConstr (View iface (Result struc (Datum src)))
                             ,DatumAPI datum)
                             ⇒ Structure struc
                             → Result struc (Datum src)
                             → Layout struc datum
                             → Viewport iface
                             → View iface (Result struc (Datum src))
    render_layout          ∷ (StructureAPI struc
                             ,FocusConstr (Focus struc iface)
                             ,ViewConstr (View iface (Result struc (Datum src)))
                             ,DatumAPI datum)
                             ⇒ Structure struc
                             → Presenter struc iface
                             → PresenterConfig iface (Presenter struc iface)
                             → Layout struc datum
                             → Viewport iface
                             → View iface (Result struc (Datum src))
                             → ()


decide_result_structure ∷ (InterfaceAPI iface, SourceAPI src, StructureAPI struc)
                          ⇒ Interface iface → Src src → Query → Either String WStruct
decide_result_structure iface src query =
    let inherent      = query_structures src query
        presentable   = presentable_strucs iface
    in case intersect inherent presentable of
         []     → Left $ printf "Error: no structure candidates: inherent %d, presentable %d, empty intersection." (length inherent) (length presentable)
         (x:xs) → Right x


-- | Datum
type family   DatumConstr c ∷ Constraint
type instance DatumConstr c = DatumAPI c

class Hashable e ⇒ DatumAPI e where



instance StructureAPI Graph where
    data Result Graph a = GraphR a
    data Layout Graph a = GraphL (Result Graph a)
    compute_structure_layout s@(GraphR _) =
        GraphL s



-- | Proof of existence of an implementation:  limited, type-level edition.
data Proof

instance SourceAPI Proof where
    data Src             Proof = ProofSrc [String]
    data SrcQuery  struc Proof = ProofQuery String
    data Datum           Proof = ProofDatum String deriving (Generic)
    query_structures (ProofSrc _) (FullTextQuery _) =
        [WStruct SGraph]
    specialize_query struc (ProofSrc _) (FullTextQuery str) =
        ProofQuery str
    run_query SGraph _ (ProofQuery str) =
        GraphR (ProofDatum str)

instance InterfaceAPI Proof where
    data Interface               Proof                         = ProofIface 
    data Presenter         Graph Proof                         = ProofGraphPres
    data PresenterConfig         Proof (Presenter Graph Proof) = ProofPresConfig
    data Clip              struc Proof                         = ProofClip
    data Focus             struc Proof                         = ProofFocus
    data Viewport                Proof                         = ProofViewport
    data View                    Proof (Result Graph a)        = ProofView
    presentable_strucs ProofIface        =
        [WStruct SGraph]
    choose_presenter SGraph ProofIface _ =
        Right ProofGraphPres
    calculate_clip SGraph ProofGraphPres ProofPresConfig =
        ProofClip
    find_focus SGraph (GraphL (GraphR _)) oldfocus =
        -- XXX: why would it change?
        case oldfocus of
          Nothing → ProofFocus
          Just _  → ProofFocus
    position_viewport SGraph ProofClip (GraphL (GraphR _)) ProofViewport ProofFocus =
        -- XXX: how do we account for user input that doesn't change focus?
        ProofViewport
    compute_view SGraph (GraphR _) (GraphL (GraphR _)) ProofViewport =
        ProofView
    render_layout SGraph ProofGraphPres ProofPresConfig (GraphL (GraphR _)) ProofViewport ProofView =
        ()

instance Hashable       (Datum Proof) where
instance DatumAPI       (Datum Proof) where
instance ViewportAPI (Viewport Proof) where

{-# LANGUAGE ConstraintKinds, DataKinds, KindSignatures, TypeApplications, TypeInType #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, InstanceSigs, MultiParamTypeClasses, NoMonomorphismRestriction, RankNTypes, TypeSynonymInstances, UndecidableInstances #-}
{-# LANGUAGE GADTs, FunctionalDependencies, TypeFamilies, TypeFamilyDependencies #-}
{-# LANGUAGE BangPatterns, LambdaCase, MultiWayIf, OverloadedStrings, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TypeOperators, UnicodeSyntax, ViewPatterns #-}
{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveGeneric, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Space where

-- Basis
import           Prelude                           hiding (putStrLn)
import           Prelude.Unicode

-- Type-level
import           GHC.Generics                             (Generic)
import           GHC.TypeLits
import           GHC.Types                         hiding (Constraint)

-- General types
import           Control.Applicative
import           Control.Applicative.Free
import           Control.Lens                      hiding (children)
import           Control.Monad.Random              hiding (lift, void)
import           Control.Monad.State               hiding (lift, void)
import           Data.Complex
import           Data.Foldable
import           Data.Function
import           Data.List
import           Data.Lub
import qualified Data.Map                          as Map
import           Data.Maybe
import           Data.Glb
import           Data.MonoTraversable
import           Data.Monoid
import qualified Data.Text                         as T
import           Data.Text                                (Text)
import qualified Data.Text.Lazy                    as TL
import           Data.Text.Lazy                           (toStrict)
import           Data.Text.IO
import qualified Data.Text.Lazy.IO                 as TLIO
import           Data.Type.Bool
import           Data.Void                                (Void)
import           Text.PrettyPrint.Leijen.Text      hiding ((<>), (<$>), space)

-- Algebra
import           Linear                            hiding (trace, unit)

-- glib-introspection -based Pango
import qualified GI.Pango                          as GIP (unitsToDouble, unitsFromDouble)

-- Misc
import           Debug.Trace                              (trace)
import           Data.Text.Format                         (format, Only(..))
import qualified Data.Text.Format                  as T

-- Dirty stuff
import qualified Foreign                           as F
import qualified System.IO.Unsafe                  as UN

-- Local
import           Elsewhere
import           Flatland
import           Flex


-- * Narrative
--
-- ** R, for design Requirements
--    1. We want to organize an simplified, constraint-solving workflow -- a 80% solution, indeed.
--    2. UI scaling is a concern we can't afford to ignore in 2017.
--    3. We want to encapsulate the solution of UI scaling concern inside the language of Spaces.
--
-- ** M, for iMplementation requirements
--    1. A free applicative is desired as the embodiment of ∈ent's visual layout, because:
--       - it lies in a sweet spot of expressivity, between Functor and Monad:
--         - entire tree can be analyzed prior to effect execution, unlike with Monad
--         - said analysis can be performed under multiple interpretations
--         - it provide a needed expressivitu boost over Functor
--       - it provides a solid abstraction, isolating layered types from each other
--    2. A GADT is needed to facilitate type-level propagation of information
--
-- ** C, for Immediate conclusions
--    1. R3 can't be resolved, unless object size requirements are scale/context sensitive.
--    2, M1 → a Functor instance is mandatory, by definition of free applicative.
--
-- ** D, for deductions
--    1. C1 → context sensitivity requires a function of form (a → Context → Size)
--    2. Type classes cannot provide D1, because..
--       - there is no obvious location to provide the constraint for C1,
--         because C2 requires unrestrained polymorphism on the variable of 'a'.
--       - skolem, implied by free applicative?  This is admittedly less clear, because of
--         well, the way we choose to use FA.
--         - we should elucidate the reasoning here..
--    3. D2 → the only other option is explicit provision of such a type-dependent method vocabulary
--

-- * TODO
--
-- 1. Tracking the dimensionality type across the entire thing is painful --
--    we might want to try shifting this information into a promoted type.
--
--    We'd still need to deal with the kind equality, but there's a hunch that
--    this might be easier.
--



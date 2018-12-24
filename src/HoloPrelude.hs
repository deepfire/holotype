{-# LANGUAGE AllowAmbiguousTypes, ExplicitForAll, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RankNTypes, UndecidableInstances #-}
{-# LANGUAGE GADTs, TypeFamilies, TypeFamilyDependencies, TypeInType #-}
{-# LANGUAGE LambdaCase, OverloadedStrings, PartialTypeSignatures, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TupleSections, TypeOperators #-}
{-# LANGUAGE ViewPatterns, UnicodeSyntax #-}
{-# OPTIONS_GHC -Wextra #-}
{-# OPTIONS_GHC -Wno-implicit-prelude -Wno-missing-import-lists -Wno-missed-specialisations #-}
{-# OPTIONS_GHC -Wno-unsafe #-}

module HoloPrelude
  ( module Control.Applicative
  , module Control.Lens
  , module Data.Function
  , module Data.Functor
  , module Data.List
  , module Elsewhere
  , module GHC.Types
  , module Prelude.Unicode
  , module Tracer
  --
  , Doc
  , Generic
  , HasCallStack
  , IsString
  , MonadIO, liftIO
  , Pretty(..)
  , HexShow(..)
  --
  , (<>)
  , assert
  , doubleToFloat
  , either
  , filterM
  , fromMaybe
  , printf
  , trace
  , unless
  , when
  --
  , dumpPretty, ppCompact, ppPretty, trace'pp, rendCompact, rendPretty, prettyMaybe, unreadable
  , (<->), (<:>), (<+>)
  , trace'
  , PP(..)
  , showT,  showTL,  showTS
  , errorT, errorTL, errorTS
  , showTime, timeDiff, printTimeDiff
  , ppV2
  )
where

import           Control.Applicative
import           Control.Lens                      hiding (children, As)
import           Control.Monad                            (unless, when, filterM)
import           Control.Monad.IO.Class                   (MonadIO, liftIO)
import           Control.Exception                        (assert)
import           Data.Either                              (either)
import           Data.Function
import           Data.Functor
import           Data.List                         hiding (uncons)
import           Data.Maybe                               (fromMaybe)
import           Data.Monoid                              ((<>))
import           Data.String                              (IsString)
import qualified Data.Text                         as TS
import qualified Data.Text.Lazy                    as TL
import qualified Data.Text.Lazy.IO
import           Data.Time.Clock
import           Debug.Trace                              (trace)
import           GHC.Generics                             (Generic)
import           GHC.Stack                                (HasCallStack)
import           GHC.Types                         hiding (Constraint, Word)
import           Numeric
import           Numeric.Extra                            (doubleToFloat)
import           Linear                            hiding (trace)
import           Prelude                           hiding ((.), Word, words)
import           Prelude.Unicode
import           Text.PrettyPrint.Leijen.Text             (Doc, Pretty
                                                          , displayT, renderCompact, renderPretty
                                                          , char, angles, text, pretty
                                                          , (<+>))
import           Text.Printf                              (printf)

import           Elsewhere
import           Tracer

-- * Pretty
newtype HexShow = HexShow Int

instance Show HexShow where
  show (HexShow x) = showHex x ""

dumpPretty ∷ Pretty a ⇒ Int → a → IO ()
dumpPretty = Data.Text.Lazy.IO.putStrLn .: ppPretty

ppCompact ∷ Pretty a ⇒ a → TL.Text
ppCompact = rendCompact ∘ pretty

ppPretty ∷ Pretty a ⇒ Int → a → TL.Text
ppPretty wi =  rendPretty wi ∘ pretty

trace'pp ∷ Pretty a ⇒ String → a → a
trace'pp prefix o = trace (prefix <> TL.unpack (ppPretty 60 o)) o

rendCompact ∷ Doc → TL.Text
rendCompact = displayT ∘ renderCompact

rendPretty ∷ Int → Doc → TL.Text
rendPretty wi = displayT ∘ renderPretty 1.0 wi

prettyMaybe ∷ Pretty a ⇒ TL.Text → Maybe a → Doc
prettyMaybe m = fromMaybe (text m) ∘ (pretty <$>)

unreadable ∷ TL.Text → Doc → Doc
unreadable ty x = char '#' <> angles (text ty <+> x)

(<->), (<:>) ∷ Doc → Doc → Doc
l <-> r = l <> char '-' <> r
l <:> r = l <> char ':' <> r

trace' ∷ Show a ⇒ String → a → a
trace' prefix o = trace (prefix <> (show o)) o

class Show a ⇒ PP a where
  {-# MINIMAL pp | ppL #-}
  pp  ∷ a → TS.Text
  ppL ∷ a → TL.Text
  ppS ∷ a → String
  pp  = TL.toStrict ∘ ppL
  ppL = TL.fromStrict ∘ pp
  ppS = TS.unpack ∘ pp

showTS, showT ∷ Show a ⇒ a → TS.Text
showTS = TS.pack ∘ show
showT = showTS

showTL ∷ Show a ⇒ a → TL.Text
showTL = TL.pack ∘ show

errorTS, errorT ∷ HasCallStack ⇒ TS.Text → a
errorTS = error ∘ TS.unpack
errorT = errorTS

errorTL ∷ HasCallStack ⇒ TL.Text → a
errorTL = error ∘ TL.unpack

-- * Pretty
ppV2 ∷ Show a ⇒ V2 a → TL.Text
ppV2 x = (showTL $ x^._x) <> "x" <> (showTL $ x^._y)

-- * simple benchamrking functions from lambdacube-quake3
--
showTime ∷ NominalDiffTime → String
showTime delta
    | t > 1e-1  = printf "%.3fs" t
    | t > 1e-3  = printf "%.1fms" (t/1e-3)
    | otherwise = printf "%.0fus" (t/1e-6)
  where
    t = realToFrac delta :: Double

timeDiff ∷ IO a → IO (NominalDiffTime, a)
timeDiff m = (\s x e -> (diffUTCTime e s, x))
  <$> getCurrentTime
  <*> m
  <*> getCurrentTime

printTimeDiff ∷ String → IO a → IO a
printTimeDiff message m = do
  (t,r) <- timeDiff m
  putStr message
  putStrLn $ showTime t
  return r

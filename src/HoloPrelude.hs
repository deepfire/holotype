{-# LANGUAGE AllowAmbiguousTypes, ExplicitForAll, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RankNTypes, UndecidableInstances #-}
{-# LANGUAGE GADTs, TypeFamilies, TypeFamilyDependencies, TypeInType #-}
{-# LANGUAGE LambdaCase, OverloadedStrings, PartialTypeSignatures, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TupleSections, TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Weverything #-}
{-# OPTIONS_GHC -Wno-implicit-prelude -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-unsafe #-}

module HoloPrelude
  ( module Control.Applicative
  , module Control.Lens
  , module Data.Function
  , module Data.List
  , module GHC.Types
  , module Prelude.Unicode
  --
  , Doc
  , Generic
  , HasCallStack
  , IsString
  , MonadIO, liftIO
  , Pretty(..)
  --
  , (<>)
  , (.:)
  , assert
  , catchAny
  , choosePartially
  , doubleToFloat
  , either
  , filterM
  , fromMaybe
  , goldenRatio
  , partial
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
  )
where

import           Control.Applicative
import           Control.Exception                        (AsyncException, SomeException, assert, catch, fromException, throwIO)
import           Control.Lens                      hiding (children)
import           Control.Monad                            (unless, when, filterM)
import           Control.Monad.IO.Class                   (MonadIO, liftIO)
import           Control.Monad.Plus                       (partial)
import           Data.Either                              (either)
import           Data.Function
import           Data.List                         hiding (uncons)
import           Data.Maybe                               (fromMaybe)
import           Data.Monoid                              ((<>))
import           Data.String                              (IsString)
import qualified Data.Text                         as TS
import qualified Data.Text.Lazy                    as TL
import qualified Data.Text.Lazy.IO
import           Debug.Trace                              (trace)
import           GHC.Generics                             (Generic)
import           GHC.Stack                                (HasCallStack)
import           GHC.Types                         hiding (Constraint, Word)
import           Numeric.Extra                            (doubleToFloat)
import           Prelude                           hiding ((.), Word)
import           Prelude.Unicode
import           Text.PrettyPrint.Leijen.Text             (Doc, Pretty
                                                          , displayT, renderCompact, renderPretty
                                                          , char, angles, text, pretty
                                                          , (<+>))
import           Text.Printf                              (printf)

-- * Pretty numbers
goldenRatio ∷ Double
goldenRatio = 1.61803398875

-- * Cool functions
(.:) ∷ ∀ a f g b. (b → a) → (f → g → b) → f → g → a
(.:) = (.) ∘ (.)

infixr 9 .:

choosePartially ∷ Eq a ⇒ a → a → a → a
choosePartially one l r = fromMaybe one $ partial (≢ one) l <|> partial (≢ one) r

-- * Exceptions
catchAny ∷ IO a → (SomeException → IO a) → IO a
catchAny guarded handler = Control.Exception.catch guarded onExc
  where onExc e | shouldCatch e = handler e
                | otherwise = throwIO e
        shouldCatch e
          | show e ≡ "<<timeout>>" = False
          | Just (_ ∷ AsyncException) ← fromException e = False
          | otherwise = True

-- * Pretty
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

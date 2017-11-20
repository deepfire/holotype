{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Weverything #-}
{-# OPTIONS_GHC -Wno-implicit-prelude -Wno-missing-import-lists -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unsafe #-}

module Elsewhere
  ( ppV2
  , textZipper, zipperText
  )
where

import           HoloPrelude

import           Control.Monad.Random
import           Control.Monad.State
import           Data.Complex
import           Data.Glb                                 (HasGlb(..))
import           Data.Lub                                 (HasLub(..))
import           Data.Monoid                              ((<>))
import qualified Data.Text                         as T
import qualified Data.Text.Lazy                    as TL
import qualified Data.Text.Zipper                  as T
import           Linear                            hiding (trace)


-- * 'lub' + 'linear'
instance Ord a ⇒ HasLub (V2 a) where lub = liftA2 max
instance Ord a ⇒ HasGlb (V2 a) where glb = liftA2 min


-- * 'base' + 'random'
--
instance Random a ⇒ Random (Complex a) where
  randomR (r:+i, r':+i') = runState $ liftA2 (:+) (state $ randomR (r,r')) (state $ randomR (i,i'))
  random                 = runState $ liftA2 (:+) (state $ random)         (state $ random)


-- * Pretty
ppV2 ∷ Show a ⇒ V2 a → TL.Text
ppV2 x = (showTL $ x^._x) <> "x" <> (showTL $ x^._y)


-- * Text.Zipper
textZipper ∷ [T.Text] → T.TextZipper T.Text
textZipper = flip T.textZipper Nothing

zipperText ∷ T.TextZipper T.Text → T.Text
zipperText = T.dropEnd 1 ∘ T.unlines ∘ T.getText

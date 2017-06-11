{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnicodeSyntax #-}

module Elsewhere where

import qualified Alib                              as M
import           Control.Applicative
import           Control.Monad.Random
import           Control.Monad.State
import           Data.Complex
import           Data.Glb             (HasGlb(..))
import           Data.Lub             (HasLub(..))
import           Data.Monoid                ((<>))
import qualified Data.Text                         as T
import qualified Data.Text.Zipper                  as T
import           Linear
import           Prelude.Unicode
import           Reflex


-- * Pretty numbers
goldenRatio ∷ Double
goldenRatio = 1.61803398875


-- * Cool functions
(.:) ∷ ∀ a f g b. (b → a) → (f → g → b) → f → g → a
(.:) = M.o


-- * 'lub' + 'linear'
instance Ord a ⇒ HasLub (V2 a) where lub = liftA2 max
instance Ord a ⇒ HasGlb (V2 a) where glb = liftA2 min


-- * 'base' + 'random'
--
instance Random a ⇒ Random (Complex a) where
  randomR (r:+i, r':+i') = runState $ liftA2 (:+) (state $ randomR (r,r')) (state $ randomR (i,i'))
  random                 = runState $ liftA2 (:+) (state $ random)         (state $ random)


-- * 'text-zipper'
textZipper ∷ [T.Text] → T.TextZipper T.Text
textZipper = flip T.textZipper Nothing

zipperText ∷ T.TextZipper T.Text → T.Text
zipperText = T.dropEnd 1 ∘ T.unlines ∘ T.getText


-- * 'reflex'
simpler ∷ Reflex t ⇒ Event t a → Event t ()
simpler = (() <$)

someFire ∷ Reflex t ⇒ Event t a → Event t b → Event t ()
someFire a b = simpler a <> simpler b

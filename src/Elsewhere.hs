{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

module Elsewhere where

import qualified Alib                              as M
import           Control.Applicative
import           Control.Exception
import           Control.Lens                      hiding (children)
import           Control.Monad.Random
import           Control.Monad.State
import           Data.Complex
import           Data.Glb                            (HasGlb(..))
import           Data.Lub                            (HasLub(..))
import           Data.Maybe                          (fromMaybe)
import           Data.Monoid                         ((<>))
import           Data.Text                           (Text)
import qualified Data.Text                         as T
import qualified Data.Text.Lazy                    as TL
import qualified Data.Text.Format                  as T
import qualified Data.Text.Zipper                  as T
import           Data.Void
import           Debug.Trace                              (trace)
import           GHC.Stack
import           Linear                            hiding (trace)
import           Prelude.Unicode
import           Reflex
import qualified Data.Text.Lazy.IO                 as TLIO
-- import           Text.PrettyPrint.Free             hiding ((<>), space)
import           Text.PrettyPrint.Leijen.Text      hiding ((<>), (<$>), space)


-- * Pretty numbers
goldenRatio ∷ Double
goldenRatio = 1.61803398875


-- * Cool functions
(.:) ∷ ∀ a f g b. (b → a) → (f → g → b) → f → g → a
(.:) = M.o

infixr 9 .:

void ∷ Void
void = (⊥)


catchAny ∷ IO a → (SomeException → IO a) → IO a
catchAny guarded handler = Control.Exception.catch guarded onExc
  where onExc e | shouldCatch e = handler e
                | otherwise = throwIO e
        shouldCatch e
          | show e ≡ "<<timeout>>" = False
          | Just (_ ∷ AsyncException) ← fromException e = False
          | otherwise = True


-- * 'lub' + 'linear'
instance Ord a ⇒ HasLub (V2 a) where lub = liftA2 max
instance Ord a ⇒ HasGlb (V2 a) where glb = liftA2 min


-- * 'base' + 'random'
--
instance Random a ⇒ Random (Complex a) where
  randomR (r:+i, r':+i') = runState $ liftA2 (:+) (state $ randomR (r,r')) (state $ randomR (i,i'))
  random                 = runState $ liftA2 (:+) (state $ random)         (state $ random)


-- * 'text-zipper'
textZipper ∷ [Text] → T.TextZipper Text
textZipper = flip T.textZipper Nothing

zipperText ∷ T.TextZipper Text → Text
zipperText = T.dropEnd 1 ∘ T.unlines ∘ T.getText


-- * 'reflex'
simpler ∷ Reflex t ⇒ Event t a → Event t ()
simpler = (() <$)

someFire ∷ Reflex t ⇒ Event t a → Event t b → Event t ()
someFire a b = simpler a <> simpler b


-- * Pretty
dumpPretty ∷ Pretty a ⇒ Int → a → IO ()
dumpPretty = TLIO.putStrLn .: ppPretty

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

ppV2 ∷ Show a ⇒ V2 a → TL.Text
ppV2 x = (showTL $ x^._x) <> "x" <> (showTL $ x^._y)

class Show a ⇒ PP a where
  {-# MINIMAL pp | ppL #-}
  pp  ∷ a → Text
  ppL ∷ a → TL.Text
  ppS ∷ a → String
  pp  = TL.toStrict ∘ ppL
  ppL = TL.fromStrict ∘ pp
  ppS = T.unpack ∘ pp

showTS, showT ∷ Show a ⇒ a → Text
showTS = T.pack ∘ show
showT = showTS

showTL ∷ Show a ⇒ a → TL.Text
showTL = TL.pack ∘ show

errorT ∷ HasCallStack ⇒ Text → a
errorT = error ∘ T.unpack

errorTL ∷ HasCallStack ⇒ TL.Text → a
errorTL = error ∘ TL.unpack

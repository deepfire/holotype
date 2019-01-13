{-# OPTIONS_GHC -Wextra #-}
{-# OPTIONS_GHC -Wno-implicit-prelude -Wno-missing-import-lists -Wno-missed-specialisations #-}
{-# OPTIONS_GHC -Wno-unsafe #-}
module Pretty
  ( Doc
  , Pretty(..)
  , HexShow(..)
  --
  , printf
  , trace
  --
  , dumpPretty, ppCompact, ppPretty, trace'pp, rendCompact, rendPretty, prettyMaybe, unreadable
  , (<->), (<:>), (<+>)
  , trace'
  , PP(..)
  , showT,  showTL,  showTS
  , errorT, errorTL, errorTS
  )
where

import           Data.Maybe                               (fromMaybe)
import qualified Data.Text                         as TS
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text    (renderLazy)
import qualified Data.Text.Lazy                    as TL
import qualified Data.Text.Lazy.IO
import           Debug.Trace                              (trace)
import           GHC.Stack                                (HasCallStack)
import           Numeric
import           Prelude                           hiding (Word, words)
import           Prelude.Unicode
import           Text.Printf                              (printf)

-- * Pretty
newtype HexShow = HexShow Int

instance Show HexShow where
  show (HexShow x) = showHex x ""

dumpPretty ∷ Pretty a ⇒ Int → a → IO ()
dumpPretty width x = Data.Text.Lazy.IO.putStrLn $ ppPretty width x

ppCompact ∷ Pretty a ⇒ a → TL.Text
ppCompact = rendCompact ∘ pretty

ppPretty ∷ Pretty a ⇒ Int → a → TL.Text
ppPretty wi =  rendPretty wi ∘ pretty

trace'pp ∷ Pretty a ⇒ String → a → a
trace'pp prefix o = trace (prefix <> TL.unpack (ppPretty 60 o)) o

rendCompact ∷ Doc ann → TL.Text
rendCompact = renderLazy ∘ layoutCompact

rendPretty ∷ Int → Doc ann → TL.Text
rendPretty wi = renderLazy ∘ layoutPretty (defaultLayoutOptions { layoutPageWidth = AvailablePerLine wi 1.0 })

prettyMaybe ∷ Pretty a ⇒ TL.Text → Maybe a → Doc ann
prettyMaybe m = fromMaybe (pretty m) ∘ (pretty <$>)

unreadable ∷ TL.Text → Doc ann → Doc ann
unreadable ty x = pretty '#' <> angles (pretty ty <+> x)

(<->), (<:>) ∷ Doc ann → Doc ann → Doc ann
l <-> r = l <> pretty '-' <> r
l <:> r = l <> pretty ':' <> r

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

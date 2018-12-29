{-# OPTIONS_GHC -Weverything #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors -Wno-missing-import-lists -Wno-implicit-prelude -Wno-monomorphism-restriction -Wno-name-shadowing -Wno-all-missed-specialisations -Wno-unsafe -Wno-missing-export-lists -Wno-type-defaults -Wno-partial-fields -Wno-missing-local-signatures -Wno-orphans #-}

module Holo.Classes
  ( As(..), defName
  , Style(..), sStyle, sStyleGene, initStyle, defStyle
  , StyleGene(..), fromStyleGene
  , Visual(..), VPort
  --
  , Interp(..)
  --
  , Name(..)
  , Named(..), defStyGeoName
  --
  , Phase(..)
  , IStrucP
  , VisualP
  , Item(..), iLeafP, iNewToken, iCompToken, iToken, iGeo, iStyleGene, diNothing
  , Node(..)
  , node, leaf
  , hbox, vbox
  , defLeaf
  --
  , traceIGeoDiff
  , iSizeRequest
  , iMandateVisual, iUnvisual
  , iRender
  --
  , treeLeaves
  , ensureTreeVisuals
  , renderTreeVisuals
  , showTreeVisuals
  -- * reёxports
  , Drawable(..)
  , module Flex
  )
where

import           Data.Foldable
import           Data.Maybe
import           Data.Text                                (Text)
import           Data.Typeable
import           GHC.Types                                (Constraint)
import           Generics.SOP                             (Proxy)
import           Linear                            hiding (trace)
import           Text.Read                                (readMaybe)
import qualified Data.Map.Strict                   as Map
import qualified Data.Text                         as T
import qualified Unsafe.Coerce                     as Co

-- Local imports
import           Graphics.Flatland
import           Graphics.Flex                            (Geo, defGeo, Flex(..))
import qualified Graphics.Flex                     as Flex

import           Holo.Prelude
import           Holo.Port                                (IdToken, Drawable, Frame)
import qualified Holo.Port                         as Port


-- * As -- assigning representation
--
class Typeable r  ⇒ As r where
  type         Denoted r ∷ Type
  type             Sty r ∷ Type
  type instance    Sty r = ()
  type          IStruc r ∷ Type
  type instance IStruc r = ()
  type             Vis r ∷ Type
  type instance    Vis r = ()
  defAs       ∷               Proxy r             → r
  defSty      ∷               Proxy r             → Sty r
  compSty     ∷                     r             → Sty r
  compSty                           x             = defSty (proxy x)
  sizeRequest ∷ MonadIO m ⇒ VPort → r → Denoted r → Sty r → m (IStruc r, Di (Maybe Double))
  setupVis    ∷ MonadIO m ⇒ VPort → r → Denoted r → Sty r → IStruc r → Area'LU Double → Drawable → m (Vis r)
  render      ∷ MonadIO m ⇒ VPort → r → Denoted r → Sty r → IStruc r → Po Double      → Drawable → Vis r → m () -- ^ Update visual.
  freeVis     ∷ MonadIO m ⇒   Proxy r                                                         → Vis r → m ()
  freeVis                           _                                                               _ = pure ()


-- * Interp -- assigning interpretation
--
class Interp (a ∷ Type) (b ∷ Type) where
  interp        ∷ a → Maybe b
  forget        ∷ b → a


-- * Mutability:  Mutable, InputEvent & Subscription
--
class Mutable a where
  subscription ∷                    IdToken → Proxy a → Subscription
  subscription = const mempty         -- declare ignorance..
  mutate       ∷ (RGLFW t m) ⇒ a → Event t InputEvent → m (Dynamic t a)
  mutate       = immutable            -- ..then effectuate it

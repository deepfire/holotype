{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Weverything #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors -Wno-missing-import-lists -Wno-implicit-prelude -Wno-monomorphism-restriction -Wno-name-shadowing -Wno-all-missed-specialisations -Wno-unsafe -Wno-missing-export-lists -Wno-type-defaults -Wno-partial-fields -Wno-missing-local-signatures -Wno-orphans #-}

module Holo.Name
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
import           Data.Proxy                               (Proxy)
import           Data.Text                                (Text)
import           Data.Typeable
import           GHC.Types                                (Constraint)
import           Linear                            hiding (trace)
import           Text.Read                                (readMaybe)
import qualified Data.Map.Strict                   as Map
import qualified Data.Text                         as T

-- Local imports
import           Graphics.Flatland
import           Graphics.Flex                            (Geo, defGeo, Flex(..))

import           Holo.Classes
import           Holo.Prelude
import           Holo.Port                                (IdToken, Drawable, Frame)
import qualified Holo.Port                         as Port


-- * Name
--    ..as per Пиотровский Р. Г. Текст, машина, человек — Л.: Наука, 1975
--    Which is supposed to make sense in context of As/Denoted
data Name a where
  Name ∷
    { nToken     ∷ IdToken
    , nStyle     ∷ Style a
    , nGeo       ∷ Geo
    , n          ∷ a
    } → Name a

defName ∷ ∀ a. As a ⇒ IdToken → a → Name a
defName tok n = Name tok defStyle defGeo n

-- default of Named.compName
defStyGeoName ∷ As n ⇒ Proxy a → IdToken → n → Name n
defStyGeoName _ tok n = Name tok (initStyle $ compSty n) defGeo n


-- * Style wrapper
--
newtype StyleGene = StyleGene { _fromStyleGene ∷ Int } deriving (Eq, Ord)
fromStyleGene ∷ Lens' StyleGene Int
fromStyleGene f (StyleGene x) = f x <&> StyleGene

data Style a where
  Style ∷
    { _sStyle      ∷ Sty a
    , _sStyleGene  ∷ StyleGene
    } → Style a

sStyle     ∷ Lens' (Style a) (Sty a)
sStyle     f s@Style{..} = f _sStyle     <&> \x→ s{_sStyle=x}
sStyleGene ∷ Lens' (Style a) StyleGene
sStyleGene f s@Style{..} = f _sStyleGene <&> \x→ s{_sStyleGene=x}

initStyle ∷ Sty a → Style a
initStyle s = Style { _sStyle = s, _sStyleGene = StyleGene 0 }

defStyle ∷ ∀ a. As a ⇒ Style a
defStyle = initStyle $ defSty (Proxy @a)


-- * Visual wrapper
--
data Visual a where
  Visual ∷ As a ⇒
    { vVisual   ∷ Vis a
    , vDrawable ∷ Drawable
    } → Visual a

type VPort = Port.Port Visual

instance Port.PortVisual Visual where
  pvDrawable = vDrawable
  pvFree _pC pA = \case
    Visual{..} → freeVis pA vVisual

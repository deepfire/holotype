{-# LANGUAGE GADTs, TypeFamilies, TypeFamilyDependencies, TypeInType #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitForAll, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RankNTypes, UndecidableInstances #-}
{-# LANGUAGE LambdaCase, OverloadedStrings, PackageImports, PartialTypeSignatures, RecordWildCards, ScopedTypeVariables, TupleSections, TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Weverything #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors -Wno-missing-import-lists -Wno-implicit-prelude #-}
{-# OPTIONS_GHC -Wno-monomorphism-restriction -Wno-name-shadowing -Wno-all-missed-specialisations #-}
{-# OPTIONS_GHC -Wno-unsafe -Wno-missing-export-lists -Wno-type-defaults #-}

module HoloTypes
where

import qualified Control.Concurrent.STM            as STM
import qualified Data.Map.Strict                   as Map
import qualified Data.Unique                       as U
import qualified Foreign.C.Types                   as F
import qualified Foreign                           as F
import qualified GI.Cairo                          as GIC
import qualified Graphics.Rendering.Cairo.Internal as GRCI
import           Graphics.GL.Core33                as GL
import "GLFW-b"  Graphics.UI.GLFW                  as GL
import qualified LambdaCube.GL                     as GL
import qualified LambdaCube.GL.Mesh                as GL
import           LambdaCube.Mesh                   as LC
import           Linear

-- Local imports
import           HoloPrelude

import           Flex                                     (Geo)

import           Flatland
import           HoloCairo
import           HoloCube
import           HoloFont


-- * Identity of what we're drawing, to allow re-use of underlying stateful resources.
newtype IdToken = IdToken { fromIdToken ∷ U.Unique } deriving (Eq, Ord)

-- * Impure IO-stateful map
data IOMap k v where
  IOMap ∷ Ord k ⇒
    { iomap               ∷ STM.TVar (Map.Map k v)
    } → IOMap k v

newtype DrawableTracker = DrawableTracker { fromDT ∷ IOMap IdToken Drawable }
newtype VisualTracker   = VisualTracker   { fromVT ∷ IOMap IdToken WVisual }


-- | Usher Cairo + Pango -enabled surfaces onto a GL Window,
--   according to user-controlled Settings.
data Port where
  Port ∷
    { portSettings        ∷ Settings
    , portFontmap         ∷ FontMap PU
    , portWindow          ∷ GL.Window
    , portObjectStream    ∷ ObjectStream
    , portRenderer        ∷ Renderer
    , portEmptyDrawable   ∷ Drawable
    , portDrawableTracker ∷ DrawableTracker
    , portVisualTracker   ∷ VisualTracker
    } → Port

data Settings where
  Settings ∷
    { sttsDΠ              ∷ DΠ
    , sttsFontPreferences ∷ FontPreferences PU
    } → Settings
    deriving (Eq, Show)

data Drawable where
  Drawable ∷
    { dObjectStream       ∷ ObjectStream
    , dDi                 ∷ Di Int
    , dSurface            ∷ GRCI.Surface
    , dSurfaceData        ∷ (F.Ptr F.CUChar, V2 Int)
    , dCairo              ∷ Cairo
    , dGIC                ∷ GIC.Context
    --
    , dMesh               ∷ LC.Mesh
    , dGPUMesh            ∷ GL.GPUMesh
    , dGLObject           ∷ GL.Object
    , dTexId              ∷ GLuint
    } → Drawable


-- | 'Holo': anything visualisable.
class (Monoid (StyleOf a)) ⇒ Holo a where
  data VisualOf a
  data StyleOf  a
  query           ∷ (MonadIO m) ⇒ Port → StyleOf a →                  a →            m (Di (Maybe Double))
  createVisual    ∷ (MonadIO m) ⇒ Port → StyleOf a → Area'LU Double → a → Drawable → m (VisualOf a)
  renderVisual    ∷ (MonadIO m) ⇒ Port →                 VisualOf a → a →            m ()  -- ^ Update a visualisation of 'a'.
  drawableOf      ∷ Port → VisualOf a → Drawable

data Visual a where
  Visual ∷ Holo a ⇒
    { vVis        ∷ VisualOf a
    , vStyGene    ∷ StyleGene
    , vDrawable   ∷ Drawable
    } → Visual a

data WVisual where
  WVisual ∷ Holo a ⇒
    { wVis        ∷ Visual a
    } → WVisual

newtype StyleGene  = StyleGene  { fromStyleGene  ∷ Int      } deriving (Eq, Ord)
newtype StyleToken = StyleToken { fromStyleToken ∷ U.Unique } deriving (Eq, Ord)


data Phase
  = PBlank
  | PLayout
  | PVisual

type family HIArea   (p ∷ Phase) ∷ Type where
  HIArea   PBlank  = ()
  HIArea   PLayout = Area'LU Double
  HIArea   PVisual = Area'LU Double

type family HIVisual (p ∷ Phase) a ∷ Type where
  HIVisual PBlank  _ = ()
  HIVisual PLayout _ = ()
  HIVisual PVisual a = VisualOf a

data HoloItem (p ∷ Phase) where
  HoloItem ∷ ∀ p a. Holo a ⇒
    { holo       ∷ a
    , hiToken    ∷ IdToken
    -- XXX: everything below seems like it's not needed early?
    , hiGeo      ∷ Geo
    , hiStyle    ∷ StyleOf a
    , hiChildren ∷ [HoloItem p]
    -- Problem:
    -- 1. We have size for top entry, want to record it
    -- 2. The tree is type-coherent, and children need have the same type.
    , hiSize     ∷ Di (Maybe Double)
    , hiArea     ∷ HIArea p
    , hiVisual   ∷ HIVisual p a
    } → HoloItem p

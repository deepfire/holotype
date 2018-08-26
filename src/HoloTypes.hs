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
import           Data.Typeable
import qualified Data.TypeMap.Dynamic              as TM
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


-- * Identity of what we're drawing, to allow re-use of underlying stateful resources.
newtype IdToken = IdToken { fromIdToken' ∷ (U.Unique, String) } deriving (Eq, Ord)

fromIdToken ∷ IdToken → U.Unique
fromIdToken = fst ∘ fromIdToken'

-- * Impure IO-stateful map
data IOMap k v where
  IOMap ∷ Ord k ⇒
    { iomap               ∷ STM.TVar (Map.Map k v)
    } → IOMap k v

data                  VisualIOMap
type instance TM.Item VisualIOMap a = Map.Map IdToken (Visual a)

newtype VIOMap = VIOMap (STM.TVar (TM.TypeMap VisualIOMap))

newtype VisualTracker = VisualTracker { fromVT ∷ VIOMap }


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
class (Typeable a, DefStyleOf (StyleOf a)) ⇒ Holo a where
  data VisualOf a
  data StyleOf  a
  query           ∷ (MonadIO m) ⇒ Port → StyleOf a →                  a →            m (Di (Maybe Double))
  createVisual    ∷ (MonadIO m) ⇒ Port → StyleOf a → Area'LU Double → a → Drawable → m (VisualOf a)
  renderVisual    ∷ (MonadIO m) ⇒ Port →                 VisualOf a → a →            m ()  -- ^ Update a visualisation of 'a'.
  freeVisualOf    ∷ (MonadIO m) ⇒                        VisualOf a → Proxy a      → m ()
class DefStyleOf a where
  defStyleOf      ∷ a

data Visual a where
  Visual ∷ Holo a ⇒
    { vVisual     ∷ VisualOf a
    , vStyleGene  ∷ StyleGene
    , vDrawable   ∷ Drawable
    } → Visual a

newtype StyleGene = StyleGene { _fromStyleGene ∷ Int } deriving (Eq, Ord)
fromStyleGene ∷ Lens' StyleGene Int
fromStyleGene f (StyleGene x) = f x <&> StyleGene

data Style a where
  Style ∷ Holo a ⇒
    { _sStyle      ∷ StyleOf a
    , _sStyleGene  ∷ StyleGene
    } → Style a

sStyle     ∷ Lens' (Style a) (StyleOf a)
sStyle     f s@Style{..} = f _sStyle     <&> \x→ s{_sStyle=x}
sStyleGene ∷ Lens' (Style a) StyleGene
sStyleGene f s@Style{..} = f _sStyleGene <&> \x→ s{_sStyleGene=x}

defStyle ∷ Holo a ⇒ Style a
defStyle = Style defStyleOf (StyleGene 0)


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
  HIVisual PVisual a = Visual a

data HoloItem (p ∷ Phase) where
  HoloItem ∷ ∀ p a. Holo a ⇒
    { holo        ∷ a
    , hiToken     ∷ IdToken
    , hiStyle     ∷ Style a
    , hiGeo       ∷ Geo
    , hiChildren  ∷ [HoloItem p]
    -- Problem (why we have both hiSize & hiArea):
    -- 1. We have size for top entry, want to record it
    -- 2. The tree is type-coherent, and children need have the same type.
    , hiSize      ∷ Di (Maybe Double)
    -- TTG-inspired phasing:
    , hiArea      ∷ HIArea p
    , hiVisual    ∷ HIVisual p a
    } → HoloItem p

hiStyleGene ∷ HoloItem p → StyleGene
hiStyleGene x =
  case x of HoloItem{..} → _sStyleGene hiStyle

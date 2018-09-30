{-# LANGUAGE GADTs, TypeFamilies, TypeFamilyDependencies, TypeInType #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, StandaloneDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitForAll, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RankNTypes, UndecidableInstances #-}
{-# LANGUAGE LambdaCase, OverloadedStrings, PackageImports, PartialTypeSignatures, RecordWildCards, ScopedTypeVariables, TupleSections, TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Weverything #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors -Wno-missing-import-lists -Wno-implicit-prelude #-}
{-# OPTIONS_GHC -Wno-monomorphism-restriction -Wno-name-shadowing -Wno-all-missed-specialisations #-}
{-# OPTIONS_GHC -Wno-unsafe -Wno-missing-export-lists -Wno-type-defaults -Wno-partial-fields #-}

module HoloTypes
  ( module HoloCairo
  --
  , IdToken(..), fromIdToken
  , VIOMap(..)
  , VisualIOMap
  , VisualTracker(..)
  --
  , Settings(..)
  , ScreenMode(..)
  , Port(..)
  , PipeName(..)
  , Frame(..)
  , ObjectStream(..), ObjArrayNameS(..), UniformNameS(..)
  --
  , Drawable(..)
  , Visual(..)
  --
  , Style(..)
  , DefStyleOf(..), defStyle
  , StyleGene(..), sStyle, sStyleGene, fromStyleGene
  , HIArea, HIVisual
  , Holo(..), hiStyleGene, hiHasVisual
  , Item(..)
  , Phase(..)
  )
where

import qualified Control.Concurrent.STM            as STM
import qualified Data.ByteString.Char8             as SB
import qualified Data.Map.Strict                   as Map
import           Data.Typeable
import qualified Data.TypeMap.Dynamic              as TM
import qualified Data.Set                          as Set
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
import qualified Reflex.GLFW                       as GLFW

-- Local imports
import           HoloPrelude

import           Flex                                     (Geo)

import           Flatland
import           HoloCairo                                (FKind(..))
import qualified HoloCairo                         as Cr


-- * Identity of what we're drawing, to allow re-use of underlying stateful resources.
newtype IdToken = IdToken { fromIdToken' ∷ U.Unique } deriving (Eq, Ord)
instance Show IdToken where
  show (IdToken u) = printf "(IdToken 0x%x)" (U.hashUnique u)

fromIdToken ∷ IdToken → U.Unique
fromIdToken = fromIdToken'

data                  VisualIOMap
type instance TM.Item VisualIOMap a = Map.Map IdToken (Visual a)

newtype VIOMap = VIOMap (STM.TVar (TM.TypeMap VisualIOMap))

newtype VisualTracker = VisualTracker { fromVT ∷ VIOMap }


-- | Usher Cairo + Pango -enabled surfaces onto a GL Window,
--   according to user-controlled Settings.
data Port where
  Port ∷
    { portSettings        ∷ Settings

    , portFontmap         ∷ Cr.FontMap PU
    , portWindow          ∷ GL.Window

    , portVisualTracker   ∷ VisualTracker

    , portGLStorage       ∷ GL.GLStorage
    , portObjectStream    ∷ ObjectStream

    , portPipelines       ∷ Map.Map PipeName GL.GLRenderer
    } → Port
    deriving (Generic)

data ScreenMode
  = FullScreen
  | Windowed
  deriving (Eq, Generic, Show)

data Settings where
  Settings ∷
    { sttsDΠ              ∷ DΠ
    , sttsFontPreferences ∷ Cr.FontPreferences PU
    , sttsScreenMode      ∷ ScreenMode
    , sttsScreenDim       ∷ Di Int
    } → Settings
    deriving (Eq, Generic, Show)

data Drawable where
  Drawable ∷
    { dObjectStream       ∷ ObjectStream
    , dDi                 ∷ Di Int
    , dSurface            ∷ GRCI.Surface
    , dSurfaceData        ∷ (F.Ptr F.CUChar, V2 Int)
    , dCairo              ∷ Cr.Cairo
    , dGIC                ∷ GIC.Context
    --
    , dMesh               ∷ LC.Mesh
    , dGPUMesh            ∷ GL.GPUMesh
    , dGLObject           ∷ GL.Object
    , dTexId              ∷ GLuint
    } → Drawable

data PipeName
  = PipeDraw
  | PipePick
  deriving (Bounded, Enum, Eq, Ord, Show)

-- | A GL 'Frame'.
data Frame where
  Frame ∷
    { fDim ∷ Di Int
    } → Frame

-- | Render context for all objects with the same GL store.
data ObjectStream where
  ObjectStream ∷
    { osStorage  ∷ GL.GLStorage
    , osObjArray ∷ ObjArrayNameS
    , osUniform  ∷ UniformNameS
    } → ObjectStream

newtype UniformNameS  = UniformNameS  { fromUNS  ∷ SB.ByteString } deriving (Eq, IsString, Ord, Show)
newtype ObjArrayNameS = ObjArrayNameS { fromOANS ∷ String }        deriving (Eq, IsString, Ord, Show)


-- | 'Holo': anything visualisable.
class (Typeable a, DefStyleOf (StyleOf a)) ⇒ Holo a where
  data VisualOf a
  data StyleOf  a
  query           ∷ (MonadIO m) ⇒ Port → StyleOf a →                                   a → m (Di (Maybe Double))
  hasVisual       ∷                                                                    a → Bool
  createVisual    ∷ (MonadIO m) ⇒ Port → StyleOf a → Area'LU Double → Drawable →       a → m (VisualOf a)
  renderVisual    ∷ (MonadIO m) ⇒ Port →                            VisualOf a →       a → m ()  -- ^ Update a visualisation of 'a'.
  freeVisualOf    ∷ (MonadIO m) ⇒                                   VisualOf a → Proxy a → m ()
  --
  hasVisual _     = False
class DefStyleOf a where
  defStyleOf      ∷ a

data Visual a where
  Visual ∷ Holo a ⇒
    { vVisual     ∷ Maybe (VisualOf a)
    , vStyleGene  ∷ StyleGene
    , vDrawable   ∷ Maybe Drawable
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
  HIVisual PVisual a = Maybe (Visual a)

data Item (p ∷ Phase) where
  Item ∷ ∀ p a. Holo a ⇒
    { holo        ∷ a
    , hiToken     ∷ IdToken
    , hiStyle     ∷ Style a
    , hiGeo       ∷ Geo
    , hiChildren  ∷ [Item p]
    -- Problem (why we have both hiSize & hiArea):
    -- 1. We have size for top entry, want to record it
    -- 2. The tree is type-coherent, and children need have the same type.
    , hiSize      ∷ Di (Maybe Double)
    -- TTG-inspired phasing:
    , hiArea      ∷ HIArea p
    , hiVisual    ∷ HIVisual p a
    } → Item p

hiStyleGene ∷ Item p → StyleGene
hiStyleGene x =
  case x of Item{..} → _sStyleGene hiStyle

hiHasVisual ∷ Item p → Bool
hiHasVisual x =
  case x of Item{..} → hasVisual holo


-- * Impure IO-stateful map

-- data IOMap k v where
--   IOMap ∷ Ord k ⇒
--     { iomap               ∷ STM.TVar (Map.Map k v)
--     } → IOMap k v

-- mkIOMap ∷ (MonadIO m, Ord k) ⇒ m (IOMap k v)
-- mkIOMap = IOMap
--   <$> (liftIO $ STM.newTVarIO $ Map.empty)

-- iomapAccess ∷ (MonadIO m) ⇒ IOMap k v → m (Map.Map k v)
-- iomapAccess IOMap{..} = liftIO $ STM.readTVarIO iomap

-- iomapAdd ∷ (MonadIO m, Ord k) ⇒ IOMap k v → k → v → m ()
-- iomapAdd IOMap{..} k v = liftIO $ do
--   STM.atomically $ STM.modifyTVar' iomap (Map.insert k v)
--   pure ()

-- iomapDrop ∷ (MonadIO m, Ord k) ⇒ IOMap k v → k → m ()
-- iomapDrop IOMap{..} x = liftIO $
--   STM.atomically $ STM.modifyTVar' iomap (Map.delete x)

-- iomapHas ∷ (MonadIO m, Ord k) ⇒ IOMap k v → k → m Bool
-- iomapHas iomap x = Map.member x <$> iomapAccess iomap

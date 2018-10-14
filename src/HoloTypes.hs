{-# LANGUAGE GADTs, TypeFamilies, TypeFamilyDependencies, TypeInType #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, StandaloneDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitForAll, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RankNTypes, UndecidableInstances #-}
{-# LANGUAGE LambdaCase, OverloadedStrings, PackageImports, PartialTypeSignatures, RecordWildCards, ScopedTypeVariables, TupleSections, TypeOperators, ViewPatterns #-}
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
  , Input(..)
  , inputMatch
  , InputMask(..), Subscription(..), subSingleton
  , inputMaskKeys, inputMaskButtons, inputMaskChars, editMaskKeys
  --
  , DefStyleOf(..)
  , Style(..), initStyle, defStyle
  , StyleGene(..), sStyle, sStyleGene, fromStyleGene
  , HIArea, HIVisual
  , Holo(..), hiStyleGene, hiHasVisual
  , Item(..)
  , Phase(..), HoloBlank
  --
  , Widget, value
  , HWidget, trim
  , InputMux
  )
where

import           Control.Arrow
import           Data.Foldable
import           Data.Functor.Misc                        (Const2(..))
import           Data.Typeable
import           Generics.SOP                             (Proxy)
import           Graphics.GL.Core33                as GL
import           LambdaCube.Mesh                   as LC
import           Linear
import           Reflex                            hiding (Query, Query(..))
import           Reflex.GLFW                              (RGLFW, InputU(..))
import "GLFW-b"  Graphics.UI.GLFW                  as GL
import qualified Control.Concurrent.STM            as STM
import qualified Data.ByteString.Char8             as SB
import qualified Data.Map.Monoidal.Strict          as MMap
import qualified Data.Map.Strict                   as Map
import qualified Data.Sequence                     as Seq
import qualified Data.Set                          as Set
import qualified Data.TypeMap.Dynamic              as TM
import qualified Data.Unique                       as U
import qualified Foreign                           as F
import qualified Foreign.C.Types                   as F
import qualified GHC.Generics                      as GHC
import qualified GI.Cairo                          as GIC
import qualified Generics.SOP                      as SOP
import qualified Graphics.Rendering.Cairo.Internal as GRCI
import qualified LambdaCube.GL                     as GL
import qualified LambdaCube.GL.Mesh                as GL
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
    deriving (GHC.Generic)

data ScreenMode
  = FullScreen
  | Windowed
  deriving (Eq, GHC.Generic, Show)

data Settings where
  Settings ∷
    { sttsDΠ              ∷ DΠ
    , sttsFontPreferences ∷ Cr.FontPreferences PU
    , sttsScreenMode      ∷ ScreenMode
    , sttsScreenDim       ∷ Di Int
    } → Settings
    deriving (Eq, GHC.Generic, Show)
instance SOP.Generic         Settings
instance SOP.HasDatatypeInfo Settings

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
type HoloBlank      = Item PBlank
type Widget   t   a =                (Dynamic t Subscription, Dynamic t (a, HoloBlank))
value               ∷                (Dynamic t Subscription, Dynamic t a) → Dynamic t a
value               = snd
type HWidget  t     =                (Dynamic t Subscription, Dynamic t HoloBlank)

trim ∷ Reflex t ⇒ Widget t a → HWidget t
trim = (id *** (snd <$>))

class (Typeable a, DefStyleOf (StyleOf a)) ⇒ Holo a where
  data VisualOf a
  data StyleOf  a
  subscription    ∷                                                    Proxy a → IdToken → Subscription
  liftDyn         ∷ (RGLFW t m) ⇒                                      a → Event t Input → m (Dynamic t a)
  compStyle       ∷                                                                    a → StyleOf a
  query           ∷ (MonadIO m) ⇒ Port → StyleOf a →                  [Item PLayout] → a → m (Di (Maybe Double))
  hasVisual       ∷                                                                    a → Bool
  createVisual    ∷ (MonadIO m) ⇒ Port → StyleOf a → Area'LU Double → Drawable →       a → m (VisualOf a)
  renderVisual    ∷ (MonadIO m) ⇒ Port →                            VisualOf a →       a → m ()  -- ^ Update a visualisation of 'a'.
  freeVisualOf    ∷ (MonadIO m) ⇒                                   VisualOf a → Proxy a → m ()
  --
  liftDyn init _e = pure $ constDyn init
  subscription    = const mempty
  compStyle       = const defStyleOf
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

initStyle ∷ Holo a ⇒ StyleOf a → Style a
initStyle s = Style { _sStyle = s, _sStyleGene = StyleGene 0 }

defStyle ∷ Holo a ⇒ Style a
defStyle = initStyle defStyleOf



type InputMux t     = EventSelector t (Const2 IdToken Input)

data Input where
  Input ∷
    { inInput ∷ GLFW.InputU
    } → Input
  deriving (Show)

data InputMask where
  InputMask ∷
    { inputMask ∷ GLFW.EventMask
    } → InputMask
  deriving (Eq, Ord)
instance Show InputMask where
  show InputMask{..} = ("(IM "<>) ∘ (<>")") $ show inputMask

inputMatch ∷ InputMask → Input → Bool
inputMatch InputMask{..} = \case
  Input{inInput=GLFW.U x} → GLFW.eventMatch inputMask x

instance Semigroup InputMask where
  InputMask a <> InputMask b = InputMask $ a <> b
instance Monoid InputMask where
  mempty = InputMask mempty

newtype Subscription = Subscription (MMap.MonoidalMap GLFW.EventType (Seq.Seq (IdToken, InputMask)))
instance Show Subscription where
  show (Subscription map) = ("(Subs"<>) ∘ (<>")") $ concat $
    [ " "<>show et<>"::"<> intercalate "+" [ printf "0x%x:%s" tok (show em)
                                           | (U.hashUnique ∘ fromIdToken' → tok,(InputMask em)) ← toList subs]
    | (et, subs) ← MMap.toList map]

instance Semigroup Subscription where
  Subscription a <> Subscription b = Subscription $ a <> b

deriving instance Monoid Subscription

subSingleton ∷ IdToken → InputMask → Subscription
subSingleton tok im@(InputMask em) = Subscription $
  MMap.fromList [ (evty, Seq.singleton (tok, im))
                | evty ← GLFW.eventMaskTypes em ]

inputMaskKeys    ∷ Set.Set GL.Key → Set.Set GL.KeyState → GL.ModifierKeys → InputMask
-- inputMaskKeys = InputMask ∘ GLFW.eventMaskKeys .:: GLFW.KeyEventMask
inputMaskKeys ks kss mks = InputMask $ GLFW.eventMaskKeys $ GLFW.KeyEventMask ks kss mks

inputMaskChars   ∷ InputMask
inputMaskChars   = InputMask $ GLFW.eventMaskChars

inputMaskButtons ∷ GLFW.ButtonEventMask → InputMask
inputMaskButtons = InputMask ∘ GLFW.eventMaskButtons

editMaskKeys ∷ InputMask
editMaskKeys = (inputMaskChars <>) $ InputMask $ GLFW.eventMaskKeys $ GLFW.KeyEventMask
  (Set.fromList
   [ GL.Key'Up, GL.Key'Down, GL.Key'Left, GL.Key'Right, GL.Key'Home, GL.Key'End
   , GL.Key'Backspace, GL.Key'Delete, GL.Key'Enter
   ])
  (Set.fromList [GL.KeyState'Pressed, GL.KeyState'Repeating])
  (mempty)


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

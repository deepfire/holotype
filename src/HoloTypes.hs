{-# LANGUAGE ConstraintKinds, GADTs, TypeFamilies, TypeFamilyDependencies, TypeInType, TypeApplications #-}
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
  , IdToken(..), fromIdToken, tokenHash, newId
  , VIOMap(..)
  , VisualIOMap
  , VisualTracker(..)
  , blankIdToken, blankIdToken'setup
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
  , item, node, leaf
  , hbox, vbox
  --
  , Derived(..), W
  , WH, wWH
  , InputMux
  , liftWStatic, liftDynW'
  )
where

import           Control.Arrow
import           Data.Foldable
import           Data.Functor.Misc                        (Const2(..))
import           Data.Typeable
import           Generics.SOP                             (Proxy)
import           Generics.SOP.Monadic
import           GHC.Types                                (Constraint)
import           Graphics.GL.Core33                as GL
import           LambdaCube.Mesh                   as LC
import           Linear
import           Reflex                            hiding (Query, Query(..))
import           Reflex.GLFW                              (RGLFW)
import "GLFW-b"  Graphics.UI.GLFW                  as GL
import qualified Control.Concurrent.STM            as STM
import qualified Data.ByteString.Char8             as SB
import qualified Data.IORef                        as IO
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
import qualified System.IO.Unsafe                  as IO

-- Local imports
import           HoloPrelude

import           Flex                                     (Geo)
import qualified Flex                              as Flex

import           Flatland
import           HoloCairo                                (FKind(..))
import qualified HoloCairo                         as Cr


-- * Drawable identity support
newtype IdToken = IdToken { fromIdToken' ∷ U.Unique } deriving (Eq, Ord)
instance Show IdToken where
  show (IdToken u) = printf "(IdToken 0x%x)" (U.hashUnique u)

fromIdToken ∷ IdToken → U.Unique
fromIdToken = fromIdToken'

newId ∷ (HasCallStack, MonadIO m) ⇒ m IdToken
newId = liftIO $ do
  tok ← U.newUnique
  trev ALLOC TOK (U.hashUnique tok) (U.hashUnique tok)
  pure $ IdToken tok

blankIdToken'      ∷ IO.IORef IdToken
blankIdToken'      = IO.unsafePerformIO $ IO.newIORef  undefined
blankIdToken'setup ∷ IO ()
blankIdToken'setup = IO.writeIORef blankIdToken' =<< newId
blankIdToken       ∷ IdToken
blankIdToken       = IO.unsafePerformIO $ IO.readIORef blankIdToken'
{-# NOINLINE blankIdToken #-}

tokenHash ∷ IdToken → Int
tokenHash = U.hashUnique ∘ fromIdToken

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


-- * Item
--
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

instance Eq (Item a) where
  (==) a b = (≡) (hiToken a) (hiToken b)

instance Ord (Item a) where
  compare a b = compare (hiToken a) (hiToken b)

instance Flex.Flex (Item PBlank) where
  geo      f hi@Item{..} = (\x→ hi {hiGeo=x})       <$> f hiGeo
  size     f hi@Item{..} = (\x→ hi {hiSize=x})      <$> f hiSize
  children f hi@Item{..} = (\x→ hi {hiChildren=x})  <$> f hiChildren
  area     f hi@Item{..} = (\_→ hi {hiArea=mempty}) <$> f mempty

instance Flex.Flex (Item PLayout) where
  geo      f hi@Item{..} = (\x→ hi {hiGeo=x})      <$> f hiGeo
  size     f hi@Item{..} = (\x→ hi {hiSize=x})     <$> f hiSize
  children f hi@Item{..} = (\x→ hi {hiChildren=x}) <$> f hiChildren
  area     f hi@Item{..} = (\x→ hi {hiArea=x})     <$> f hiArea

instance Flex.Flex (Item PVisual) where
  geo      f hi@Item{..} = (\x→ hi {hiGeo=x})      <$> f hiGeo
  size     f hi@Item{..} = (\x→ hi {hiSize=x})     <$> f hiSize
  children f hi@Item{..} = (\x→ hi {hiChildren=x}) <$> f hiChildren
  area     f hi@Item{..} = (\x→ hi {hiArea=x})     <$> f hiArea

hiStyleGene ∷ Item p → StyleGene
hiStyleGene x =
  case x of Item{..} → _sStyleGene hiStyle

hiHasVisual ∷ Item p → Bool
hiHasVisual x =
  case x of Item{holo=_holo ∷ a, ..} → hasVisual (Proxy @a)


-- * Leaf & Node
--
data KNode
  = VBox
  | HBox

data Node (k ∷ KNode) where
  HBoxN ∷ Node HBox
  VBoxN ∷ Node VBox

boxAxis ∷ Node a → Axis
boxAxis HBoxN = X
boxAxis VBoxN = Y

instance DefStyleOf (StyleOf (Node k)) where
  defStyleOf             = NodeStyle
instance Typeable k ⇒ Holo   (Node (k ∷ KNode)) where
  data StyleOf  (Node k) = NodeStyle
  data VisualOf (Node k) = NodeVisual
  compGeo HBoxN          = mempty & Flex.direction .~ Flex.DirRow    & Flex.align'content .~ Flex.AlignStart
  compGeo VBoxN          = mempty & Flex.direction .~ Flex.DirColumn & Flex.align'content .~ Flex.AlignStart
  query       _ _ xs box =
    -- Requirement is a sum of children requirements
    pure $ (Just <$>) $ _reqt'di $ foldl' (reqt'add $ boxAxis box) zero $ (\x→ Reqt (fromMaybe 0 <$> x^.Flex.size)) <$> xs

item ∷ ∀ a. (Holo a)
  ⇒ IdToken
  → Style a
  → a
  → Geo
  → [Item PBlank]
  → Item PBlank
item hiToken hiStyle holo hiGeo hiChildren =
  let hiSize   = Di (V2 Nothing Nothing)
      hiArea   = ()
      hiVisual = ()
  in Item{..}

node ∷ ∀ a k. (Holo a, a ~ Node k)
  ⇒ IdToken
  → Style a
  → a
  → [Item PBlank]
  → Item PBlank
node tok sty holo = item tok sty holo (compGeo holo)

leaf ∷ Holo a
  ⇒ IdToken
  → Style a
  → a
  → Item PBlank
leaf tok sty holo = item tok sty holo (compGeo holo) []

vbox, hbox ∷ [Item PBlank] → Item PBlank
-- XXX: here's trouble -- we're using blankIdToken!
hbox = node blankIdToken (initStyle NodeStyle) (HBoxN ∷ Node HBox)
vbox = node blankIdToken (initStyle NodeStyle) (VBoxN ∷ Node VBox)


-- * Holo
--
type instance ConsCtx  t a = (InputMux t, a)
type instance FieldCtx t a = (InputMux t, a)
data instance Derived  t a = Reflex t ⇒ W { fromW ∷ (Dynamic t Subscription, Dynamic t (a, HoloBlank)) }

type HoloBlank      = Item PBlank
type W        t   a = Derived t a
type WH       t     = (Dynamic t Subscription, Dynamic t HoloBlank)

wWH ∷ Reflex t ⇒ W t a → WH t
wWH = (id *** (snd <$>)) ∘ fromW

class (Typeable a, DefStyleOf (StyleOf a)) ⇒ Holo a where
  data VisualOf a
  data StyleOf  a
  type CLiftW   t (m ∷ Type → Type) a ∷ Constraint
  compStyle       ∷                                                                    a → StyleOf a
  compGeo         ∷                                                                    a → Geo
  hasVisual       ∷                                                              Proxy a → Bool
  liftHoloDyn     ∷ (RGLFW t m) ⇒                                      a → Event t Input → m (Dynamic t a)
  liftHoloItem    ∷                                                          IdToken → a → Item PBlank
  subscription    ∷                                                    IdToken → Proxy a → Subscription
  liftDynW        ∷ (Reflex t)  ⇒                                  IdToken → Dynamic t a → W t a
  -- liftW           ∷ (RGLFW t m, CLiftW t m a) ⇒                           InputMux t → a → m (W t a)
  liftW           ∷ (RGLFW t m) ⇒                                         InputMux t → a → m (W t a)
  query           ∷ (MonadIO m) ⇒ Port → StyleOf a →                  [Item PLayout] → a → m (Di (Maybe Double))
  createVisual    ∷ (MonadIO m) ⇒ Port → StyleOf a → Area'LU Double → Drawable →       a → m (VisualOf a)
  renderVisual    ∷ (MonadIO m) ⇒ Port →                            VisualOf a →       a → m ()  -- ^ Update a visualisation of 'a'.
  freeVisualOf    ∷ (MonadIO m) ⇒                                   VisualOf a → Proxy a → m ()
  --
  compStyle       = const defStyleOf     -- default style
  compGeo         = const mempty         -- default geometry
  hasVisual       = const False          -- no visual by default
  liftHoloDyn     = liftHoloDynStatic    -- no value change in response to events
  liftHoloItem    = liftItemStatic       -- static style and geometry
  subscription    = const mempty         -- ignore events
  liftDynW        = liftDynWStatic       -- static subscriptions
  liftW           = liftWDynamic         -- static subscriptions
-- XXX: get rid of this separation
class DefStyleOf a where
  defStyleOf      ∷ a

compToken ∷ ∀ m a. (Holo a, MonadIO m) ⇒ Proxy a → m IdToken
compToken (hasVisual → True) = newId
compToken _                  = pure blankIdToken

liftHoloDynStatic ∷ (RGLFW t m) ⇒ a → Event t Input → m (Dynamic t a)
liftHoloDynStatic init _ev = pure $ constDyn init

liftItemStatic ∷ ∀ a. (Holo a) ⇒ IdToken → a → Item PBlank
liftItemStatic tok x = leaf tok (initStyle $ compStyle x) x

liftDynWStatic ∷ ∀ t a. (Holo a, Reflex t) ⇒ IdToken → Dynamic t a → W t a
liftDynWStatic tok valD =
  W ( constDyn $ subscription tok (Proxy @a)
    , valD <&> (id &&& liftHoloItem tok))

liftWDynamic ∷ ∀ t m a. (Holo a, RGLFW t m) ⇒ InputMux t → a → m (W t a)
liftWDynamic imux initial = do
  tok ← compToken $ Proxy @a
  liftDynW tok <$> (liftHoloDyn initial $ select imux $ Const2 tok)

liftWStatic ∷ ∀ t m a c. (Holo a, RGLFW t m) ⇒ Proxy c → InputMux t → a → m (W t a)
liftWStatic _pC _imux initial = do
  tok ← compToken $ Proxy @a
  pure $ W ( constDyn $ subscription tok (Proxy @a)
           , constDyn (initial, liftHoloItem tok initial))

liftDynW' ∷ ∀ a t m. (Holo a, RGLFW t m) ⇒ Dynamic t a → m (W t a)
liftDynW' h = do
  tok ← newId
  pure $ liftDynWStatic tok h

data Visual a where
  Visual ∷ Holo a ⇒
    { vVisual     ∷ Maybe (VisualOf a)
    , vStyleGene  ∷ StyleGene
    , vDrawable   ∷ Maybe Drawable
    } → Visual a


-- * Minimal case
--
instance DefStyleOf (StyleOf ()) where
  defStyleOf           = UnitStyle
instance Holo   () where
  data StyleOf  ()     = UnitStyle
  data VisualOf ()     = UnitVisual
  compStyle        _   = UnitStyle
  query        _ _ _ _ = pure $ Di $ V2 Nothing Nothing

instance Semigroup (Item PBlank)  where _ <> _ = mempty
instance Monoid    (Item PBlank)  where mempty = Item () blankIdToken (initStyle UnitStyle) mempty [] (Di $ V2 Nothing Nothing) mempty ()
instance Semigroup (Item PLayout) where _ <> _ = mempty
instance Monoid    (Item PLayout) where mempty = Item () blankIdToken (initStyle UnitStyle) mempty [] (Di $ V2 Nothing Nothing) mempty ()

-- instance Semigroup (Item PLayout) where
--   l <> r = vbox [l, r]
-- instance Monoid    (Item PLayout) where
--   mempty      = Item () blankIdToken mempty UnitStyle [] (Di $ V2 Nothing Nothing) mempty ()
-- instance Monoid (Item Visual) where
--   mappend l r = holoVBox [l, r]
--   mempty      = Item () blankIdToken SPU mempty UnitStyle [] (Di $ V2 Nothing Nothing) mempty UnitVisual


-- * Styles
--
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


-- * Input & Subscription
--
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

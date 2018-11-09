{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
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
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors -Wno-missing-import-lists -Wno-implicit-prelude -Wno-monomorphism-restriction -Wno-name-shadowing -Wno-all-missed-specialisations -Wno-unsafe -Wno-missing-export-lists -Wno-type-defaults -Wno-partial-fields -Wno-missing-local-signatures #-}

module Holo
  ( liftWStatic, liftDynW'
  , Frame(..)
  , VPort
  --
  , Drawable(..)
  , Visual(..)
  , VisualOf(..)
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
  , Holo(..), hiStyleGene, hiHasVisual, hiLeaves
  , Item(..)
  , Phase(..), HoloBlank
  , item, node, leaf
  , hbox, vbox
  , hiQuery
  , hiRender
  , ensureHolotreeVisuals
  , renderHolotreeVisuals
  , drawHolotreeVisuals
  --
  , Derived(..), W
  , WH, wWH
  --
  , InputMux
  )
where

import           Control.Arrow
import           Data.Foldable
import           Data.Functor.Misc                        (Const2(..))
import           Data.Maybe
import           Data.Typeable
import           Generics.SOP                             (Proxy)
import           Generics.SOP.Monadic
import           GHC.Types                                (Constraint)
import           Linear
import           Reflex                            hiding (Query, Query(..))
import           Reflex.GLFW                              (RGLFW)
import "GLFW-b"  Graphics.UI.GLFW                  as GL
import qualified Data.Map.Monoidal.Strict          as MMap
import qualified Data.Map.Strict                   as Map
import qualified Data.Sequence                     as Seq
import qualified Data.Set                          as Set
import qualified Reflex.GLFW                       as GLFW

-- Local imports
import           HoloPrelude

import           Flex                                     (Geo)
import qualified Flex                              as Flex

import           Flatland
import           HoloPort                                 (IdToken, Drawable, Frame)
import qualified HoloPort                          as Port


-- * Holo: making data types interactive.
--
--   Holo a ⇒ a
--   → Dynamic t (a, Item PBlank)
--   → Dynamic t (a, Item PLayout)
--   → Dynamic t (a, Item PVisual)
--
class (Typeable a, DefStyleOf (StyleOf a)) ⇒ Holo a where
  data VisualOf a
  data StyleOf  a
  type CLiftW   t (m ∷ Type → Type) a ∷ Constraint
  compStyle       ∷                                                                     a → StyleOf a
  compGeo         ∷                                                                     a → Geo
  hasVisual       ∷                                                               Proxy a → Bool
  liftHoloDyn     ∷ (RGLFW t m) ⇒                                       a → Event t Input → m (Dynamic t a)
  liftHoloItem    ∷                                                           IdToken → a → Item PBlank
  subscription    ∷                                                     IdToken → Proxy a → Subscription
  liftDynW        ∷ (Reflex t)  ⇒                                   IdToken → Dynamic t a → W t a
  liftW           ∷ (RGLFW t m) ⇒                                          InputMux t → a → m (W t a)
  query           ∷ (MonadIO m) ⇒ VPort → StyleOf a →                  [Item PLayout] → a → m (Di (Maybe Double))
  createVisual    ∷ (MonadIO m) ⇒ VPort → StyleOf a → Area'LU Double → Drawable →       a → m (VisualOf a)
  renderVisual    ∷ (MonadIO m) ⇒ VPort →                            VisualOf a →       a → m ()  -- ^ Update a visualisation of 'a'.
  freeVisualOf    ∷ (MonadIO m) ⇒                                    Proxy a → VisualOf a → m ()
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
compToken (hasVisual → True) = Port.newId
compToken _                  = pure Port.blankIdToken

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
  tok ← Port.newId
  pure $ liftDynWStatic tok h

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

data Visual a where
  Visual ∷ Holo a ⇒
    { vVisual     ∷ Maybe (VisualOf a)
    , vStyleGene  ∷ StyleGene
    , vDrawable   ∷ Maybe Drawable
    } → Visual a

type VPort = Port.Port Visual

instance Port.PortVisual Visual where
  pvDrawable = vDrawable
  pvFree _pC = \case
    Visual{..} → sequence_ $ freeVisualOf Proxy <$> vVisual


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
  -- XXX: we're losing type safety here..
  case x of Item{holo=_holo ∷ a, ..} → hasVisual (Proxy @a)

hiLeaves ∷ Item a → Map.Map IdToken (Item a)
hiLeaves root = Map.fromList $ walk root
  where walk x@(hiChildren → []) = [(hiToken x, x)]
        walk Item{..}        = concat $ walk <$> hiChildren

hiQuery ∷ ∀ m. (MonadIO m) ⇒ VPort → Item PBlank → m (Item PLayout)
hiQuery port hoi@Item{..} =
  queryOne port hoi =<< (sequence $ hiQuery port <$> hiChildren)
  where queryOne ∷ VPort → Item PBlank → [Item PLayout] → m (Item PLayout)
        queryOne port hoi children =
          case hoi of
            Item{..} → do
              size ← query port (_sStyle $ hiStyle) children holo
              trev SIZE HOLO size (Port.tokenHash hiToken)
              pure Item{hiSize=size, hiArea=mempty, hiChildren=children, ..}

proxy ∷ a → Proxy a
proxy = const Proxy

hiEnsureVisual ∷ (HasCallStack, MonadIO m) ⇒ VPort → Item PLayout → [Item PVisual] → m (Item PVisual)
hiEnsureVisual port hi children = case hi of
  Item{..} → do
    let dim = hiArea^.area'b.size'di
    vis ← if not $ hasVisual (proxy holo)
      then pure $ Visual Nothing (_sStyleGene hiStyle) Nothing
      else Port.portEnsureVisual port dim (Proxy @Holo) hiToken Proxy (\Visual{..}→ vStyleGene ≢ hiStyleGene hi) $
           \drw→ Visual <$> (Just <$> createVisual port (_sStyle hiStyle) hiArea drw holo)
                        <*> pure (_sStyleGene $ hiStyle)
                        <*> (pure $ Just drw)
    pure Item{hiVisual=Just vis, hiChildren=children, ..}

hiRender ∷ (MonadIO m) ⇒ VPort → Item PVisual → m ()
hiRender port Item{..} = do
  case hiVisual of
    Just Visual{vDrawable=Just drw, vVisual=Just vis} → do
      Port.clearDrawable drw
      renderVisual port vis holo
      Port.drawableContentToGPU drw
    _ → pure ()



ensureHolotreeVisuals ∷ (MonadIO m) ⇒ VPort → Item PLayout → m (Item PVisual)
ensureHolotreeVisuals port hoi@Item{..} =
  hiEnsureVisual port hoi =<< (sequence $ ensureHolotreeVisuals port <$> hiChildren)

renderHolotreeVisuals ∷ (MonadIO m) ⇒ VPort → Item PVisual → m ()
renderHolotreeVisuals port hoi@Item{..} = do
  hiRender port hoi
  forM_ hiChildren (renderHolotreeVisuals port)

drawHolotreeVisuals ∷ (MonadIO m) ⇒ Frame → Item PVisual → m ()
drawHolotreeVisuals frame root = recur (luOf (hiArea root)^.lu'po) "" root
  where
    recur parOff pfx Item{..} = do
      -- liftIO $ putStrLn $ pfx <> show (offset ^.po'v) <> " " <> Flex.ppItemArea i
      let ourOff = parOff + luOf hiArea^.lu'po
      case hiVisual of
        Just Visual{vDrawable=Just drw} →
          Port.framePutDrawable frame drw (doubleToFloat <$> ourOff)
        _ → pure ()
      forM_ hiChildren $ recur ourOff (pfx <> "  ")


-- * Node
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


-- * Constructors
--
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
hbox = node Port.blankIdToken (initStyle NodeStyle) (HBoxN ∷ Node HBox)
vbox = node Port.blankIdToken (initStyle NodeStyle) (VBoxN ∷ Node VBox)


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
                                           | (Port.tokenHash → tok,(InputMask em)) ← toList subs]
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


-- * The final lift:  W(-idget)
--
type HoloBlank      = Item PBlank
type W        t   a = Derived t a
type WH       t     = (Dynamic t Subscription, Dynamic t HoloBlank)

type instance ConsCtx  t a = (InputMux t, a)
type instance FieldCtx t a = (InputMux t, a)
data instance Derived  t a = Reflex t ⇒ W { fromW ∷ (Dynamic t Subscription, Dynamic t (a, HoloBlank)) }

wWH ∷ Reflex t ⇒ W t a → WH t
wWH = (id *** (snd <$>)) ∘ fromW



-- * Concrete, minimal case, to keep us in check
--
instance DefStyleOf (StyleOf ()) where
  defStyleOf           = UnitStyle
instance Holo   () where
  data StyleOf  ()     = UnitStyle
  data VisualOf ()     = UnitVisual
  compStyle        _   = UnitStyle
  query        _ _ _ _ = pure $ Di $ V2 Nothing Nothing

instance Semigroup (Item PBlank)  where _ <> _ = mempty
instance Monoid    (Item PBlank)  where mempty = Item () Port.blankIdToken (initStyle UnitStyle) mempty [] (Di $ V2 Nothing Nothing) mempty ()
instance Semigroup (Item PLayout) where _ <> _ = mempty
instance Monoid    (Item PLayout) where mempty = Item () Port.blankIdToken (initStyle UnitStyle) mempty [] (Di $ V2 Nothing Nothing) mempty ()

-- instance Semigroup (Item PLayout) where
--   l <> r = vbox [l, r]
-- instance Monoid    (Item PLayout) where
--   mempty      = Item () blankIdToken mempty UnitStyle [] (Di $ V2 Nothing Nothing) mempty ()
-- instance Monoid (Item Visual) where
--   mappend l r = holoVBox [l, r]
--   mempty      = Item () blankIdToken SPU mempty UnitStyle [] (Di $ V2 Nothing Nothing) mempty UnitVisual

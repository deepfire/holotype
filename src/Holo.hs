{-# LANGUAGE AllowAmbiguousTypes #-}
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

module Holo
  ( liftWDynamic, liftWSeed, liftWStatic
  , Frame(..)
  , VPort
  --
  , Vis(..), compToken
  , StyleOf, Style(..), initStyle, defStyle
  , StyleGene(..), sStyle, sStyleGene, fromStyleGene
  , VisualOf, Visual(..)
  , Drawable(..)
  --
  , InputEvent(..)
  , inputMatch
  , InputEventMask(..), Subscription(..), subSingleton
  , inputMaskKeys, inputMaskButtons, inputMaskChars, editMaskKeys
  --
  , HIArea, HIVisual
  , Holo(..), hiStyleGene, hiHasVisual, hiLeaves
  , Item(..)
  , Phase(..), HoloBlank
  , item, node, leaf
  , hbox, vbox
  , hiSizeRequest
  , hiRender
  , ensureHolotreeVisuals
  , renderHolotreeVisuals
  , showHolotreeVisuals
  --
  , Structure, Result(..)
  , WH, wWH
  , Widget
  --
  , Static(..)
  --
  , InputEventMux
  )
where

import           Control.Arrow
import           Control.Newtype.Generics
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


-- * Vis: making things visible
--
type family StyleOf  a ∷ Type
type family VisualOf a ∷ Type

class Typeable a ⇒ Vis a where
  defStyleOf      ∷                                                               Proxy a → StyleOf a
  compStyleOf     ∷                                                                     a → StyleOf a
  compGeo         ∷                                                                     a → Geo
  sizeRequest     ∷ (MonadIO m) ⇒ VPort → StyleOf a →                  [Item PLayout] → a → m (Di (Maybe Double))
  setupVisual     ∷ (MonadIO m) ⇒ VPort → StyleOf a → Area'LU Double → Drawable →       a → m (VisualOf a)
  render          ∷ (MonadIO m) ⇒ VPort → StyleOf a → VisualOf a     → Drawable →       a → m ()  -- ^ Update a visualisation of 'a'.
  freeVisualOf    ∷ (MonadIO m) ⇒                                    Proxy a → VisualOf a → m ()
  --
  compStyleOf     = defStyleOf ∘ proxy   -- default style
  compGeo         = const mempty         -- default geometry


-- * Style wrapper
--
newtype StyleGene = StyleGene { _fromStyleGene ∷ Int } deriving (Eq, Ord)
fromStyleGene ∷ Lens' StyleGene Int
fromStyleGene f (StyleGene x) = f x <&> StyleGene

data Style a where
  Style ∷ Vis a ⇒
    { _sStyle      ∷ StyleOf a
    , _sStyleGene  ∷ StyleGene
    } → Style a

sStyle     ∷ Lens' (Style a) (StyleOf a)
sStyle     f s@Style{..} = f _sStyle     <&> \x→ s{_sStyle=x}
sStyleGene ∷ Lens' (Style a) StyleGene
sStyleGene f s@Style{..} = f _sStyleGene <&> \x→ s{_sStyleGene=x}

initStyle ∷ Vis a ⇒ StyleOf a → Style a
initStyle s = Style { _sStyle = s, _sStyleGene = StyleGene 0 }

defStyle ∷ ∀ a. Vis a ⇒ Style a
defStyle = initStyle $ defStyleOf (Proxy @a)


-- * Visual wrapper
--
data Visual a where
  Visual ∷ Vis a ⇒
    { vVisual   ∷ Maybe (VisualOf a)
    , vStyle    ∷ Style a
    , vDrawable ∷ Maybe Drawable
    } → Visual a

type VPort = Port.Port Visual

instance Port.PortVisual Visual where
  pvDrawable = vDrawable
  pvFree _pC pA = \case
    Visual{..} → sequence_ $ freeVisualOf pA <$> vVisual



class Input iv a where

class (Typeable a, Vis a) ⇒ Holo a where
  -- type CLiftW   t (m ∷ Type → Type) a ∷ Constraint
  hasVisual       ∷                                                               Proxy a → Bool
  liftHoloDyn     ∷ (RGLFW t m) ⇒                                  a → Event t InputEvent → m (Dynamic t a)
  liftHoloItem    ∷                                                           IdToken → a → Item PBlank
  subscription    ∷                                                     IdToken → Proxy a → Subscription
  liftDynW        ∷ (Reflex t)  ⇒                                   IdToken → Dynamic t a → Widget t a
  liftW           ∷ (RGLFW t m) ⇒                                     InputEventMux t → a → m (Widget t a)
  --
  hasVisual       = const False          -- no visual by default
  liftHoloDyn     = liftHoloDynStatic    -- no value change in response to events
  liftHoloItem    = liftItemStatic       -- static style and geometry
  subscription    = const mempty         -- ignore events
  liftDynW        = liftDynWStaticSubs
  liftW           = liftWSeed

compToken ∷ ∀ m a. (Holo a, MonadIO m) ⇒ Proxy a → m IdToken
compToken (hasVisual → True) = Port.newId
compToken _                  = pure Port.blankIdToken


-- * The final lift:  W(-idget)
--
type HoloBlank      = Item PBlank
type WH       t     = (Dynamic t Subscription, Dynamic t HoloBlank)

-- Result of the lifts -- the Widget:
type Widget     t a = Result t a
data instance Result t a
  = Reflex t ⇒ W
    { fromW ∷ (Dynamic t Subscription, Dynamic t (a, HoloBlank))
    }

wWH ∷ Reflex t ⇒ Widget t a → WH t
wWH = (id *** (snd <$>)) ∘ fromW

instance Functor (Result t) where
  fmap f (W (subs, vals)) = W (subs, (f *** id) <$> vals)

instance Reflex t ⇒ Applicative (Result t) where
  pure x = W (mempty, constDyn (x, vbox []))
  W (fsubs, fvals) <*> W (xsubs, xvals) =
    W $ (,)
    (zipDynWith (<>) fsubs xsubs)
    (zipDynWith ((\(f,   fhb)
                   (  x, xhb@Item{..})→
                   (f x, xhb { hiChildren = fhb : hiChildren })))
      fvals xvals)

-- record lifting for unchanging values
-- type instance ConsCtx  t (Static t a)  = (InputEventMux t, a)
-- type instance FieldCtx t (Static t a)  = (InputEventMux t, a)
newtype Static t a = Static a -- XXX: once we're successful with the lift, let's drop the 't'
  deriving newtype (Newtype)

-- instance {-# OVERLAPPABLE #-}
--   (Typeable a
--   , DefStyleOf (StyleOf a)
--   , ∀ xs. SOP.Code a ~ '[xs]
--   ) ⇒ Holo a where
--   type CLiftW t m a = ()
--   liftW ∷ (RGLFW t m, CLiftW t m a) ⇒ InputEventMux t → a → m (W t a)
--   liftW = liftRecord

liftItemStatic ∷ ∀ a. (Holo a) ⇒ IdToken → a → Item PBlank
liftItemStatic tok x = leaf tok (initStyle $ compStyleOf x) x

liftHoloDynStatic ∷ (RGLFW t m) ⇒ a → Event t InputEvent → m (Dynamic t a)
liftHoloDynStatic init _ev = pure $ constDyn init

liftWStatic ∷ ∀ t m a. (Holo a, RGLFW t m) ⇒ InputEventMux t → Static t a → m (Widget t a)
liftWStatic _imux (Static initial) = do
  tok ← compToken $ Proxy @a
  pure $ W ( constDyn $ subscription tok (Proxy @a)
           , constDyn (initial, liftHoloItem tok initial))

liftWSeed   ∷ ∀ t m a. (Holo a, RGLFW t m) ⇒ InputEventMux t → a → m (Widget t a)
liftWSeed imux initial = do
  tok ← compToken $ Proxy @a
  liftDynW tok <$> (liftHoloDyn initial $ select imux $ Const2 tok)

liftDynWStaticSubs ∷ ∀ t a. (Holo a, Reflex t) ⇒ IdToken → Dynamic t a → Widget t a
liftDynWStaticSubs tok valD =
  W ( constDyn $ subscription tok (Proxy @a)
    , valD <&> (id &&& liftHoloItem tok))

liftWDynamic ∷ ∀ a t m. (Holo a, RGLFW t m) ⇒ Dynamic t a → m (Widget t a)
liftWDynamic h = do
  tok ← compToken $ Proxy @a
  pure $ liftDynWStaticSubs tok h


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
    , hiSize      ∷ Di (Maybe Double) -- Flex input:  the desired size
    -- TTG-inspired phasing:
    , hiArea      ∷ HIArea p          -- Flex output: the resultant size + coords
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

hiSizeRequest ∷ ∀ m. (MonadIO m) ⇒ VPort → Item PBlank → m (Item PLayout)
hiSizeRequest port hoi@Item{..} =
  queryOne port hoi =<< (sequence $ hiSizeRequest port <$> hiChildren)
  where queryOne ∷ VPort → Item PBlank → [Item PLayout] → m (Item PLayout)
        queryOne port hoi children =
          case hoi of
            Item{..} → do
              size ← sizeRequest port (_sStyle $ hiStyle) children holo
              trev SIZE HOLO size (Port.tokenHash hiToken)
              pure Item{hiSize=size, hiArea=mempty, hiChildren=children, ..}

hiEnsureVisual ∷ (HasCallStack, MonadIO m) ⇒ VPort → Item PLayout → [Item PVisual] → m (Item PVisual)
hiEnsureVisual port hi children = case hi of
  Item{..} → do
    let dim = hiArea^.area'b.size'di
    vis ← if not $ hasVisual (proxy holo)
      then pure $ Visual Nothing hiStyle Nothing
      else Port.portEnsureVisual port dim (Proxy @Vis) hiToken Proxy (\Visual{..}→ _sStyleGene vStyle ≢ hiStyleGene hi) $
           \drw→ Visual <$> (Just <$> setupVisual port (_sStyle hiStyle) hiArea drw holo)
                        <*> pure hiStyle
                        <*> (pure $ Just drw)
    pure Item{hiVisual=Just vis, hiChildren=children, ..}

hiRender ∷ (MonadIO m) ⇒ VPort → Item PVisual → m ()
hiRender port Item{..} = do
  case hiVisual of
    Just Visual{vVisual=Just vis, vStyle=Style{..}, vDrawable=Just drw} → do
      -- XXX: 'render' is called every frame for everything
      Port.clearDrawable drw
      render port _sStyle vis drw holo
      Port.drawableContentToGPU drw
    _ → pure ()



ensureHolotreeVisuals ∷ (MonadIO m) ⇒ VPort → Item PLayout → m (Item PVisual)
ensureHolotreeVisuals port hoi@Item{..} =
  hiEnsureVisual port hoi =<< (sequence $ ensureHolotreeVisuals port <$> hiChildren)

renderHolotreeVisuals ∷ (MonadIO m) ⇒ VPort → Item PVisual → m ()
renderHolotreeVisuals port hoi@Item{..} = do
  hiRender port hoi
  forM_ hiChildren (renderHolotreeVisuals port)

showHolotreeVisuals ∷ (MonadIO m) ⇒ Frame → Item PVisual → m ()
showHolotreeVisuals frame root = recur (luOf (hiArea root)^.lu'po) "" root
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
  deriving anyclass Holo

boxAxis ∷ Node a → Axis
boxAxis HBoxN = X
boxAxis VBoxN = Y

type instance StyleOf  (Node k) = ()
type instance VisualOf (Node k) = ()

instance Typeable k ⇒ Vis (Node (k ∷ KNode)) where
  defStyleOf _           = ()
  compGeo HBoxN          = mempty & Flex.direction .~ Flex.DirRow    & Flex.align'content .~ Flex.AlignStart
  compGeo VBoxN          = mempty & Flex.direction .~ Flex.DirColumn & Flex.align'content .~ Flex.AlignStart
  sizeRequest _ _ xs box =
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
hbox = node Port.blankIdToken (initStyle ()) (HBoxN ∷ Node HBox)
vbox = node Port.blankIdToken (initStyle ()) (VBoxN ∷ Node VBox)


-- * InputEvent & Subscription
--
type InputEventMux t     = EventSelector t (Const2 IdToken InputEvent)

data InputEvent where
  InputEvent ∷
    { inInputEvent ∷ GLFW.InputU
    } → InputEvent
  deriving (Show)

data InputEventMask where
  InputEventMask ∷
    { inputMask ∷ GLFW.EventMask
    } → InputEventMask
  deriving (Eq, Ord)
instance Show InputEventMask where
  show InputEventMask{..} = ("(IM "<>) ∘ (<>")") $ show inputMask

inputMatch ∷ InputEventMask → InputEvent → Bool
inputMatch InputEventMask{..} = \case
  InputEvent{inInputEvent=GLFW.U x} → GLFW.eventMatch inputMask x

instance Semigroup InputEventMask where
  InputEventMask a <> InputEventMask b = InputEventMask $ a <> b
instance Monoid InputEventMask where
  mempty = InputEventMask mempty

newtype Subscription = Subscription (MMap.MonoidalMap GLFW.EventType (Seq.Seq (IdToken, InputEventMask)))
instance Show Subscription where
  show (Subscription map) = ("(Subs"<>) ∘ (<>")") $ concat $
    [ " "<>show et<>"::"<> intercalate "+" [ printf "0x%x:%s" tok (show em)
                                           | (Port.tokenHash → tok,(InputEventMask em)) ← toList subs]
    | (et, subs) ← MMap.toList map]

instance Semigroup Subscription where
  Subscription a <> Subscription b = Subscription $ a <> b

deriving newtype instance Monoid Subscription

subSingleton ∷ IdToken → InputEventMask → Subscription
subSingleton tok im@(InputEventMask em) = Subscription $
  MMap.fromList [ (evty, Seq.singleton (tok, im))
                | evty ← GLFW.eventMaskTypes em ]

inputMaskKeys    ∷ Set.Set GL.Key → Set.Set GL.KeyState → GL.ModifierKeys → InputEventMask
-- inputMaskKeys = InputEventMask ∘ GLFW.eventMaskKeys .:: GLFW.KeyEventMask
inputMaskKeys ks kss mks = InputEventMask $ GLFW.eventMaskKeys $ GLFW.KeyEventMask ks kss mks

inputMaskChars   ∷ InputEventMask
inputMaskChars   = InputEventMask $ GLFW.eventMaskChars

inputMaskButtons ∷ GLFW.ButtonEventMask → InputEventMask
inputMaskButtons = InputEventMask ∘ GLFW.eventMaskButtons

editMaskKeys ∷ InputEventMask
editMaskKeys = (inputMaskChars <>) $ InputEventMask $ GLFW.eventMaskKeys $ GLFW.KeyEventMask
  (Set.fromList
   [ GL.Key'Up, GL.Key'Down, GL.Key'Left, GL.Key'Right, GL.Key'Home, GL.Key'End
   , GL.Key'Backspace, GL.Key'Delete, GL.Key'Enter
   ])
  (Set.fromList [GL.KeyState'Pressed, GL.KeyState'Repeating])
  (mempty)



-- * Concrete, minimal case, to keep us in check
--
type instance StyleOf () = ()
instance Vis () where
  sizeRequest _ _ _ _ = pure $ Di $ V2 Nothing Nothing
instance Holo () where

instance Semigroup (Item PBlank)  where _ <> _ = mempty
instance Monoid    (Item PBlank)  where mempty = Item () Port.blankIdToken (initStyle ()) mempty [] (Di $ V2 Nothing Nothing) mempty ()
instance Semigroup (Item PLayout) where _ <> _ = mempty
instance Monoid    (Item PLayout) where mempty = Item () Port.blankIdToken (initStyle ()) mempty [] (Di $ V2 Nothing Nothing) mempty ()

-- instance Semigroup (Item PLayout) where
--   l <> r = vbox [l, r]
-- instance Monoid    (Item PLayout) where
--   mempty      = Item () blankIdToken mempty UnitStyle [] (Di $ V2 Nothing Nothing) mempty ()
-- instance Monoid (Item Visual) where
--   mappend l r = holoVBox [l, r]
--   mempty      = Item () blankIdToken SPU mempty UnitStyle [] (Di $ V2 Nothing Nothing) mempty UnitVisual

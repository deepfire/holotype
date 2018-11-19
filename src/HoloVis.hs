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

module HoloVis
  ( Vis(..)
  , StyleOf
  , Style(..), sStyle, sStyleGene, initStyle, defStyle
  , StyleGene(..), fromStyleGene
  , VisualOf, Visual(..)
  --
  , HIArea, HIVisual
  , Item(..), hiStyleGene, hiLeaves
  , Phase(..)
  , Node(..)
  , item, node, leaf
  , hbox, vbox
  , hiSizeRequest
  , hiMandateVisual, hiUnvisual
  , hiRender
  , renderHolotreeVisuals
  , showHolotreeVisuals
  --
  , Drawable(..)
  , VPort
  )
where

import           Data.Foldable
import           Data.Maybe
import           Data.Typeable
import           Generics.SOP                             (Proxy)
import           GHC.Types                                (Constraint)
import           Linear
import qualified Data.Map.Strict                   as Map

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
  sizeRequest     ∷ (MonadIO m, c a) ⇒ VPort → StyleOf a →           [Item c PLayout] → a → m (Di (Maybe Double))
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

data Item (c ∷ Type → Constraint) (p ∷ Phase) where
  Item ∷ ∀ p c a. (c a, Vis a) ⇒
    { holo        ∷ a
    , hiConstr    ∷ Proxy c
    , hiToken     ∷ IdToken
    , hiStyle     ∷ Style a
    , hiGeo       ∷ Geo
    , hiChildren  ∷ [Item c p]
    , hiSize      ∷ Di (Maybe Double) -- Flex input:  the desired size
    -- TTG-inspired phasing:
    , hiArea      ∷ HIArea p          -- Flex output: the resultant size + coords
    , hiVisual    ∷ HIVisual p a
    } → Item c p

instance Eq (Item c a) where
  (==) a b = (≡) (hiToken a) (hiToken b)

instance Ord (Item c a) where
  compare a b = compare (hiToken a) (hiToken b)

instance Flex.Flex (Item c PBlank) where
  geo      f hi@Item{..} = (\x→ hi {hiGeo=x})       <$> f hiGeo
  size     f hi@Item{..} = (\x→ hi {hiSize=x})      <$> f hiSize
  children f hi@Item{..} = (\x→ hi {hiChildren=x})  <$> f hiChildren
  area     f hi@Item{..} = (\_→ hi {hiArea=mempty}) <$> f mempty

instance Flex.Flex (Item c PLayout) where
  geo      f hi@Item{..} = (\x→ hi {hiGeo=x})      <$> f hiGeo
  size     f hi@Item{..} = (\x→ hi {hiSize=x})     <$> f hiSize
  children f hi@Item{..} = (\x→ hi {hiChildren=x}) <$> f hiChildren
  area     f hi@Item{..} = (\x→ hi {hiArea=x})     <$> f hiArea

instance Flex.Flex (Item c PVisual) where
  geo      f hi@Item{..} = (\x→ hi {hiGeo=x})      <$> f hiGeo
  size     f hi@Item{..} = (\x→ hi {hiSize=x})     <$> f hiSize
  children f hi@Item{..} = (\x→ hi {hiChildren=x}) <$> f hiChildren
  area     f hi@Item{..} = (\x→ hi {hiArea=x})     <$> f hiArea

hiStyleGene ∷ Item c p → StyleGene
hiStyleGene x =
  case x of Item{..} → _sStyleGene hiStyle

hiLeaves ∷ Item c a → Map.Map IdToken (Item c a)
hiLeaves root = Map.fromList $ walk root
  where walk x@(hiChildren → []) = [(hiToken x, x)]
        walk Item{..}        = concat $ walk <$> hiChildren

hiSizeRequest ∷ ∀ c m. (MonadIO m) ⇒ VPort → Item c PBlank → m (Item c PLayout)
hiSizeRequest port hoi@Item{..} =
  queryOne port hoi =<< (sequence $ hiSizeRequest port <$> hiChildren)
  where queryOne ∷ VPort → Item c PBlank → [Item c PLayout] → m (Item c PLayout)
        queryOne port hoi children =
          case hoi of
            Item{..} → do
              size ← sizeRequest port (_sStyle $ hiStyle) children holo
              trev SIZE HOLO size (Port.tokenHash hiToken)
              pure Item{hiSize=size, hiArea=mempty, hiChildren=children, ..}

hiMandateVisual ∷ (HasCallStack, MonadIO m) ⇒ VPort → Item c PLayout → [Item c PVisual] → m (Item c PVisual)
hiMandateVisual port hi children = case hi of
  Item{..} → do
    let dim = hiArea^.area'b.size'di
    vis ←  Port.portEnsureVisual port dim (Proxy @Vis) hiToken Proxy (\Visual{..}→ _sStyleGene vStyle ≢ hiStyleGene hi) $
           \drw→ Visual <$> (Just <$> setupVisual port (_sStyle hiStyle) hiArea drw holo)
                        <*> pure hiStyle
                        <*> (pure $ Just drw)
    pure Item{hiVisual=Just vis, hiChildren=children, ..}

hiUnvisual ∷ Item c PLayout → [Item c PVisual] → Item c PVisual
hiUnvisual hi children = case hi of
  Item{..} → Item{hiVisual=Just (Visual Nothing hiStyle Nothing), hiChildren=children,..}

hiRender ∷ (MonadIO m) ⇒ VPort → Item c PVisual → m ()
hiRender port Item{..} = do
  case hiVisual of
    Just Visual{vVisual=Just vis, vStyle=Style{..}, vDrawable=Just drw} → do
      -- XXX: 'render' is called every frame for everything
      Port.clearDrawable drw
      render port _sStyle vis drw holo
      Port.drawableContentToGPU drw
    _ → pure ()


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

type instance StyleOf  (Node _) = ()
type instance VisualOf (Node _) = ()

instance Typeable k ⇒ Vis (Node (k ∷ KNode)) where
  defStyleOf _           = ()
  compGeo HBoxN          = mempty & Flex.direction .~ Flex.DirRow    & Flex.align'content .~ Flex.AlignStart
  compGeo VBoxN          = mempty & Flex.direction .~ Flex.DirColumn & Flex.align'content .~ Flex.AlignStart
  sizeRequest _ _ xs box =
    -- Requirement is a sum of children requirements
    pure $ (Just <$>) $ _reqt'di $ foldl' (reqt'add $ boxAxis box) zero $ (\x→ Reqt (fromMaybe 0 <$> x^.Flex.size)) <$> xs


-- * Constructors
--
item ∷ ∀ c a. (c a, Vis a)
  ⇒ IdToken
  → Style a
  → a
  → Geo
  → [Item c PBlank]
  → Item c PBlank
item hiToken hiStyle holo hiGeo hiChildren =
  let hiSize   = Di (V2 Nothing Nothing)
      hiArea   = ()
      hiVisual = ()
  in Item{..}

node ∷ ∀ a c k. (c a, a ~ Node k, Typeable k)
  ⇒ IdToken
  → Style a
  → a
  → [Item c PBlank]
  → Item c PBlank
node tok sty holo = item tok sty holo (compGeo holo)

leaf ∷ (c a, Vis a)
  ⇒ IdToken
  → Style a
  → a
  → Item c PBlank
leaf tok sty holo = item tok sty holo (compGeo holo) []

-- XXX: here's trouble -- we're using blankIdToken!
hbox ∷ (c (Node 'HBox)) ⇒ [Item c PBlank] → Item c PBlank
vbox ∷ (c (Node 'VBox)) ⇒ [Item c PBlank] → Item c PBlank
hbox = node Port.blankIdToken (initStyle ()) (HBoxN ∷ Node HBox)
vbox = node Port.blankIdToken (initStyle ()) (VBoxN ∷ Node VBox)


-- * Tree-wise ops
--
renderHolotreeVisuals ∷ (MonadIO m) ⇒ VPort → Item c PVisual → m ()
renderHolotreeVisuals port hoi@Item{..} = do
  hiRender port hoi
  forM_ hiChildren (renderHolotreeVisuals port)

showHolotreeVisuals ∷ (MonadIO m) ⇒ Frame → Item c PVisual → m ()
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



-- * Concrete, minimal case, to keep us in check
--
type instance StyleOf () = ()
instance Vis () where
  sizeRequest _ _ _ _ = pure $ Di $ V2 Nothing Nothing

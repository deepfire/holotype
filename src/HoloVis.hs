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
  ( As(..)
  , Style(..), sStyle, sStyleGene, initStyle, defStyle
  , StyleGene(..), fromStyleGene
  , Visual(..)
  -- , Vis(..)
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


-- * As -- assigning representation
--
class As r where
  type          Sty r ∷ Type
  type instance Sty r = ()
  type          Vis r ∷ Type
  type instance Vis r = ()
  defSty      ∷                                                           Proxy r → Sty r
  compSty     ∷                                                                 r → Sty r
  compSty                                                                       x = defSty (proxy x)
  compGeo     ∷                     r                                             → Geo
  compGeo                                                                         = const mempty
  sizeRequest ∷ MonadIO m ⇒ VPort → r → Sty r                                     → m (Di (Maybe Double))
  setupVis    ∷ MonadIO m ⇒ VPort → r → Sty r → Area'LU Double → Drawable         → m (Vis r)
  render      ∷ MonadIO m ⇒ VPort → r → Sty r                  → Drawable → Vis r → m () -- ^ Update visual.
  freeVis     ∷ MonadIO m ⇒   Proxy r                                     → Vis r → m ()
  freeVis                           _                                           _ = pure ()

-- * Concrete, minimal case, to keep us in check
instance As () where
  defSty                             _px    = ()
  compSty                                _x = ()
  compGeo                                _x = mempty
  sizeRequest  _port _x  _sty               = pure $ Di $ V2 Nothing Nothing
  setupVis     _port _x  _sty _area _drw    = pure ()
  render         _port _x  _sty _vis  _drw  = pure ()
  freeVis             _px       _vis        = pure ()


-- Note [Granularity and composite structures]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Notions of granularity:
--  - g. of input events -- drawables, easily, because of picking (unless we're willing to introduce complicated mapping schemes)
--  - g. of visual effects -- drawables, again, because of shaders
--  - g. of values -- minimally decorated entry "widgets", in a narrow sense of a widget
--                    ..as well as their composition (Record..) ?
--
-- data Composite s a where
--   Composite ∷ (Vis a, Vis (Composite s a)) ⇒
--     { cValue     ∷ a
--     , cStyle     ∷ Style (Composite s a)
--     , cStructure ∷ Item Vis PBlank       -- internal structure
--     , cToken     ∷ IdToken               -- identity of the backing drawable
--     } → Composite s a
-- Questions:
-- 1. How does it integrate with the old compose/layout/render/show workflow?
--    ..in particular, how does Query continues to work?
-- 2. How should we supply values / obtain them?
--    ..most every Item used to correspond to a Dynamic value,
--      so same should be somehow true for Composite?

-- * Goal
--
-- Minimum extension over Vis, to allow:
-- 1. text entry
-- 2. …as an abstract composite:
--    - literal text entry defined by isolated code
--    - label defined separately, yet reuses text's vis (but not necessarily style)
--    - frame defined separately
-- 3. a single drawable to be used
--    …ergo, the token is per-composite
--    …ergo, the Vis API shrinks even further
-- 4. partial redraws, at least in cases where no internal geometry changes
-- 5. with its own style
-- 6. lifecycle defined as incremental changes driven by FRP Event updates, that reuse structure


-- * Style wrapper
--
newtype StyleGene = StyleGene { _fromStyleGene ∷ Int } deriving (Eq, Ord)
fromStyleGene ∷ Lens' StyleGene Int
fromStyleGene f (StyleGene x) = f x <&> StyleGene

data Style a where
  Style ∷ As a ⇒
    { _sStyle      ∷ Sty a
    , _sStyleGene  ∷ StyleGene
    } → Style a

sStyle     ∷ Lens' (Style a) (Sty a)
sStyle     f s@Style{..} = f _sStyle     <&> \x→ s{_sStyle=x}
sStyleGene ∷ Lens' (Style a) StyleGene
sStyleGene f s@Style{..} = f _sStyleGene <&> \x→ s{_sStyleGene=x}

initStyle ∷ As a ⇒ Sty a → Style a
initStyle s = Style { _sStyle = s, _sStyleGene = StyleGene 0 }

defStyle ∷ ∀ a. As a ⇒ Style a
defStyle = initStyle $ defSty (Proxy @a)


-- * Visual wrapper
--
data Visual a where
  Visual ∷ As a ⇒
    { vVisual   ∷ Maybe (Vis a)
    , vStyle    ∷ Style a
    , vDrawable ∷ Maybe Drawable
    } → Visual a

type VPort = Port.Port Visual

instance Port.PortVisual Visual where
  pvDrawable = vDrawable
  pvFree _pC pA = \case
    Visual{..} → sequence_ $ freeVis pA <$> vVisual


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
  Item ∷ ∀ p c a. (c a, As a, Flex.Flex a, Typeable a) ⇒
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
              size ← sizeRequest port holo (_sStyle $ hiStyle)
              trev SIZE HOLO size (Port.tokenHash hiToken)
              pure Item{hiSize=size, hiArea=mempty, hiChildren=children, ..}

hiMandateVisual ∷ (HasCallStack, MonadIO m) ⇒ VPort → Item c PLayout → [Item c PVisual] → m (Item c PVisual)
hiMandateVisual port hi children = case hi of
  Item{..} → do
    let dim = hiArea^.area'b.size'di
    vis ←  Port.portEnsureVisual port dim (Proxy @As) hiToken Proxy (\Visual{..}→ _sStyleGene vStyle ≢ hiStyleGene hi) $
           \drw→ Visual <$> (Just <$> setupVis port holo (_sStyle hiStyle) hiArea drw)
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
      render port holo _sStyle drw vis
      Port.drawableContentToGPU drw
    _ → pure ()


-- * Node
--
data KNode
  = VBox
  | HBox

data Node (k ∷ KNode) where
  HBoxN ∷ (As r, Flex.Flex r) ⇒ [r] → Node HBox
  VBoxN ∷ (As r, Flex.Flex r) ⇒ [r] → Node VBox

boxAxis ∷ Node a → Axis
boxAxis (HBoxN _) = X
boxAxis (VBoxN _) = Y

instance As (Node (k ∷ KNode)) where
  type instance Sty (Node k) = ()
  type instance Vis (Node k) = ()
  defSty _          = ()
  compGeo (HBoxN _) = mempty & Flex.direction .~ Flex.DirRow    & Flex.align'content .~ Flex.AlignStart
  compGeo (VBoxN _) = mempty & Flex.direction .~ Flex.DirColumn & Flex.align'content .~ Flex.AlignStart
  sizeRequest _ box@(HBoxN xs) _ =
    -- Requirement is a sum of children requirements
    pure $ (Just <$>) $ _reqt'di $ foldl' (reqt'add $ boxAxis box) zero $ (\x→ Reqt (fromMaybe 0 <$> x^.Flex.size)) <$> xs


-- * Constructors
--
item ∷ ∀ c a. (c a, As a, Flex.Flex a, Typeable a)
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

node ∷ ∀ a c k. (c a, a ~ Node k, Flex.Flex a, Typeable k)
  ⇒ IdToken
  → Style a
  → a
  → [Item c PBlank]
  → Item c PBlank
node tok sty holo = item tok sty holo (compGeo holo)

leaf ∷ (c a, As a, Flex.Flex a, Typeable a)
  ⇒ IdToken
  → Style a
  → a
  → Item c PBlank
leaf tok sty holo = item tok sty holo (compGeo holo) []

-- XXX: here's trouble -- we're using blankIdToken!
hbox ∷ (c (Node 'HBox)) ⇒ [Item c PBlank] → Item c PBlank
vbox ∷ (c (Node 'VBox)) ⇒ [Item c PBlank] → Item c PBlank
hbox = node Port.blankIdToken (initStyle ()) ∘ (HBoxN ∷ r → Node HBox)
vbox = node Port.blankIdToken (initStyle ()) ∘ (VBoxN ∷ r → Node VBox)


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

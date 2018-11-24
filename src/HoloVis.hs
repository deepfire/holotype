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
  type      Denoted r ∷ Type
  type          Sty r ∷ Type
  type instance Sty r = ()
  type          Vis r ∷ Type
  type instance Vis r = ()
  defSty      ∷               Proxy r             → Sty r
  compSty     ∷                     r             → Sty r
  compSty                           x             = defSty (proxy x)
  sizeRequest ∷ MonadIO m ⇒ VPort → r → Denoted r → Sty r → m (Di (Maybe Double))
  setupVis    ∷ MonadIO m ⇒ VPort → r → Denoted r → Sty r → Area'LU Double → Drawable → m (Vis r)
  render      ∷ MonadIO m ⇒ VPort → r → Denoted r → Sty r                  → Drawable → Vis r → m () -- ^ Update visual.
  freeVis     ∷ MonadIO m ⇒   Proxy r                                                 → Vis r → m ()
  freeVis                           _                                                       _ = pure ()

-- * Concrete, minimal case, to keep us in check
instance As () where
  type Denoted () = ()
  type     Sty () = ()
  type     Vis () = ()
  defSty            _px   = ()
  compSty            _x   = ()
  sizeRequest  _port () () _sty = pure $ Di $ V2 Nothing Nothing
  setupVis     _port () () _sty _area      _drw = pure ()
  render       _port () () _sty       _vis _drw = pure ()
  freeVis           _px               _vis      = pure ()


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
    { vVisual   ∷ Vis a
    , vDrawable ∷ Drawable
    } → Visual a

type VPort = Port.Port Visual

instance Port.PortVisual Visual where
  pvDrawable = vDrawable
  pvFree _pC pA = \case
    Visual{..} → freeVis pA vVisual


-- * Item
--
data Phase
  = PBlank
  | PLayout
  | PVisual

type family HISize   (p ∷ Phase) ∷ Type where
  HISize   PBlank  = ()
  HISize   PLayout = Di (Maybe Double)
  HISize   PVisual = Di (Maybe Double)

type family HIArea   (p ∷ Phase) ∷ Type where
  HIArea   PBlank  = ()
  HIArea   PLayout = Area'LU Double
  HIArea   PVisual = Area'LU Double

type family HIVisual (p ∷ Phase) a ∷ Type where
  HIVisual PBlank  _ = ()
  HIVisual PLayout _ = ()
  HIVisual PVisual a = Maybe (Visual a)

data Name a where
  Name ∷ ∀ a. (As a, Typeable a) ⇒
    { nToken     ∷ IdToken
    , nStyle     ∷ Style a
    , nGeo       ∷ Geo
    , n          ∷ a
    } → Name a

-- Phasing is TTG-inspired
data Item (p ∷ Phase) where
  Leaf ∷ ∀ p a. (As a, Typeable a) ⇒
    { name        ∷ Name a
    , denoted     ∷ Denoted a
    , iSize       ∷ HISize p          -- Flex input:  the desired size
    , iArea       ∷ HIArea p          -- Flex output: the resultant size + coords
    , lVisual     ∷ HIVisual p a
    } → Item p
  Node ∷ ∀ p a. (As a, Typeable a) ⇒
    { name        ∷ Name a
    , denoted     ∷ Denoted a
    , iSize       ∷ HISize p          -- Flex input:  the desired size
    , iArea       ∷ HIArea p          -- Flex output: the resultant size + coords
    } → Item p

iLeafP ∷ Item p → Bool
iLeafP = \case
  Leaf{..} → True
  _        → False

iToken ∷ Item p → IdToken
iToken = \case
  Leaf{name=Name{..},..} → nToken
  Node{name=Name{..},..} → nToken

iStyleGene ∷ Item p → StyleGene
iStyleGene = \case
  Leaf{name=Name{..},..} → _sStyleGene nStyle
  Node{name=Name{..},..} → _sStyleGene nStyle
  -- case x of Item{..} → case hiName of Name{..} → _sStyleGene nStyle

instance Eq (Item a) where
  (==)    a b = iToken a ≡ iToken b

instance Ord (Item a) where
  compare a b = iToken a `compare` iToken b

instance Flex.Flex (Item PLayout) where
  geo      f    Leaf{..} = (\x→ Leaf {name=name {nGeo = x}, ..}) <$> f (nGeo name)
  geo      f    Node{..} = (\x→ Node {name=name {nGeo = x}, ..}) <$> f (nGeo name)
  size     f hi@Leaf{..} = (\x→ hi   {iSize=x})                  <$> f iSize
  size     f hi@Node{..} = (\x→ hi   {iSize=x})                  <$> f iSize
  area     f hi@Leaf{..} = (\x→ hi   {iArea=x})                  <$> f iArea
  area     f hi@Node{..} = (\x→ hi   {iArea=x})                  <$> f iArea
  children f hi@Leaf{..} = (\x→ hi)                              <$> f []
  children f hi@Node{..} = (\x→ hi   {nChildren=x})              <$> f nChildren

instance Flex.Flex (Item PVisual) where
  geo      f    Leaf{..} = (\x→ Leaf {name=name {nGeo = x}, ..}) <$> f (nGeo name)
  geo      f    Node{..} = (\x→ Node {name=name {nGeo = x}, ..}) <$> f (nGeo name)
  size     f hi@Leaf{..} = (\x→ hi   {iSize=x})                  <$> f iSize
  size     f hi@Node{..} = (\x→ hi   {iSize=x})                  <$> f iSize
  area     f hi@Leaf{..} = (\x→ hi   {iArea=x})                  <$> f iArea
  area     f hi@Node{..} = (\x→ hi   {iArea=x})                  <$> f iArea
  children f hi@Leaf{..} = (\x→ hi)                              <$> f []
  children f hi@Node{..} = (\x→ hi   {nChildren=x})              <$> f nChildren

hiLeaves ∷ Item a → Map.Map IdToken (Item a)
hiLeaves root = Map.fromList $ walk root
  where walk x@Leaf{..} = [(iToken x, x)]
        walk   Node{..} = concat $ walk <$> nChildren

hiSizeRequest ∷ ∀ c m. (MonadIO m) ⇒ VPort → Item PBlank → m (Item PLayout)
hiSizeRequest port Leaf{name=name@Name{..},..} = do
  size ← sizeRequest port n denoted (_sStyle $ nStyle)
  trev SIZE HOLO size $ Port.tokenHash nToken
  pure Leaf{iSize=(size ∷ Di (Maybe Double)), iArea=mempty, ..}
hiSizeRequest port hoi@Node{name=Name{..},..} =
  queryOne port hoi =<< (sequence $ hiSizeRequest port <$> nChildren)
  where queryOne ∷ VPort → Item PBlank → [Item PLayout] → m (Item PLayout)
        queryOne port hoi children =
          case hoi of
            Node{name=name@Name{..},..} → do
              size ← sizeRequest port n denoted (_sStyle $ nStyle)
              trev SIZE HOLO size (Port.tokenHash nToken)
              pure Node{iSize=size, iArea=mempty, nChildren=children, ..}

-- hiMandateVisual ∷ (HasCallStack, MonadIO m) ⇒ VPort → Item PLayout → [Item PVisual] → m (Item PVisual)
-- hiMandateVisual port hi children = case hi of
--   Item{..} → do
--     let dim = hiArea^.area'b.size'di
--     vis ←  Port.portEnsureVisual port dim (Proxy @As) hiToken Proxy (\Visual{..}→ _sStyleGene vStyle ≢ hiStyleGene hi) $
--            \drw→ Visual <$> (Just <$> setupVis port holo (_sStyle hiStyle) hiArea drw)
--                         <*> pure hiStyle
--                         <*> (pure $ Just drw)
--     pure Item{hiVisual=Just vis, hiChildren=children, ..}

-- hiUnvisual ∷ Item PLayout → [Item PVisual] → Item PVisual
-- hiUnvisual hi children = case hi of
--   Item{..} → Item{hiVisual=Nothing, hiChildren=children,..}

-- hiRender ∷ (MonadIO m) ⇒ VPort → Item PVisual → m ()
-- hiRender port Item{..} = do
--   case hiVisual of
--     Just Visual{vVisual=Just vis, vDrawable=Just drw} → do
--       -- XXX: 'render' is called every frame for everything
--       Port.clearDrawable drw
--       render port holo _sStyle drw vis
--       Port.drawableContentToGPU drw
--     _ → pure ()


-- * Node
--
data KNode
  = VBox
  | HBox

data Node (k ∷ KNode) (p ∷ Phase) where
  HBoxN ∷ Node HBox p
  VBoxN ∷ Node VBox p

boxAxis ∷ Node a p → Axis
boxAxis HBoxN = X
boxAxis VBoxN = Y

nodeGeo ∷ Node k p → Geo
nodeGeo HBoxN = mempty & Flex.direction .~ Flex.DirRow    & Flex.align'content .~ Flex.AlignStart
nodeGeo VBoxN = mempty & Flex.direction .~ Flex.DirColumn & Flex.align'content .~ Flex.AlignStart

instance As (Node (k ∷ KNode) (p ∷ Phase)) where
  type Denoted (Node k p) = [Item p]
  type Sty     (Node k p) = ()
  type Vis     (Node k p) = ()
  defSty        _         = ()
  -- comnGeo (HBoxN _) = mempty & Flex.direction .~ Flex.DirRow    & Flex.align'content .~ Flex.AlignStart
  -- comnGeo (VBoxN _) = mempty & Flex.direction .~ Flex.DirColumn & Flex.align'content .~ Flex.AlignStart
  sizeRequest _ box chi _ =
    -- Requirement is a sum of children requirements
    pure $ (Just <$>) $ _reqt'di $ foldl' (reqt'add $ boxAxis box) zero $ (\x→ Reqt (fromMaybe 0 <$> x^.Flex.size)) <$> chi


-- * Constructors
--
node ∷ ∀ a k. (a ~ Node k PBlank, Flex.Flex a, Typeable k)
  ⇒ IdToken
  → Style a
  → a
  → [Item PBlank]
  → Item PBlank
node tok sty i chi = Node (Name tok sty (nodeGeo i) i) chi () ()

leaf ∷ (As a, Flex.Flex a, Typeable a)
  ⇒ IdToken
  → Style a
  → a
  → Denoted a
  → Item PBlank
leaf tok sty name denoted = Leaf (Name tok sty mempty name) denoted () () ()

-- XXX: here's trouble -- we're using blankIdToken!
hbox ∷ [Item PBlank] → Item PBlank
vbox ∷ [Item PBlank] → Item PBlank
hbox = node Port.blankIdToken (initStyle ()) ∘ (HBoxN ∷ r → Node HBox)
vbox = node Port.blankIdToken (initStyle ()) ∘ (VBoxN ∷ r → Node VBox)


-- * Tree-wise ops
--
renderHolotreeVisuals ∷ (MonadIO m) ⇒ VPort → Item PVisual → m ()
renderHolotreeVisuals port hoi@Leaf{..} = do
  hiRender port hoi
renderHolotreeVisuals port Node{..} = do
  forM_ nChildren (renderHolotreeVisuals port)

showHolotreeVisuals ∷ (MonadIO m) ⇒ Frame → Item PVisual → m ()
showHolotreeVisuals frame root = recur (luOf (hiArea root)^.lu'po) "" root
  where
    recur parOff pfx Leaf{..} = do
      let ourOff = parOff + luOf hiArea^.lu'po
      case hiVisual of
        Just Visual{vDrawable=Just drw} →
          Port.framePutDrawable frame drw (doubleToFloat <$> ourOff)
        _ → pure ()
    recur parOff pfx Node{..} = do
      -- liftIO $ putStrLn $ pfx <> show (offset ^.po'v) <> " " <> Flex.ppItemArea i
      let ourOff = parOff + luOf iArea^.lu'po
      forM_ nChildren $ recur ourOff (pfx <> "  ")

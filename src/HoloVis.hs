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
  --
  , Name(..)
  , IVisual
  , Phase(..)
  , Item(..), iLeafP, iToken, iStyleGene
  , Node(..)
  , node, leaf
  , hbox, vbox
  , iSizeRequest
  , iMandateVisual, iUnvisual
  , iRender
  --
  , treeLeaves
  , renderTreeVisuals
  , showTreeVisuals
  --
  , Drawable(..)
  , VPort
  )
where

import           Data.Foldable
import           Data.Maybe
import           Data.Typeable
import           Generics.SOP                             (Proxy)
import           Linear
import qualified Data.Map.Strict                   as Map
import qualified Unsafe.Coerce                     as Co

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


-- * Style wrapper
--
newtype StyleGene = StyleGene { _fromStyleGene ∷ Int } deriving (Eq, Ord)
fromStyleGene ∷ Lens' StyleGene Int
fromStyleGene f (StyleGene x) = f x <&> StyleGene

data Style a where
  Style ∷
    { _sStyle      ∷ Sty a
    , _sStyleGene  ∷ StyleGene
    } → Style a

sStyle     ∷ Lens' (Style a) (Sty a)
sStyle     f s@Style{..} = f _sStyle     <&> \x→ s{_sStyle=x}
sStyleGene ∷ Lens' (Style a) StyleGene
sStyleGene f s@Style{..} = f _sStyleGene <&> \x→ s{_sStyleGene=x}

initStyle ∷ Sty a → Style a
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

type family IVisual (p ∷ Phase) a ∷ Type where
  IVisual PBlank  _ = ()
  IVisual PLayout _ = ()
  IVisual PVisual a = Maybe (Visual a)

data Name a where
  Name ∷
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
    , iSize       ∷ Di (Maybe Double) -- Flex input:  the desired size
                                      -- Has to be always defined, because we need an universally quantified Flex (Item p)
    , iArea       ∷ Area'LU Double    -- Flex output: the resultant size + coords
                                      -- Same quip as for iSize
    , lVisual     ∷ IVisual p a
    } → Item p
  Node ∷ ∀ k p a. (a ~ Node k p) ⇒
    { name        ∷ Name a
    , denoted     ∷ Denoted a
    , iSize       ∷ Di (Maybe Double) -- Flex input:  the desired size
    , iArea       ∷ Area'LU Double    -- Flex output: the resultant size + coords
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

blankSize ∷ Di (Maybe Double)
blankSize = unsafe'di Nothing Nothing

instance Eq (Item a) where
  (==)    a b = iToken a ≡ iToken b

instance Ord (Item a) where
  compare a b = iToken a `compare` iToken b

instance Flex.Flex (Item (a ∷ Phase)) where
  geo      f   Leaf{..} = (\x→ Leaf {name=name {nGeo = x}, ..}) <$> f (nGeo name)
  geo      f   Node{..} = (\x→ Node {name=name {nGeo = x}, ..}) <$> f (nGeo name)
  size     f i@Leaf{..} = (\x→ i    {iSize=x})                  <$> f iSize
  size     f i@Node{..} = (\x→ i    {iSize=x})                  <$> f iSize
  area     f i@Leaf{..} = (\x→ i    {iArea=x})                  <$> f iArea
  area     f i@Node{..} = (\x→ i    {iArea=x})                  <$> f iArea
  children f i@Leaf{..} = (\_→ i)                               <$> f []
  children f   Node{..} = (\x→ Node {denoted=x, ..})            <$> f denoted

iSizeRequest ∷ ∀ m. (MonadIO m) ⇒ VPort → Item PBlank → m (Item PLayout)
iSizeRequest port Leaf{name=name@Name{..},..} = do
  size ← sizeRequest port n denoted (_sStyle $ nStyle)
  trev SIZE HOLO size $ Port.tokenHash nToken
  pure Leaf{iSize=(size ∷ Di (Maybe Double)), iArea=mempty, ..}
iSizeRequest port hoi@Node{name=Name{..},..} =
  queryOne port hoi =<< (sequence $ iSizeRequest port <$> denoted)
  where queryOne ∷ VPort → Item PBlank → [Item PLayout] → m (Item PLayout)
        queryOne port hoi children =
          case hoi of
            Node{name=name@Name{..},..} → do
              size ← sizeRequest port n denoted (_sStyle $ nStyle)
              trev SIZE HOLO size (Port.tokenHash nToken)
              pure Node{name=nodeNameBtoL name, iSize=size, iArea=mempty, denoted=children, ..}

iMandateVisual ∷ (HasCallStack, MonadIO m) ⇒ VPort → Item PLayout → [Item PVisual] → m (Item PVisual)
iMandateVisual port hi children = case hi of
  Node{..} → pure $ Node {name = nodeNameLtoV name, denoted = children, ..}
  Leaf{name=name@Name{..},..} → do
    let dim = iArea^.area'b.size'di
    vis ←  Port.portEnsureVisual port dim (Proxy @As) nToken Proxy (\Visual{..}→ _sStyleGene nStyle ≢ iStyleGene hi) $
           \drw→ Visual <$> setupVis port n denoted (_sStyle nStyle) iArea drw
                        <*> pure drw
    pure Leaf{lVisual=Just vis, ..}

iUnvisual ∷ Item PLayout → [Item PVisual] → Item PVisual
iUnvisual hi children = case hi of
  Node{..} → Node{name = nodeNameLtoV name, denoted = children, ..}
  Leaf{..} → Leaf{lVisual = Nothing, ..}

iRender ∷ (MonadIO m) ⇒ VPort → Item PVisual → m ()
iRender port Leaf{name=Name{..}, lVisual=Just Visual{..},..} = do
  -- XXX: 'render' is called every frame for everything
  Port.clearDrawable vDrawable
  render port n denoted (_sStyle nStyle) vDrawable vVisual
  Port.drawableContentToGPU vDrawable
iRender _ _ = pure ()


-- * Node
--
data KNode
  = VBox
  | HBox

data Node (k ∷ KNode) (p ∷ Phase) where
  -- Safety note: once Phase-dependent fields are added,
  -- make sure to update nodeName*to*
  HBoxN ∷ Node HBox p
  VBoxN ∷ Node VBox p

boxAxis ∷ Node a p → Axis
boxAxis HBoxN = X
boxAxis VBoxN = Y

nodeGeo ∷ Node k p → Geo
nodeGeo HBoxN = mempty & Flex.direction .~ Flex.DirRow    & Flex.align'content .~ Flex.AlignStart
nodeGeo VBoxN = mempty & Flex.direction .~ Flex.DirColumn & Flex.align'content .~ Flex.AlignStart

nodeNameBtoL ∷ Name (Node k PBlank)  → Name (Node k PLayout)
nodeNameBtoL = Co.unsafeCoerce
nodeNameLtoV ∷ Name (Node k PLayout) → Name (Node k PVisual)
nodeNameLtoV = Co.unsafeCoerce

-- We're pushed to implement a generic As (Node k p) instance, because otherwise,
-- any function depending on As:Denoted type family to quantify over all phases
-- would break.
-- This, in turn, requires a generic Flex (Item p) instance.
instance As (Node (k ∷ KNode) (p ∷ Phase)) where
  type Denoted (Node k p) = [Item p]
  type Sty     (Node k p) = ()
  type Vis     (Node k p) = ()
  defSty _ = ()
  -- compGeo (HBoxN _) = mempty & Flex.direction .~ Flex.DirRow    & Flex.align'content .~ Flex.AlignStart
  -- compGeo (VBoxN _) = mempty & Flex.direction .~ Flex.DirColumn & Flex.align'content .~ Flex.AlignStart
  sizeRequest _ box chi _ =
    -- Requirement is a sum of children requirements
    pure $ (Just <$>) $ _reqt'di $ foldl' (reqt'add $ boxAxis box) zero $ (\x→ Reqt (fromMaybe 0 <$> x^.Flex.size)) <$> chi


-- * Constructors
--
node ∷ ∀ a k. (a ~ Node k PBlank)
  ⇒ IdToken
  → Style a
  → a
  → [Item PBlank]
  → Item PBlank
node tok sty i chi = Node (Name tok sty (nodeGeo i) i) chi blankSize mempty

leaf ∷ (As a, Typeable a)
  ⇒ IdToken
  → Style a
  → a
  → Denoted a
  → Item PBlank
leaf tok sty name denoted = Leaf (Name tok sty mempty name) denoted blankSize mempty ()

-- XXX: here's trouble -- we're using blankIdToken!  No messages for the nodes! ..not that they care yet..
hbox ∷ [Item PBlank] → Item PBlank
vbox ∷ [Item PBlank] → Item PBlank
hbox = node Port.blankIdToken (initStyle ()) HBoxN
vbox = node Port.blankIdToken (initStyle ()) VBoxN


-- * Tree-wise ops
--
treeLeaves ∷ ∀ a. Item a → Map.Map IdToken (Item a)
treeLeaves root = Map.fromList $ walk root
  where walk x@Leaf{..} = [(iToken x, x)]
        walk   Node{..} = concat $ walk <$> denoted

renderTreeVisuals ∷ (MonadIO m) ⇒ VPort → Item PVisual → m ()
renderTreeVisuals port l@Leaf{..} = iRender port l
renderTreeVisuals port   Node{..} = forM_ denoted (renderTreeVisuals port)

showTreeVisuals ∷ (MonadIO m) ⇒ Frame → Item PVisual → m ()
showTreeVisuals frame root = recur (luOf (iArea root)^.lu'po) "" root
  where
    recur parOff _ Leaf{..} = do
      let ourOff = parOff + luOf iArea^.lu'po
      case lVisual of
        Just Visual{..} →
          Port.framePutDrawable frame vDrawable (doubleToFloat <$> ourOff)
        _ → pure ()
    recur parOff pfx Node{..} = do
      -- liftIO $ putStrLn $ pfx <> show (offset ^.po'v) <> " " <> Flex.ppItemArea i
      let ourOff = parOff + luOf iArea^.lu'po
      forM_ denoted $ recur ourOff (pfx <> "  ")

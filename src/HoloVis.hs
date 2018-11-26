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
  ( As(..), defName
  , Style(..), sStyle, sStyleGene, initStyle, defStyle
  , StyleGene(..), fromStyleGene
  , Visual(..)
  --
  , Name(..)
  , IVisual
  , Phase(..)
  , Item(..), iLeafP, iToken, iGeo, iStyleGene, diNothing
  , Node(..)
  , node, leaf
  , hbox, vbox
  , defLeaf
  --
  , traceIGeoDiff
  , iSizeRequest
  , iMandateVisual, iUnvisual
  , iRender
  --
  , treeLeaves
  , renderTreeVisuals
  , showTreeVisuals
  -- * reёxports
  , Drawable(..)
  , VPort
  , module Flex
  )
where

import           Data.Foldable
import           Data.Maybe
import           Data.Typeable
import           GHC.Types                                (Constraint)
import           Generics.SOP                             (Proxy)
import           Linear                            hiding (trace)
import qualified Data.Map.Strict                   as Map
import qualified Unsafe.Coerce                     as Co

-- Local imports
import           HoloPrelude

import           Flex                                     (Geo, defGeo, Flex(..))
import qualified Flex                              as Flex

import           Flatland
import           HoloPort                                 (IdToken, Drawable, Frame)
import qualified HoloPort                          as Port


-- * As -- assigning representation
--
class Typeable r ⇒ As r where
  type      Denoted r ∷ Type
  type          Sty r ∷ Type
  type instance Sty r = ()
  type          Vis r ∷ Type
  type instance Vis r = ()
  defAs       ∷               Proxy r             → r
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
  defAs             _px   = ()
  defSty            _px   = ()
  compSty            _x   = ()
  sizeRequest  _port () () _sty = pure diNothing
  setupVis     _port () () _sty _area      _drw = pure ()
  render       _port () () _sty       _vis _drw = pure ()
  freeVis           _px               _vis      = pure ()

defName ∷ ∀ a. As a ⇒ IdToken → a → Name a
defName tok n = Name tok defStyle defGeo n


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
data Item (c ∷ Type → Constraint) (p ∷ Phase) where
  Leaf ∷ ∀ c p a. (As a, c (Denoted a), Typeable a) ⇒
    { name        ∷ Name a
    , denoted     ∷ Denoted a
    , iSize       ∷ Di (Maybe Double) -- Flex input:  the desired size
                                      -- Has to be always defined, because we need an universally quantified Flex (Item p)
    , iArea       ∷ Area'LU Double    -- Flex output: the resultant size + coords
                                      -- Same quip as for iSize
    , lVisual     ∷ IVisual p a
    } → Item c p
  Node ∷ ∀ c k p a. (a ~ Node c k p, Typeable k) ⇒
    { name        ∷ Name a
    , denoted     ∷ Denoted a
    , iSize       ∷ Di (Maybe Double) -- Flex input:  the desired size
    , iArea       ∷ Area'LU Double    -- Flex output: the resultant size + coords
    } → Item c p

iLeafP ∷ Item c p → Bool
iLeafP = \case
  Leaf{..} → True
  _        → False

iToken ∷ Item c p → IdToken
iToken = \case
  Leaf{name=Name{..},..} → nToken
  Node{name=Name{..},..} → nToken

iGeo ∷ Item c p → Geo
iGeo = \case
  Leaf{name=Name{..},..} → nGeo
  Node{name=Name{..},..} → nGeo

iStyleGene ∷ Item c p → StyleGene
iStyleGene = \case
  Leaf{name=Name{..},..} → _sStyleGene nStyle
  Node{name=Name{..},..} → _sStyleGene nStyle

diNothing ∷ Di (Maybe Double)
diNothing = unsafe'di Nothing Nothing

instance Eq (Item c a) where
  (==)    a b = iToken a ≡ iToken b

instance Ord (Item c a) where
  compare a b = iToken a `compare` iToken b

instance Flex (Item c (a ∷ Phase)) where
  geo      f   Leaf{..} = (\x→ Leaf {name=name {nGeo = x}, ..}) <$> f (nGeo name)
  geo      f   Node{..} = (\x→ Node {name=name {nGeo = x}, ..}) <$> f (nGeo name)
  size     f i@Leaf{..} = (\x→ i    {iSize=x})                  <$> f iSize
  size     f i@Node{..} = (\x→ i    {iSize=x})                  <$> f iSize
  area     f i@Leaf{..} = (\x→ i    {iArea=x})                  <$> f iArea
  area     f i@Node{..} = (\x→ i    {iArea=x})                  <$> f iArea
  children f i@Leaf{..} = (\_→ i)                               <$> f []
  children f   Node{..} = (\x→ Node {denoted=x, ..})            <$> f denoted

traceIGeoDiff ∷ String → Item a b → Item a b
traceIGeoDiff desc x = trace (desc<>" geoΔ: "<>Flex.ppdefGeoDiff (iGeo x)) x

iSizeRequest ∷ ∀ c m. (MonadIO m, Typeable c) ⇒ VPort → Item c PBlank → m (Item c PLayout)
iSizeRequest port Leaf{name=name@Name{..},..} = do
  --
  iSize ← sizeRequest port n denoted (_sStyle $ nStyle)
  trev SIZE HOLO iSize $ Port.tokenHash nToken
  pure Leaf{iArea=mempty, ..}
iSizeRequest port Node{name=name',iSize=_,..} = do
  chi ← (sequence $ iSizeRequest port <$> denoted)
  let name@Name{..} = nodeNameBtoL name'
  --
  size ← sizeRequest port n chi (_sStyle $ nStyle)
  trev SIZE HOLO size (Port.tokenHash nToken)
  pure $ Node{iArea=mempty, iSize=size, denoted=chi, ..}

iMandateVisual ∷ (HasCallStack, MonadIO m) ⇒ VPort → Item c PLayout → [Item c PVisual] → m (Item c PVisual)
iMandateVisual port hi children = case hi of
  Node{..} → pure $ Node {name = nodeNameLtoV name, denoted = children, ..}
  Leaf{name=name@Name{..},..} → do
    let dim = iArea^.area'b.size'di
    vis ←  Port.portEnsureVisual port dim (Proxy @As) nToken Proxy (\Visual{..}→ _sStyleGene nStyle ≢ iStyleGene hi) $
           \drw→ Visual <$> setupVis port n denoted (_sStyle nStyle) iArea drw
                        <*> pure drw
    pure Leaf{lVisual=Just vis, ..}

iUnvisual ∷ Item c PLayout → [Item c PVisual] → Item c PVisual
iUnvisual hi children = case hi of
  Node{..} → Node{name = nodeNameLtoV name, denoted = children, ..}
  Leaf{..} → Leaf{lVisual = Nothing, ..}

iRender ∷ (MonadIO m) ⇒ VPort → Item c PVisual → m ()
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

data Node c (k ∷ KNode) (p ∷ Phase) where
  -- Safety note: once Phase-dependent fields are added,
  -- make sure to update nodeName*to*
  HBoxN ∷ Node c HBox p
  VBoxN ∷ Node c VBox p

boxAxis ∷ Node c k p → Axis
boxAxis HBoxN = X
boxAxis VBoxN = Y

nodeGeo ∷ Node c k p → Geo
nodeGeo HBoxN = defGeo & Flex.direction .~ Flex.DirRow    & Flex.align'content .~ Flex.AlignStart
nodeGeo VBoxN = defGeo & Flex.direction .~ Flex.DirColumn & Flex.align'content .~ Flex.AlignStart

nodeNameBtoL ∷ Name (Node c k PBlank)  → Name (Node c k PLayout)
nodeNameBtoL = Co.unsafeCoerce
nodeNameLtoV ∷ Name (Node c k PLayout) → Name (Node c k PVisual)
nodeNameLtoV = Co.unsafeCoerce

-- We're pushed to implement a generic As (Node k p) instance, because otherwise,
-- any function depending on As:Denoted type family to quantify over all phases
-- would break.
-- This, in turn, requires a generic Flex (Item p) instance.
instance (Typeable c, Typeable k, Typeable p) ⇒ As (Node (c ∷ Type → Constraint) (k ∷ KNode) (p ∷ Phase)) where
  type Denoted (Node c k p) = [Item c p]
  type Sty     (Node c k p) = ()
  type Vis     (Node c k p) = ()
  defAs    = undefined --VBoxN
  defSty _ = ()
  -- compGeo (HBoxN _) = mempty & Flex.direction .~ Flex.DirRow    & Flex.align'content .~ Flex.AlignStart
  -- compGeo (VBoxN _) = mempty & Flex.direction .~ Flex.DirColumn & Flex.align'content .~ Flex.AlignStart
  sizeRequest _ box chi _ = do
    -- Requirement is a sum of children requirements
    let chireqs = (\x→ (,) (iToken x) $ Reqt (fromMaybe 0 <$> x^.Flex.size)) <$> chi
    pure $ Just <$> (_reqt'di $ foldl' (reqt'add $ boxAxis box) zero (snd <$> chireqs))


-- * Constructors
--
node ∷ (a ~ Node c k PBlank, Typeable k)
  ⇒ Name a
  → [Item c PBlank]
  → Item c PBlank
node name denoted = Node name denoted diNothing mempty

leaf ∷ (As a, c (Denoted a))
  ⇒ Name a
  → Denoted a
  → Item c PBlank
leaf name denoted = Leaf name denoted diNothing mempty ()

hbox, vbox ∷ [Item c PBlank] → Item c PBlank
hbox chi = node (Name Port.blankIdToken (initStyle ()) (nodeGeo HBoxN) HBoxN) chi
vbox chi = node (Name Port.blankIdToken (initStyle ()) (nodeGeo VBoxN) VBoxN) chi


defLeaf ∷ (As a, c (Denoted a)) ⇒ IdToken → a → Denoted a → Item c PBlank
defLeaf tok a denoted = leaf (defName tok a) denoted


-- * Tree-wise ops
--
treeLeaves ∷ Item c a → Map.Map IdToken (Item c a)
treeLeaves root = Map.fromList $ walk root
  where walk ∷ Item c a → [(IdToken, Item c a)]
        walk x@Leaf{..} = [(iToken x, x)]
        walk   Node{..} = concat $ walk <$> denoted

renderTreeVisuals ∷ (MonadIO m) ⇒ VPort → Item c PVisual → m ()
renderTreeVisuals port l@Leaf{..} = iRender port l
renderTreeVisuals port   Node{..} = forM_ denoted (renderTreeVisuals port)

showTreeVisuals ∷ (MonadIO m) ⇒ Frame → Item c PVisual → m ()
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

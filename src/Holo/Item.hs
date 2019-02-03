{-# OPTIONS_GHC -Weverything #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors -Wno-missing-import-lists -Wno-implicit-prelude -Wno-monomorphism-restriction -Wno-name-shadowing -Wno-all-missed-specialisations -Wno-unsafe -Wno-missing-export-lists -Wno-type-defaults -Wno-partial-fields -Wno-missing-local-signatures -Wno-orphans #-}
--
-- $Mandate of the module
--
--   1. Provide a generic Item element for a visual layout hierarchy.
--   2. No one does pattern-matching on Item outside this module.
--
-- $Module TODO
--
--   1. Reconsider if (Denoted a) has to reside within the Item itself.
--      After all, layout can (should?) sometimes (often?) happen independent of
--      the value changes?
--   2. Try to drop the first argument of the Node type (the constraint).
--
module Holo.Item
  ( Phase(..)
  , Item
  , iNewToken, iSizeRequest
  , leaf, defLeaf
  , IsNode, IsNodeP, completeNodeName
  , Node, KNode
  , vbox, hbox
  --
  , treeLeaves
  , ensureTreeVisuals
  , renderTreeVisuals
  , showTreeVisuals
  )
where
import qualified Data.IntMap                       as IntMap
import qualified Unsafe.Coerce                     as Co
import           ExternalImports

import           Graphics.Flatland
import           Graphics.Flex
import qualified Graphics.Flex                     as Flex
import           Pretty
import           Tracer

import {-# SOURCE #-}
                 Holo.Classes
import           Holo.Instances.As()
import {-# SOURCE #-}
                 Holo.Name
import           Holo.Port                                (Frame, IdToken)
import qualified Holo.Port                         as Port


-- Note [Granularity and composite structures]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Notions of granularity:
--  - g. of input events -- drawables, easily, because of picking (unless we're willing to introduce complicated mapping schemes)
--  - g. of visual effects -- drawables, again, because of shaders
--  - g. of values -- minimally decorated entry "widgets", in a narrow sense of a widget
--                    ..as well as their composition (Record..) ?


data Phase
  = PBlank
  | PLayout
  | PVisual

type family PIStruc (p ∷ Phase) a ∷ Type where
  PIStruc PBlank  _ = ()
  PIStruc PLayout a = IStruc a
  PIStruc PVisual a = IStruc a

type family PVis    (p ∷ Phase) a ∷ Type where
  PVis    PBlank  _ = ()
  PVis    PLayout _ = ()
  PVis    PVisual a = Maybe (Visual a)

type IsNodeP c (k ∷ KNode) a p = (Typeable a, Typeable c, Typeable k, a ~ Node c k p)
type IsNode  c k a             = IsNodeP c k a PBlank

-- Phasing is TTG-inspired
data Item (c ∷ Type → Constraint) (p ∷ Phase) where
  Leaf ∷ ∀ c p a. (As a, c (Denoted a), Typeable a) ⇒
    { name        ∷ Name a
    , denoted     ∷ Denoted a
    , iStruc      ∷ PIStruc p a
    , iSize       ∷ Di (Maybe Double) -- Flex input:  the desired size
                                      -- Has to be always defined, because we need an universally quantified Flex (Item p)
    , iArea       ∷ Area'LU Double    -- Flex output: the resultant size + coords
                                      -- Same quip as for iSize
    , lVisual     ∷ PVis p a
    } → Item c p
  -- XXX: make Node ∷ Item c PBlank, so it's inadmissible in other contexts
  Node ∷ ∀ c p a k. (IsNodeP c k a p) ⇒
    { name        ∷ Name a
    , denoted     ∷ Denoted a
    , iStruc      ∷ PIStruc p a
    , iSize       ∷ Di (Maybe Double) -- Flex input:  the desired size
    , iArea       ∷ Area'LU Double    -- Flex output: the resultant size + coords
    } → Item c p
  -- FullNode has IdTokens allocated
  FullNode ∷ ∀ c p a k. (IsNodeP c k a p) ⇒
    { name        ∷ Name a
    , denoted     ∷ Denoted a
    , iStruc      ∷ PIStruc p a
    , iSize       ∷ Di (Maybe Double) -- Flex input:  the desired size
    , iArea       ∷ Area'LU Double    -- Flex output: the resultant size + coords
    } → Item c p

instance Semigroup (Item Top PBlank)  where _ <> _ = mempty
instance Monoid    (Item Top PBlank)  where mempty = Leaf (Name Port.blankIdToken (initStyle ()) defGeo ()) () () diNothing mempty mempty

instance Semigroup (Item Top PLayout) where _ <> _ = mempty
instance Monoid    (Item Top PLayout) where mempty = Leaf (Name Port.blankIdToken (initStyle ()) defGeo ()) () () diNothing mempty mempty

iNewToken ∷ (MonadTrace r m, Typeable a) ⇒ Proxy a → m IdToken
iNewToken p = Port.newId $ showT $ typeRep p

iToken ∷ Item c p → IdToken
iToken = \case
  Leaf{name=Name{..},..}     → nToken
  FullNode{name=Name{..},..} → nToken
  n@Node{}                   → eIncompleteNode "iToken" n

iStyleGene ∷ Item c p → StyleGene
iStyleGene = \case
  Leaf{name=Name{..},..}     → _sStyleGene nStyle
  FullNode{name=Name{..},..} → _sStyleGene nStyle
  n@Node{}                   → eIncompleteNode "iStyleGene" n

instance Eq (Item c a) where
  (==)    a b = iToken a ≡ iToken b

instance Ord (Item c a) where
  compare a b = iToken a `compare` iToken b

instance Show (Item c a) where
  show     Leaf{..} = showItemName name
  show     Node{..} = showItemName name
  show FullNode{..} = showItemName name

showItemName ∷ ∀ a. Typeable a ⇒ Name a → String
showItemName n = printf "(Item %s id=0x%x)" (show $ typeRep $ Proxy @a) (Port.tokenHash $ nToken n)

instance Flex (Item c (a ∷ Phase)) where
  geo      f       Leaf{..} = (\x→     Leaf {name=name {nGeo = x}, ..}) <$> f (nGeo name)
  geo      f       Node{..} = (\x→     Node {name=name {nGeo = x}, ..}) <$> f (nGeo name)
  geo      f   FullNode{..} = (\x→ FullNode {name=name {nGeo = x}, ..}) <$> f (nGeo name)
  size     f     i@Leaf{..} = (\x→ i        {iSize=x})                  <$> f iSize
  size     f     i@Node{..} = (\x→ i        {iSize=x})                  <$> f iSize
  size     f i@FullNode{..} = (\x→ i        {iSize=x})                  <$> f iSize
  area     f     i@Leaf{..} = (\x→ i        {iArea=x})                  <$> f iArea
  area     f     i@Node{..} = (\x→ i        {iArea=x})                  <$> f iArea
  area     f i@FullNode{..} = (\x→ i        {iArea=x})                  <$> f iArea
  children f     i@Leaf{..} = (\_→ i)                                   <$> f []
  children f       Node{..} = (\x→     Node {denoted=x, ..})            <$> f denoted
  children f   FullNode{..} = (\x→ FullNode {denoted=x, ..})            <$> f denoted

_iGeo ∷ Item c p → Geo
_iGeo = \case
  Leaf{name=Name{..},..}     → nGeo
  Node{name=Name{..},..}     → nGeo
  FullNode{name=Name{..},..} → nGeo

_traceIGeoDiff ∷ String → Item a b → Item a b
_traceIGeoDiff desc x = trace (desc<>" geoΔ: "<>Flex.ppdefGeoDiff (_iGeo x)) x

iSizeRequest ∷ ∀ m r c. (MonadTrace r m, Typeable c) ⇒ VPort → Item c PBlank → m (Item c PLayout)
iSizeRequest port Leaf{name=name@Name{..},..} = do
  --
  (,) iStruc iSize ← sizeRequest port n denoted (_sStyle $ nStyle)
  logDebug "HOLO SIZE %s %d" (show iSize, Port.tokenHash nToken)
  -- trev SIZE HOLO iSize $ Port.tokenHash nToken
  pure Leaf{iArea=mempty, ..}
iSizeRequest port FullNode{name=name',iSize=_,..} = do
  chi ← (sequence $ iSizeRequest port <$> denoted)
  let name@Name{..} = nodeNameBtoL name'
  --
  (,) iStruc iSize ← sizeRequest port n chi (_sStyle $ nStyle)
  logDebug "HOLO SIZE %s %d" (show iSize, Port.tokenHash nToken)
  -- trev SIZE HOLO iSize (Port.tokenHash nToken)
  pure $ Node{iArea=mempty, iSize=iSize, denoted=chi, ..}
iSizeRequest _ n@Node{} =
  eIncompleteNode "iSizeRequest" n

iMandateVisual ∷ (HasCallStack, MonadTrace r m) ⇒ VPort → Item c PLayout → [Item c PVisual] → m (Item c PVisual)
iMandateVisual port hi children = case hi of
  Leaf{name=name@Name{..},..} → do
    let dim = iArea^.area'b.size'di
    vis ←  Port.portEnsureVisual port dim (Proxy @As) nToken Proxy (\Visual{..}→ _sStyleGene nStyle ≢ iStyleGene hi) $
           \drw→ Visual <$> setupVis port n denoted (_sStyle nStyle) iStruc iArea drw
                        <*> pure drw
    pure Leaf{lVisual=Just vis, ..}
  FullNode{..} → pure $ FullNode {name = nodeNameLtoV name, denoted = children, ..}
  Node{..}     → eIncompleteNode "iMandateVisual" hi

iUnvisual ∷ Item c PLayout → [Item c PVisual] → Item c PVisual
iUnvisual hi children = case hi of
  Leaf{..}     → Leaf{lVisual = Nothing, ..}
  FullNode{..} → FullNode{name = nodeNameLtoV name, denoted = children, ..}
  Node{..}     → eIncompleteNode "iUnvisual" hi

iRender ∷ (MonadTrace r m) ⇒ VPort → Item c PVisual → m ()
iRender port Leaf{name=Name{..}, lVisual=Just Visual{..},..} = do
  -- XXX: 'render' is called every frame for everything
  Port.clearDrawable vDrawable
  render port n denoted (_sStyle nStyle) iStruc zero vDrawable vVisual
  Port.drawableContentToGPU vDrawable
iRender _ _ = pure ()


-- * Node
--
data KNode
  = VBox
  | HBox

data Node  (c ∷ Type → Constraint) (k ∷ KNode) (p ∷ Phase) where
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
    pure $ (,) () $ Just <$> (_reqt'di $ foldl' (reqt'add $ boxAxis box) zero (snd <$> chireqs))


-- * Constructors
--
node ∷ IsNode c k a
  ⇒ Name a
  → [Item c PBlank]
  → Item c PBlank
node name denoted = Node name denoted () diNothing mempty

replaceNameToken ∷ IdToken → Name n → Name n
replaceNameToken tok name = name { nToken = tok }

completeNodeName ∷ ()
  ⇒ IdToken → Item c p → Item c p
completeNodeName tok (Node name denoted struc size area) =
  FullNode (replaceNameToken tok name) denoted struc size area
completeNodeName _ x = x

eIncompleteNode ∷ HasCallStack ⇒ String → Item c a → b
eIncompleteNode ctx item = error $ printf "%s: incomplete node %s" ctx (show item)

leaf ∷ (As a, c (Denoted a))
  ⇒ Name a
  → Denoted a
  → Item c PBlank
leaf name denoted = Leaf name denoted () diNothing mempty ()

hbox, vbox ∷ Typeable c ⇒ [Item c PBlank] → Item c PBlank
hbox chi = node (Name Port.blankIdToken (initStyle ()) (nodeGeo HBoxN) HBoxN) chi
vbox chi = node (Name Port.blankIdToken (initStyle ()) (nodeGeo VBoxN) VBoxN) chi


defLeaf ∷ (As a, c (Denoted a)) ⇒ IdToken → a → Denoted a → Item c PBlank
defLeaf tok a denoted = leaf (defName tok a) denoted


-- * Tree-wise ops
--
treeLeaves ∷ Item c a → IntMap.IntMap (Item c a)
treeLeaves root = IntMap.fromList $ walk root
  where walk ∷ Item c a → [(Int, Item c a)]
        walk   x@Leaf{..} = [(Port.tokenHash $ iToken x, x)]
        walk FullNode{..} = concat $ walk <$> denoted
        walk     x@Node{} = eIncompleteNode "treeLeaves" x

ensureTreeVisuals ∷ (MonadTrace r m) ⇒ VPort → Item c PLayout → m (Item c PVisual)
ensureTreeVisuals port i = case i of
  Leaf{}       → iMandateVisual port i []
  FullNode{..} → iUnvisual i <$> (sequence $ ensureTreeVisuals port <$> denoted)
  Node{}       → eIncompleteNode "treeLeaves" i

renderTreeVisuals ∷ (MonadTrace r m) ⇒ VPort → Item c PVisual → m ()
renderTreeVisuals port     i@Leaf{} = iRender port i
renderTreeVisuals port FullNode{..} = forM_ denoted (renderTreeVisuals port)
renderTreeVisuals _        i@Node{} = eIncompleteNode "renderTreeVisuals" i

showTreeVisuals ∷ ∀ m r c. (MonadTrace r m) ⇒ Frame → Item c PVisual → m ()
showTreeVisuals frame root = recur (luOf (iArea root)^.lu'po) "" root
  where
    recur parOff _ Leaf{..} = do
      let ourOff = parOff + luOf iArea^.lu'po
      case lVisual of
        Just Visual{..} →
          Port.framePutDrawable frame vDrawable (doubleToFloat <$> ourOff)
        _ → pure ()
    recur parOff pfx FullNode{..} = do
      -- liftIO $ putStrLn $ pfx <> show (offset ^.po'v) <> " " <> Flex.ppItemArea i
      let ourOff = parOff + luOf iArea^.lu'po
      forM_ denoted $ recur ourOff (pfx <> "  ")
    recur _ _ i@Node{..} = eIncompleteNode "showTreeVisuals" i

---
--- Demonstration that compounds can't be easily done via As instances.
---
-- instance (As a, As b) ⇒ As (Axis, (a, b)) where
--   type Denoted (Axis, (a, b)) = (Denoted a, Denoted b)
--   type Sty     (Axis, (a, b)) = (Sty     a,     Sty b)
--   type IStruc  (Axis, (a, b)) = (Double
--                                 ,(IStruc a,  IStruc b))
--   -- type Drw     (Axis, (a, b)) = (Drw     a,     Drw b)
--   type Vis     (Axis, (a, b)) = (Vis     a,     Vis b)
--   defAs                     _ = (X, (defAs Proxy, defAs Proxy))
--   defSty                    _ = (defSty $ Proxy @a, defSty $ Proxy @b)
--   sizeRequest p (ax, (asA, asB)) (cA, cB) (sA, sB) = do
--     (isA, rA@(Di v)) ← ((fromMaybe 0 <$>) <$>) <$> sizeRequest p asA cA sA
--     (isB, rB)        ← ((fromMaybe 0 <$>) <$>) <$> sizeRequest p asB cB sB
--     pure $ ((v2'proj ax v, (isA, isB)),) $ Just <$> (_reqt'di $ reqt'add ax (Reqt rA) (Reqt rB))
--   setupVis    p (ax, (asA, asB)) (cA, cB) (sA, sB) (shift, (isA, isB)) area' --------(drwA, drwB)------- = do
--     let (aLU, aRB) = area'split'start ax shift area'
--     vA ← setupVis p asA           cA       sA               isA        aLU    drwA
--     vB ← setupVis p asB               cB       sB                isB   aRB    drwB
--     pure $ (,) vA vB
--   render      p (ax, (asA, asB)) (cA, cB) (sA, sB) (shiftN, (isA, isB)) shift drw (aV, bV) = do
--     render        p asA           cA       sA                isA        shift drw  aV
--     render        p asB               cB       sB                 isB  (shift & po'd ax %~ (+shiftN)) drw bV
--   freeVis _ (aV, bV) = do
--     freeVis (Proxy @a) aV
--     freeVis (Proxy @b) bV

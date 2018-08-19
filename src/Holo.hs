{-# LANGUAGE AllowAmbiguousTypes, ExplicitForAll, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RankNTypes, UndecidableInstances #-}
{-# LANGUAGE DataKinds, GADTs, NoMonomorphismRestriction, TypeFamilies, TypeFamilyDependencies, TypeInType #-}
{-# LANGUAGE LambdaCase, OverloadedLists, OverloadedStrings, PartialTypeSignatures, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TupleSections, TypeOperators, ViewPatterns #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors -Wno-orphans #-}
module Holo
  (
  -- * Define this:
    Holo(..)
  -- * Get that:
  , StyleOf(..), VisualOf(..)
  , KNode(..), Node(..)
  , item
  , node
  , vbox, hbox
  , leaf
  , Phase(..)
  , HoloItem, hiToken, hiArea, holoitemDrawable
  , holotreeLeaves
  , queryHoloitem
  , renderHoloitem
  --
  , queryHolotree
  , visualiseHolotree
  , renderHolotreeVisuals
  , drawHolotreeVisuals
  -- * Dirty parts
  , emptyLayoutHolo
  , emptyVisualHolo
  , blankIdToken
  -- * Holo instances
  , Rect(..), RectStyle, RectVisual
  , TextStyle, TextVisual
  , TextZipperStyle, TextZipperVisual
  , tsFontKey, tsSizeSpec, tsColor, tesTSStyle
  )
where

import           HoloPrelude

import           Control.Monad
import qualified Data.Map.Strict                   as Map
-- import qualified Data.Set                          as Set
import qualified Data.Text                         as T
import qualified Data.Text.Zipper                  as T
import           Linear
import           Prelude                           hiding ((.), id)

import qualified GI.Pango                          as GIP

-- Local imports
import           HoloTypes

import           Elsewhere
import           Flatland
import qualified Flex                              as Flex
import           Flex                                     (Flex, Geo)
import           HoloCube                                 (Frame)
import           HoloFont
import           HoloPort


-- * HoloItem

instance Eq (HoloItem a) where
  (==) a b = (≡) (hiToken a) (hiToken b)

instance Ord (HoloItem a) where
  compare a b = compare (hiToken a) (hiToken b)

holotreeLeaves ∷ HoloItem a → Map.Map IdToken (HoloItem a)
holotreeLeaves root = Map.fromList $ walk root
  where walk x@(hiChildren → []) = [(hiToken x, x)]
        walk HoloItem{..}        = concat $ walk <$> hiChildren

holoitemDrawable ∷ Port → HoloItem PVisual → Drawable
holoitemDrawable port HoloItem{..} = drawableOf port hiVisual


-- * Minimal
instance Holo () where
  data StyleOf  ()     = UnitStyle
  data VisualOf ()     = UnitVisual
  drawableOf Port{..} _  = portEmptyDrawable
  query        _ _ _     = pure $ Di $ V2 Nothing Nothing
  createVisual _ _ _ _ _ = pure UnitVisual
  renderVisual _ _ _     = pure ()

instance Semigroup (StyleOf ()) where
  _ <> _ = mempty
instance Monoid    (StyleOf ()) where
  mempty      = UnitStyle

instance Semigroup (HoloItem PBlank) where
  _ <> _ = mempty
instance Monoid    (HoloItem PBlank) where
  mempty      = HoloItem () blankIdToken mempty UnitStyle [] (Di $ V2 Nothing Nothing) mempty ()

instance Semigroup (HoloItem PLayout) where
  l <> r = vbox [l, r]
instance Monoid    (HoloItem PLayout) where
  mempty      = HoloItem () blankIdToken mempty UnitStyle [] (Di $ V2 Nothing Nothing) mempty ()
-- instance Monoid (HoloItem Visual) where
--   mappend l r = holoVBox [l, r]
--   mempty      = HoloItem () blankIdToken SPU mempty UnitStyle [] (Di $ V2 Nothing Nothing) mempty UnitVisual

emptyLayoutHolo ∷ HoloItem PLayout
emptyLayoutHolo =
  HoloItem () blankIdToken mempty mempty [] (Di $ V2 Nothing Nothing) mempty ()

emptyVisualHolo ∷ HoloItem PVisual
emptyVisualHolo =
  HoloItem () blankIdToken mempty mempty [] (Di $ V2 Nothing Nothing) mempty UnitVisual


instance Flex (HoloItem PBlank) where
  geo      f hi@HoloItem{..} = (\x→ hi {hiGeo=x})      <$> f hiGeo
  size     f hi@HoloItem{..} = (\x→ hi {hiSize=x})     <$> f hiSize
  children f hi@HoloItem{..} = (\x→ hi {hiChildren=x}) <$> f hiChildren
  -- area     f hi@HoloItem{..} = (\x→ hi {hiArea=x})     <$> f hiArea

instance Flex (HoloItem PLayout) where
  geo      f hi@HoloItem{..} = (\x→ hi {hiGeo=x})      <$> f hiGeo
  size     f hi@HoloItem{..} = (\x→ hi {hiSize=x})     <$> f hiSize
  children f hi@HoloItem{..} = (\x→ hi {hiChildren=x}) <$> f hiChildren
  area     f hi@HoloItem{..} = (\x→ hi {hiArea=x})     <$> f hiArea

instance Flex (HoloItem PVisual) where
  geo      f hi@HoloItem{..} = (\x→ hi {hiGeo=x})      <$> f hiGeo
  size     f hi@HoloItem{..} = (\x→ hi {hiSize=x})     <$> f hiSize
  children f hi@HoloItem{..} = (\x→ hi {hiChildren=x}) <$> f hiChildren
  area     f hi@HoloItem{..} = (\x→ hi {hiArea=x})     <$> f hiArea


-- | 'HoloItem': raison d'etre: type-free payload of Flex's item tree.
--
-- | Designate three passes over the tree:
--   1. Query
--   2. Layout
--   2. CreateVisual

queryHoloitem ∷ (MonadIO m) ⇒ Port → HoloItem PBlank → [HoloItem PLayout] → m (HoloItem PLayout)
queryHoloitem port hoi children =
  case hoi of
    HoloItem{..} → do
      size ← query port hiStyle holo
      pure HoloItem{hiSize=size, hiArea=mempty, hiChildren=children, ..}

visualiseHoloitem ∷ (HasCallStack, MonadIO m) ⇒ Port → HoloItem PLayout → [HoloItem PVisual] → m (HoloItem PVisual)
visualiseHoloitem port HoloItem{..} hiChildren' = do
  drw ← establishSizedDrawableForId port hiToken (hiArea^.area'b.size'di)
  vis ← createVisual port hiStyle hiArea holo drw
  pure HoloItem{hiVisual=vis, hiChildren = hiChildren', ..}

renderHoloitem ∷ (MonadIO m) ⇒ Port → HoloItem PVisual → m ()
renderHoloitem port HoloItem{..} = do
  let drw = drawableOf port hiVisual
  clearDrawable drw
  renderVisual port hiVisual holo
  drawableContentToGPU drw


queryHolotree ∷ (MonadIO m) ⇒ Port → HoloItem PBlank → m (HoloItem PLayout)
queryHolotree port hoi@HoloItem{..} =
  queryHoloitem     port hoi =<< (sequence $ queryHolotree     port <$> hiChildren)

visualiseHolotree ∷ (MonadIO m) ⇒ Port → HoloItem PLayout → m (HoloItem PVisual)
visualiseHolotree port hoi@HoloItem{..} = do
  visualiseHoloitem port hoi =<< (sequence $ visualiseHolotree port <$> hiChildren)

renderHolotreeVisuals ∷ (MonadIO m) ⇒ Port → HoloItem PVisual → m ()
renderHolotreeVisuals port hoi@HoloItem{..} = do
  renderHoloitem port hoi
  forM_ hiChildren (renderHolotreeVisuals port)

drawHolotreeVisuals ∷ (MonadIO m) ⇒ Port → Frame → HoloItem PVisual → m ()
drawHolotreeVisuals port frame root = loop (luOf (hiArea root)^.lu'po) root
  where
    loop offset HoloItem{..} = do
      if null hiChildren
      then do
        framePutDrawable frame (drawableOf port hiVisual) (doubleToFloat <$> (offset + luOf hiArea^.lu'po))
        -- liftIO $ putStrLn $ "draw -- " <> (show $ luOf hiArea^.lu'po) <> " " <> (TL.unpack $ rendCompact $ pretty'Area hiArea) <> " " <> (TL.unpack $ rendCompact $ pretty'Area (area'LU hiArea))
      else do
        forM_ hiChildren $ loop (luOf hiArea^.lu'po)


-- * Internal nodes
data KNode
  = VBox
  | HBox

data Node (k ∷ KNode) where
  HBoxN ∷ Node HBox
  VBoxN ∷ Node VBox

instance Holo   (Node (k ∷ KNode)) where
  data StyleOf  (Node k) = NodeStyle
  data VisualOf (Node k) = NodeVisual
  drawableOf Port{..} _  = portEmptyDrawable
  query        _ _ _     = pure $ Di $ V2 Nothing Nothing
  createVisual _ _ _ _ _ = pure NodeVisual
  renderVisual _ _ _     = pure ()

instance Semigroup (StyleOf (Node k)) where
  _ <> _ = NodeStyle
instance Monoid    (StyleOf (Node k)) where
  mempty      = NodeStyle

nodeGeo ∷ Node k → Geo
nodeGeo HBoxN = mempty & Flex.grow .~ 1 & Flex.direction .~ Flex.DirRow
nodeGeo VBoxN = mempty & Flex.grow .~ 1 & Flex.direction .~ Flex.DirColumn


-- * Layout tree Item constructors
--
-- XXX: this FromUnit constraint is a genuine pain.

item ∷ ∀ p a. (Holo a) ⇒ HIVisual p a → StyleOf a → HIArea p → Geo → IdToken → a → [HoloItem p] → HoloItem p
item hiVisual hiStyle hiArea hiGeo hiToken holo hiChildren =
  let hiSize = Di (V2 Nothing Nothing)
  in HoloItem{..}

node ∷ ∀ p k. (Holo (Node k)) ⇒ IdToken → HIVisual p (Node k) → HIArea p → Node k → [HoloItem p] → HoloItem p
node idToken visual area holo =
  item visual mempty area (nodeGeo holo) idToken holo

leaf ∷ ∀ a. (Holo a) ⇒ IdToken → a → StyleOf a → HoloItem PBlank
leaf idToken holo hiStyle =
  item () hiStyle () mempty         idToken holo []


-- * Pre-package tree constructors
--
vbox, hbox ∷ [HoloItem PLayout] → HoloItem PLayout
-- XXX: here's trouble -- we're using blankIdToken!
hbox = node blankIdToken () (Area (mkLU 0 0) (mkSize 0 0)) (HBoxN ∷ Node HBox)
vbox = node blankIdToken () (Area (mkLU 0 0) (mkSize 0 0)) (VBoxN ∷ Node VBox)


-- * Leaves

data Rect where
  Rect ∷
    { _rectDim   ∷ Di (Unit PU)
    , _rectColor ∷ Co Double
    } → Rect
-- makeLenses ''Rect

instance Semigroup RectStyle where
  _ <> _ = RectStyle
instance Monoid    RectStyle where
  mempty      = RectStyle

type RectStyle  = StyleOf  Rect
type RectVisual = VisualOf Rect
instance Holo   Rect where
  data StyleOf  Rect where RectStyle  ∷ RectStyle
  data VisualOf Rect where RectVisual ∷ { rectDrawable ∷ Drawable } → RectVisual
  drawableOf _ = rectDrawable
  query     port _       Rect{..} = pure $ Just ∘ fromPU ∘ fromUnit (portDΠ port) <$> _rectDim
  createVisual port@Port{..} _ _area Rect{..} _drw = do
    let dim = PUs <$> _area^.area'b.size'di
    -- WTF: we're given a DRW, and then we proceed to ignore it?
    d@Drawable{..} ← portMakeDrawable port $ fromPU ∘ fromUnit (sttsDΠ portSettings) <$> dim
    drawableDrawRect port d _rectColor dim
    pure $ RectVisual d
  renderVisual port v@RectVisual{rectDrawable=Drawable{..}} Rect{..} =
    drawableDrawRect port (rectDrawable v) _rectColor _rectDim


-- * This is a complicated story:
--
-- Actors:
--  1. u-free text style
--  2. PU-wired Fontmap from the Port
--
instance Semigroup TextStyle where
  TextStyle lfk (TextSizeSpec lws lhl) lco <> TextStyle rfk (TextSizeSpec rws rhl) rco = TextStyle
    { _tsFontKey     = choosePartially "default" lfk rfk
    , _tsSizeSpec    = TextSizeSpec (lws <|> rws) (choosePartially OneLine lhl rhl)
    , _tsColor       = lco <> rco
    }
instance Monoid    TextStyle where
  mempty = TextStyle
    { _tsFontKey     = "default"
    , _tsSizeSpec    = TextSizeSpec Nothing OneLine
    , _tsColor       = white
    }

type TextStyle  = StyleOf  T.Text
type TextVisual = VisualOf T.Text
instance Holo T.Text where
  data StyleOf  T.Text where
    TextStyle ∷
      { _tsFontKey     ∷ FontKey
      , _tsSizeSpec    ∷ TextSizeSpec PU
      , _tsColor       ∷ Co Double
      } → TextStyle
  data VisualOf T.Text where
    Text ∷
      { tStyle         ∷ TextStyle
      , tDrawable      ∷ Drawable
      , tFont          ∷ WFont Bound
      , tLayout        ∷ GIP.Layout
      , tDim           ∷ Di (Unit u)
      } → TextVisual
  drawableOf _ = tDrawable
  query port@Port{..} TextStyle{..} content = do
    let font = portFont' port _tsFontKey -- XXX: non-total
    (Just ∘ fromPU <$>) ∘ either errorT id <$> fontQuerySize font (convert (portDΠ port) _tsSizeSpec) (partial (≢ "") content)
  createVisual port tStyle@TextStyle{..} area' _content tDrawable = do
    -- 1. find font, 2. bind font to GIC, 3. create layout
    -- Q: why not also draw here?  Reflow?
    let font = portFont' port _tsFontKey -- XXX: non-total
        tDim = fromUnit (portDΠ port) ∘ PUs <$> dimOf area'
    -- liftIO $ putStrLn $ printf "createVisual T.Text: %s → %s" (show _tsFontKey) (show font)
    (,) tFont tLayout ← drawableBindFontLayout (portDΠ port) tDrawable font tDim _tsSizeSpec
    pure Text{..}
  renderVisual _ Text{..} text =
    -- 1. execute GIP draw & GIPC composition
    drawableDrawText tDrawable tLayout (_tsColor tStyle) text

tsFontKey   ∷ Lens' TextStyle FontKey
tsFontKey  f ts@(TextStyle x _ _) = (\xx→ts{_tsFontKey=xx})  <$> f x
tsSizeSpec  ∷ Lens' TextStyle (TextSizeSpec PU)
tsSizeSpec f ts@(TextStyle _ x _) = (\xx→ts{_tsSizeSpec=xx}) <$> f x
tsColor     ∷ Lens' TextStyle (Co Double)
tsColor    f ts@(TextStyle _ _ x) = (\xx→ts{_tsColor=xx})    <$> f x


type TextZipperStyle  = StyleOf  (T.TextZipper T.Text)
type TextZipperVisual = VisualOf (T.TextZipper T.Text)

instance Semigroup TextZipperStyle where
  TextZipperStyle l <> TextZipperStyle r = TextZipperStyle $ l <> r
instance Monoid    TextZipperStyle where
  mempty = TextZipperStyle mempty

instance Holo   (T.TextZipper T.Text) where
  data StyleOf  (T.TextZipper T.Text) where
    TextZipperStyle ∷
      { fromTextZipperStyle ∷ TextStyle
      } → TextZipperStyle
  data VisualOf (T.TextZipper T.Text) where
    TextZipper ∷
      { teText ∷ VisualOf T.Text
      } → TextZipperVisual
  drawableOf _ = tDrawable ∘ teText
  query port TextZipperStyle{..} tz =
    query port fromTextZipperStyle (zipperText tz)
  createVisual port hsty area' content drw = do
    TextZipper <$> createVisual port (hsty & fromTextZipperStyle) area' (zipperText content) drw
  renderVisual _ (TextZipper Text{..}) content = do
    -- XXX: cursor position
    drawableDrawText tDrawable tLayout (_tsColor tStyle) (zipperText content)

tesTSStyle  ∷ Lens' TextZipperStyle TextStyle
tesTSStyle f (TextZipperStyle x) = TextZipperStyle <$> f x



-- visual ∷ (ReflexGLFWCtx t m, Holo a) ⇒ Settings PU → ObjectStream → StyleOf (Visual a) → Event t (a, b) → m (Event t (Holosome a, b))
-- visual stts holoStream holoStyle holoE =
--   performEvent (holoE <&> ((\(holo, x) → liftIO $ do
--                                -- XXX/expressivity:  this threading of 'x' is..
--                                holoVisual ← createVisual stts holoStream holoStyle holo
--                                holoRef    ← IO.newIORef holo
--                                -- holoPosRef ← IO.newIORef pos
--                                pure (Holosome{..}, x))
--                           ))

-- update ∷ (MonadIO m, Holo a) ⇒ Settings PU → Holosome a → (a → a) → m ()
-- update stts Holosome{..} f = do
--   old ← liftIO $ IO.readIORef holoRef
--   let new = f old
--   liftIO $ IO.writeIORef holoRef new
--   renderVisual stts holoStream holoVisual new

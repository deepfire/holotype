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
import           Data.Singletons
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

holoitemDrawable ∷ Port → HoloItem Visual → Drawable
holoitemDrawable port HoloItem{..} = drawableOf port hiVisual


-- * Minimal
instance FromUnit u ⇒ Holo u () where
  data StyleOf  u ()     = UnitStyle
  data VisualOf u ()     = UnitVisual
  drawableOf Port{..} _  = portEmptyDrawable
  query        _ _ _     = pure $ Di $ V2 Nothing Nothing
  createVisual _ _ _ _ _ = pure UnitVisual
  renderVisual _ _ _     = pure ()

instance Semigroup (StyleOf u ()) where
  _ <> _ = mempty
instance Monoid    (StyleOf u ()) where
  mempty      = UnitStyle

instance Semigroup (HoloItem Blank) where
  _ <> _ = mempty
instance Monoid    (HoloItem Blank) where
  mempty      = HoloItem () blankIdToken SPU mempty UnitStyle [] (Di $ V2 Nothing Nothing) mempty ()

instance Semigroup (HoloItem Layout) where
  l <> r = vbox [l, r]
instance Monoid    (HoloItem Layout) where
  mempty      = HoloItem () blankIdToken SPU mempty UnitStyle [] (Di $ V2 Nothing Nothing) mempty ()
-- instance Monoid (HoloItem Visual) where
--   mappend l r = holoVBox [l, r]
--   mempty      = HoloItem () blankIdToken SPU mempty UnitStyle [] (Di $ V2 Nothing Nothing) mempty UnitVisual

emptyLayoutHolo ∷ HoloItem Layout
emptyLayoutHolo =
  HoloItem () blankIdToken SPU mempty mempty [] (Di $ V2 Nothing Nothing) mempty ()

emptyVisualHolo ∷ HoloItem Visual
emptyVisualHolo =
  HoloItem () blankIdToken SPU mempty mempty [] (Di $ V2 Nothing Nothing) mempty UnitVisual


instance Flex (HoloItem Blank) where
  geo      f hi@HoloItem{..} = (\x→ hi {hiGeo=x})      <$> f hiGeo
  size     f hi@HoloItem{..} = (\x→ hi {hiSize=x})     <$> f hiSize
  children f hi@HoloItem{..} = (\x→ hi {hiChildren=x}) <$> f hiChildren
  -- area     f hi@HoloItem{..} = (\x→ hi {hiArea=x})     <$> f hiArea

instance Flex (HoloItem Layout) where
  geo      f hi@HoloItem{..} = (\x→ hi {hiGeo=x})      <$> f hiGeo
  size     f hi@HoloItem{..} = (\x→ hi {hiSize=x})     <$> f hiSize
  children f hi@HoloItem{..} = (\x→ hi {hiChildren=x}) <$> f hiChildren
  area     f hi@HoloItem{..} = (\x→ hi {hiArea=x})     <$> f hiArea

instance Flex (HoloItem Visual) where
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

queryHoloitem ∷ (MonadIO m) ⇒ Port → HoloItem Blank → [HoloItem Layout] → m (HoloItem Layout)
queryHoloitem port hoi children =
  case hoi of
    HoloItem{..} → do
      size ← query port hiStyle holo
      pure HoloItem{hiSize=size, hiArea=mempty, hiChildren=children, ..}

visualiseHoloitem ∷ (HasCallStack, MonadIO m) ⇒ Port → HoloItem Layout → [HoloItem Visual] → m (HoloItem Visual)
visualiseHoloitem port HoloItem{..} hiChildren' = do
  drw ← establishSizedDrawableForId port hiToken (hiArea^.area'b.size'di)
  vis ← createVisual port hiStyle hiArea holo drw
  pure HoloItem{hiVisual=vis, hiChildren = hiChildren', ..}

renderHoloitem ∷ (MonadIO m) ⇒ Port → HoloItem Visual → m ()
renderHoloitem port HoloItem{..} = do
  let drw = drawableOf port hiVisual
  clearDrawable drw
  renderVisual port hiVisual holo
  drawableContentToGPU drw


queryHolotree ∷ (MonadIO m) ⇒ Port → HoloItem Blank → m (HoloItem Layout)
queryHolotree port hoi@HoloItem{..} =
  queryHoloitem     port hoi =<< (sequence $ queryHolotree     port <$> hiChildren)

visualiseHolotree ∷ (MonadIO m) ⇒ Port → HoloItem Layout → m (HoloItem Visual)
visualiseHolotree port hoi@HoloItem{..} = do
  visualiseHoloitem port hoi =<< (sequence $ visualiseHolotree port <$> hiChildren)

renderHolotreeVisuals ∷ (MonadIO m) ⇒ Port → HoloItem Visual → m ()
renderHolotreeVisuals port hoi@HoloItem{..} = do
  renderHoloitem port hoi
  forM_ hiChildren (renderHolotreeVisuals port)

drawHolotreeVisuals ∷ (MonadIO m) ⇒ Port → Frame → HoloItem Visual → m ()
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

data Node (u ∷ UnitK) (k ∷ KNode) where
  HBoxN ∷ Node u HBox
  VBoxN ∷ Node u VBox

instance FromUnit u ⇒ Holo   u (Node u (k ∷ KNode)) where
  data StyleOf  u (Node u k) = NodeStyle
  data VisualOf u (Node u k) = NodeVisual
  drawableOf Port{..} _  = portEmptyDrawable
  query        _ _ _     = pure $ Di $ V2 Nothing Nothing
  createVisual _ _ _ _ _ = pure NodeVisual
  renderVisual _ _ _     = pure ()

instance Semigroup (StyleOf u (Node u k)) where
  _ <> _ = NodeStyle
instance Monoid    (StyleOf u (Node u k)) where
  mempty      = NodeStyle

nodeGeo ∷ Node u k → Geo
nodeGeo HBoxN = mempty & Flex.grow .~ 1 & Flex.direction .~ Flex.DirRow
nodeGeo VBoxN = mempty & Flex.grow .~ 1 & Flex.direction .~ Flex.DirColumn


-- * Layout tree Item constructors
--
-- XXX: this FromUnit constraint is a genuine pain.

item ∷ ∀ p u a. (FromUnit u, Holo u a, SingI u) ⇒ SUnitK u → HIVisual p u a → StyleOf u a → HIArea p → Geo → IdToken → a → [HoloItem p] → HoloItem p
item hiUnit hiVisual hiStyle hiArea hiGeo hiToken holo hiChildren =
  let hiSize = Di (V2 Nothing Nothing)
  in HoloItem{..}

node ∷ ∀ p u k. (FromUnit u, Holo u (Node u k), SingI u) ⇒ IdToken → HIVisual p u (Node u k) → HIArea p → Node u k → [HoloItem p] → HoloItem p
node idToken visual area holo =
  item (sing ∷ SUnitK u) visual mempty area (nodeGeo holo) idToken holo

leaf ∷ ∀ u a. (Holo u a, SingI u) ⇒ IdToken → a → StyleOf u a → HoloItem Blank
leaf idToken holo hiStyle =
  item (sing ∷ SUnitK u) () hiStyle () mempty         idToken holo []


-- * Pre-package tree constructors
--
vbox, hbox ∷ [HoloItem Layout] → HoloItem Layout
-- XXX: here's trouble -- we're using blankIdToken!
hbox = node blankIdToken () (Area (mkLU 0 0) (mkSize 0 0)) (HBoxN ∷ Node PU HBox)
vbox = node blankIdToken () (Area (mkLU 0 0) (mkSize 0 0)) (VBoxN ∷ Node PU VBox)


-- * Leaves

data Rect u where
  Rect ∷
    { _rectDim   ∷ Di (Unit u)
    , _rectColor ∷ Co Double
    } → Rect u
-- makeLenses ''Rect

instance (FromUnit u) ⇒ Semigroup (RectStyle u) where
  _ <> _ = RectStyle
instance (FromUnit u) ⇒ Monoid    (RectStyle u) where
  mempty      = RectStyle

type RectStyle  u = StyleOf  u (Rect u)
type RectVisual u = VisualOf u (Rect u)
instance FromUnit u ⇒ Holo   u (Rect u) where
  data StyleOf  u (Rect u) where RectStyle  ∷ RectStyle u
  data VisualOf u (Rect u) where RectVisual ∷ { rectDrawable ∷ Drawable } → RectVisual u
  drawableOf _ = rectDrawable
  query     port _       Rect{..} = pure $ Just ∘ fromPU ∘ fromUnit (portDΠ port) <$>_rectDim
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
instance (FromUnit u) ⇒ Semigroup (TextStyle u) where
  TextStyle lfk (TextSizeSpec lws lhl) lco <> TextStyle rfk (TextSizeSpec rws rhl) rco = TextStyle
    { _tsFontKey     = choosePartially "default" lfk rfk
    , _tsSizeSpec    = TextSizeSpec (lws <|> rws) (choosePartially OneLine lhl rhl)
    , _tsColor       = lco <> rco
    }
instance (FromUnit u) ⇒ Monoid    (TextStyle u) where
  mempty = TextStyle
    { _tsFontKey     = "default"
    , _tsSizeSpec    = TextSizeSpec Nothing OneLine
    , _tsColor       = white
    }

type TextStyle  u = StyleOf  u T.Text
type TextVisual u = VisualOf u T.Text
instance FromUnit u ⇒ Holo u T.Text where
  data StyleOf  u T.Text where
    TextStyle ∷
      { _tsFontKey     ∷ FontKey
      , _tsSizeSpec    ∷ TextSizeSpec u
      , _tsColor       ∷ Co Double
      } → TextStyle u
  data VisualOf u T.Text where
    Text ∷
      { tStyle         ∷ TextStyle u
      , tDrawable      ∷ Drawable
      , tFont          ∷ WFont Bound
      , tLayout        ∷ GIP.Layout
      , tDim           ∷ Di (Unit u)
      } → TextVisual u
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

tsFontKey   ∷ Lens' (TextStyle u) FontKey
tsFontKey  f ts@(TextStyle x _ _) = (\xx→ts{_tsFontKey=xx})  <$> f x
tsSizeSpec  ∷ Lens' (TextStyle u) (TextSizeSpec u)
tsSizeSpec f ts@(TextStyle _ x _) = (\xx→ts{_tsSizeSpec=xx}) <$> f x
tsColor     ∷ Lens' (TextStyle u) (Co Double)
tsColor    f ts@(TextStyle _ _ x) = (\xx→ts{_tsColor=xx})    <$> f x


type TextZipperStyle  u = StyleOf  u (T.TextZipper T.Text)
type TextZipperVisual u = VisualOf u (T.TextZipper T.Text)

instance (FromUnit u) ⇒ Semigroup (TextZipperStyle u) where
  TextZipperStyle l <> TextZipperStyle r = TextZipperStyle $ l <> r
instance (FromUnit u) ⇒ Monoid    (TextZipperStyle u) where
  mempty = TextZipperStyle mempty

instance FromUnit u ⇒ Holo  u  (T.TextZipper T.Text) where
  data StyleOf u  (T.TextZipper T.Text) where
    TextZipperStyle ∷
      { fromTextZipperStyle ∷ TextStyle u
      } → TextZipperStyle u
  data VisualOf u (T.TextZipper T.Text) where
    TextZipper ∷
      { teText ∷ VisualOf u T.Text
      } → TextZipperVisual u
  drawableOf _ = tDrawable ∘ teText
  query port TextZipperStyle{..} tz =
    query port fromTextZipperStyle (zipperText tz)
  createVisual port hsty area' content drw = do
    TextZipper <$> createVisual port (hsty & fromTextZipperStyle) area' (zipperText content) drw
  renderVisual _ (TextZipper Text{..}) content = do
    -- XXX: cursor position
    drawableDrawText tDrawable tLayout (_tsColor tStyle) (zipperText content)

tesTSStyle  ∷ Lens' (TextZipperStyle u) (TextStyle u)
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

{-# LANGUAGE AllowAmbiguousTypes, ExplicitForAll, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RankNTypes, UndecidableInstances #-}
{-# LANGUAGE DataKinds, GADTs, NoMonomorphismRestriction, TypeFamilies, TypeFamilyDependencies, TypeInType #-}
{-# LANGUAGE LambdaCase, OverloadedLists, OverloadedStrings, PartialTypeSignatures, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TupleSections, TypeOperators, ViewPatterns #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors #-}
module Holo
  (
  -- * Define this:
    Holo(..)
  -- * Get that:
  , StyleOf(..), VisualOf(..)
  , KNode(..), Node(..)
  , mkHoloitem
  , mkHoloNode
  , holoVBox, holoHBox
  , holoLeaf
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
import qualified Data.Set                          as Set
import           Data.Singletons
import qualified Data.Text                         as T
import qualified Data.Text.Zipper                  as T
import qualified Data.Unique                       as U
import           Linear
import           Prelude                           hiding ((.), id)

import qualified GI.Pango                          as GIP

-- Local imports
import           Elsewhere
import           Flatland
import           Flex                              as Flex
import           HoloCube                                 (Frame)
import           HoloFont
import           HoloPort


-- | 'Holo': anything visualisable.
class (FromUnit u, Monoid (StyleOf u a)) ⇒ Holo (u ∷ UnitK) a where
  data VisualOf u a
  data StyleOf  u a
  -- | Given:
  --   1. global context
  --   2. geometry-enriched style
  --   3. datum
  --   Produce an initial visualisation.
  query           ∷ (MonadIO m, FromUnit u) ⇒ Port → StyleOf u a →                  a →            m (Di (Maybe Double))
  createVisual    ∷ (MonadIO m, FromUnit u) ⇒ Port → StyleOf u a → Area'LU Double → a → Drawable → m (VisualOf u a)
  -- * Question: what do we change, to allow animation of style?
  --
  -- The current model doesn't allow for it.
  renderVisual    ∷ (MonadIO m) ⇒ Port →            VisualOf u a → a → m ()           -- ^ Update a visualisation of 'a'.
  -- For: drawHolotree
  drawableOf      ∷ Port → VisualOf u a → Drawable


-- * HoloItem
--
data Phase
  = Blank
  | Layout
  | Visual

type family HIArea   (p ∷ Phase) ∷ Type where
  HIArea   Blank  = ()
  HIArea   Layout = Area'LU Double
  HIArea   Visual = Area'LU Double

type family HIVisual (p ∷ Phase) u a ∷ Type where
  HIVisual Blank  u a = ()
  HIVisual Layout u a = ()
  HIVisual Visual u a = VisualOf u a

data HoloItem (p ∷ Phase) where
  HoloItem ∷ (FromUnit u, Holo u a) ⇒
    { holo       ∷ a
    , hiToken    ∷ IdToken
    , hiUnit     ∷ SUnitK u
    , hiGeo      ∷ Geo
    , hiStyle    ∷ StyleOf u a
    , hiChildren ∷ [HoloItem p]
    -- Problem:
    -- 1. We have size for top entry, want to record it
    -- 2. The tree is type-coherent, and children need have the same type.
    , hiSize     ∷ Di (Maybe Double)
    , hiArea     ∷ HIArea p
    , hiVisual   ∷ HIVisual p u a
    } → HoloItem p

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

instance Monoid (StyleOf u ()) where
  mappend _ _ = mempty
  mempty      = UnitStyle

instance Monoid (HoloItem Blank) where
  mappend l r = mempty
  mempty      = HoloItem () blankIdToken SPU mempty UnitStyle [] (Di $ V2 Nothing Nothing) mempty ()
instance Monoid (HoloItem Layout) where
  mappend l r = holoVBox [l, r]
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
      hiSize     ← query port hiStyle holo
      let hiArea     = mempty
          hiChildren = children
      pure HoloItem{..}

visualiseHoloitem ∷ (MonadIO m) ⇒ Port → HoloItem Layout → [HoloItem Visual] → m (HoloItem Visual)
visualiseHoloitem port HoloItem{..} hiChildren' = do
  drw      ← establishSizedDrawableForId port hiToken (hiArea^.area'b.size'di)
  hiVisual ← createVisual port hiStyle hiArea holo drw
  let hiChildren = hiChildren'
  pure HoloItem{..}

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

instance Monoid (StyleOf u (Node u k)) where
  mempty      = NodeStyle
  mappend _ _ = NodeStyle

nodeGeo ∷ Node u k → Geo
nodeGeo HBoxN = mempty & grow .~ 1 & direction .~ DirRow
nodeGeo VBoxN = mempty & grow .~ 1 & direction .~ DirColumn


-- * Layout tree Item constructors
--
-- XXX: this FromUnit constraint is a genuine pain.

mkHoloitem ∷ ∀ p u a. (FromUnit u, Holo u a, SingI u) ⇒ SUnitK u → HIVisual p u a → StyleOf u a → HIArea p → Geo → IdToken → a → [HoloItem p] → HoloItem p
mkHoloitem hiUnit hiVisual hiStyle hiArea hiGeo hiToken holo hiChildren =
  let hiSize = Di (V2 Nothing Nothing)
  in HoloItem{..}

mkHoloNode ∷ ∀ p u k. (FromUnit u, Holo u (Node u k), SingI u) ⇒ IdToken → HIVisual p u (Node u k) → HIArea p → Node u k → [HoloItem p] → HoloItem p
mkHoloNode idToken visual area holo =
  mkHoloitem (sing ∷ SUnitK u) visual mempty area (nodeGeo holo) idToken holo

holoLeaf ∷ ∀ u a. (Holo u a, SingI u) ⇒ IdToken → a → StyleOf u a → HoloItem Blank
holoLeaf idToken holo hiStyle =
  mkHoloitem (sing ∷ SUnitK u) () hiStyle () mempty         idToken holo []


-- * Pre-package tree constructors
--
holoVBox, holoHBox ∷ [HoloItem Layout] → HoloItem Layout
holoHBox = mkHoloNode blankIdToken () (Area (mkLU 0 0) (mkSize 0 0)) (HBoxN ∷ Node PU HBox)
holoVBox = mkHoloNode blankIdToken () (Area (mkLU 0 0) (mkSize 0 0)) (VBoxN ∷ Node PU VBox)


-- * Leaves

data Rect u where
  Rect ∷
    { _rectDim   ∷ Di (Unit u)
    , _rectColor ∷ Co Double
    } → Rect u
-- makeLenses ''Rect

instance (FromUnit u) ⇒ Monoid (RectStyle u) where
  mempty      = RectStyle
  mappend _ _ = RectStyle

type RectStyle  u = StyleOf  u (Rect u)
type RectVisual u = VisualOf u (Rect u)
instance FromUnit u ⇒ Holo   u (Rect u) where
  data StyleOf  u (Rect u) where RectStyle  ∷ RectStyle u
  data VisualOf u (Rect u) where RectVisual ∷ { rectDrawable ∷ Drawable } → RectVisual u
  drawableOf _ = rectDrawable
  query     port _       Rect{..} = pure $ Just ∘ fromPU ∘ fromUnit (portDΠ port) <$>_rectDim
  createVisual port@Port{..} _ _area Rect{..} _drw = do
    let dim = PUs <$> _area^.area'b.size'di
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
instance (FromUnit u) ⇒ Monoid (TextStyle u) where
  mempty = TextStyle
    { _tsFontKey     = "default"
    , _tsSizeSpec    = TextSizeSpec Nothing OneLine
    , _tsColor       = white
    }
  TextStyle lfk (TextSizeSpec lws lhl) lco `mappend` TextStyle rfk (TextSizeSpec rws rhl) rco = TextStyle
    { _tsFontKey     = choosePartially "default" lfk rfk
    , _tsSizeSpec    = TextSizeSpec (lws <|> rws) (choosePartially OneLine lhl rhl)
    , _tsColor       = lco <> rco
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
    let font = portFont' port _tsFontKey -- XXX: non-total
        tDim = fromUnit (portDΠ port) ∘ PUs <$> dimOf area'
    liftIO $ putStrLn $ printf "%s → %s" (show _tsFontKey) (show font)
    (,) tFont tLayout ← drawableBindFontLayout (portDΠ port) tDrawable font tDim _tsSizeSpec
    pure Text{..}
  renderVisual _ Text{..} text =
    drawableDrawText tDrawable tLayout (_tsColor tStyle) text

tsFontKey   ∷ Lens' (TextStyle u) FontKey
tsFontKey  f ts@(TextStyle x _ _) = (\xx→ts{_tsFontKey=xx})  <$> f x
tsSizeSpec  ∷ Lens' (TextStyle u) (TextSizeSpec u)
tsSizeSpec f ts@(TextStyle _ x _) = (\xx→ts{_tsSizeSpec=xx}) <$> f x
tsColor     ∷ Lens' (TextStyle u) (Co Double)
tsColor    f ts@(TextStyle _ _ x) = (\xx→ts{_tsColor=xx})    <$> f x


type TextZipperStyle  u = StyleOf  u (T.TextZipper T.Text)
type TextZipperVisual u = VisualOf u (T.TextZipper T.Text)

instance (FromUnit u) ⇒ Monoid (TextZipperStyle u) where
  mempty = TextZipperStyle mempty
  TextZipperStyle l `mappend` TextZipperStyle r = TextZipperStyle $ l <> r

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

{-# LANGUAGE AllowAmbiguousTypes, ExplicitForAll, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RankNTypes, UndecidableInstances #-}
{-# LANGUAGE DataKinds, GADTs, NoMonomorphismRestriction, TypeFamilies, TypeFamilyDependencies, TypeInType #-}
{-# LANGUAGE LambdaCase, OverloadedLists, OverloadedStrings, PartialTypeSignatures, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TupleSections, TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors #-}
module Holo
  (
  -- * Define this:
    Holo(..)
  -- * Get that:
  , StyleOf(..), VisualOf(..)
  , KNode(..), Node(..)
  , holoNode
  , holoVBox, holoHBox
  , holoLeaf
  , Ghola(..), GState(..)
  , drawGholaAt
  , HoloItem
  , queryHoloItem
  , visualiseHoloItem
  , drawHoloItem
  -- * Holo instances
  , Rect(..), RectStyle, RectVisual
  , TextStyle, TextVisual
  , TextZipperStyle, TextZipperVisual
  , tsFontKey, tsSizeSpec, tsColor, tesTSStyle
  )
where

import           HoloPrelude

import qualified Data.Text                         as T
import qualified Data.Text.Zipper                  as T
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
  query           ∷ (MonadIO m, FromUnit u) ⇒ Port → StyleOf u a →               a → m (Di (Maybe Double))
  visualise       ∷ (MonadIO m, FromUnit u) ⇒ Port → StyleOf u a → Area Double → a → m (VisualOf u a)
  -- * Question: what do we change, to allow animation of style?
  --
  -- The current model doesn't allow for it.
  updateVisual    ∷ (MonadIO m) ⇒ Port →            VisualOf u a → a → m ()           -- ^ Update a visualisation of 'a'.
  drawableOf      ∷ VisualOf u a → Drawable


-- | 'HoloItem' / 'Ghola': raison d'etre: type-free payload of Flex's item tree.
--
-- | Designate two passes over the tree:
--   1. Post-'query', pre-layout
--   2. Post-layout, with drawables allocated.
data GState
  = Blank
  | Sized
  | Visual

type family GholaVisualOf u a (s ∷ GState) where
  GholaVisualOf u a Blank  = ()
  GholaVisualOf u a Sized  = ()
  GholaVisualOf u a Visual = VisualOf u a

data Ghola (k ∷ GState) where
  Ghola ∷ (Holo u a, Monoid (StyleOf u a)) ⇒
    { _holo     ∷ a
    , _uptodate ∷ Bool
    , _style    ∷ StyleOf u a
    , _visual   ∷ GholaVisualOf u a k
    } → Ghola k

drawGholaAt ∷ (MonadIO m) ⇒ Frame → Ghola Visual → LU Double → m ()
drawGholaAt frame  Ghola{..} pos = do
  --liftIO $ printf "sWHA %s | " (show $ pos^.lu'po.po'v)
  drawableContentToGPU   (drawableOf _visual)
  framePutDrawable frame (drawableOf _visual) (doubleToFloat <$> pos^.lu'po)


type family HoloItemContent (s ∷ GState) ∷ Type where
  HoloItemContent Blank  = Ghola Blank
  HoloItemContent Sized  = (Di Double, Ghola Sized)
  HoloItemContent Visual = (Di Double, Ghola Visual)

type HoloItem (s ∷ GState) = Item (HoloItemContent s)

queryHoloItem ∷ (MonadIO m) ⇒ Port → HoloItem Blank → m (HoloItem Sized)
queryHoloItem port hoi =
  case hoi^.this of
    Ghola{..} → do
      dim ← query port _style _holo
      pure $ hoi & place.size .~ dim

visualiseHoloItem ∷ (MonadIO m) ⇒ Port → HoloItem Sized → m (HoloItem Visual)
visualiseHoloItem port hoi =
  case hoi^.this of
    Ghola{..} → do
      _visual ← visualise port _style (hoi^.area) _holo
      pure $ Item
        { _geo      = hoi^.geo
        , _place    = hoi^.place
        , _this     = Ghola{..} ∷ Ghola Visual
        , _children = []        -- XXX: landmine
        , _area     = hoi^.area }

drawHoloItem ∷ (MonadIO m) ⇒ Frame → HoloItem Visual → m ()
drawHoloItem frame item =
  drawGholaAt frame (item^.this) (luOf $ item^.area)


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
  drawableOf           = (⊥)
  query        _ _ _   = pure $ Di $ V2 Nothing Nothing
  visualise    _ _ _ _ = pure NodeVisual
  updateVisual _ _ _   = pure ()

instance Monoid (StyleOf u (Node u k)) where
  mempty      = NodeStyle
  mappend _ _ = NodeStyle

nodeGeo ∷ Node u k → Geo
nodeGeo HBoxN = mempty & direction .~ DirColumn
nodeGeo VBoxN = mempty & direction .~ DirRow


-- * Layout tree Item constructors
--
-- XXX: this FromUnit constraint is a genuine pain.
holoNode  ∷ FromUnit u ⇒ Node u k → [HoloItem Query] → HoloItem Query
holoNode selector chi =
  flip (mkItem (nodeGeo selector) mempty) chi $ Ghola selector False NodeStyle ()

holoLeaf ∷ Holo u a ⇒ Port → a → StyleOf u a → HoloItem Query
holoLeaf _port holo sty = do
  flip (mkItem defaultGeo         mempty) []  $ Ghola holo     False sty       ()


-- * Pre-package tree constructors
--
holoVBox, holoHBox ∷ [HoloItem Query] → HoloItem Query
holoHBox = holoNode (HBoxN ∷ Node PU HBox)
holoVBox = holoNode (VBoxN ∷ Node PU VBox)


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
  drawableOf = rectDrawable
  query     port _       Rect{..} = pure $ Just ∘ fromPU ∘ fromUnit (portDΠ port) <$>_rectDim
  visualise port _ _area Rect{..} =
    RectVisual <$> mkRectDrawable port _rectDim _rectColor
  updateVisual port v@RectVisual{rectDrawable=Drawable{..}} Rect{..} = do
    redrawRectDrawable port (rectDrawable v) _rectColor _rectDim


-- * This is a complicated story:
--
-- Actors:
--  1. u-free text style
--  2. PU-wired Fontmap from the Port 
mkText ∷ ∀ m u. (MonadIO m, FromUnit u) ⇒ Port → TextStyle u → Maybe T.Text → m (VisualOf u T.Text)
mkText port@Port{..} tStyle@TextStyle{..} mText = do
  let Settings{..} = portSettings
      font         = lookupFont' portFontmap _tsFontKey
  tDim ← (fromUnit sttsDΠ <$>) ∘ either errorT id <$> fontQuerySize font (convert sttsDΠ _tsSizeSpec) mText
 
  tDrawable         ← makeDrawable port $ fromPU ∘ fromUnit sttsDΠ <$> tDim
  (,) tFont tLayout ← drawableBindFontLayout sttsDΠ tDrawable font tDim _tsSizeSpec
  pure $ Text{..}

drawText ∷ (MonadIO m) ⇒ VisualOf u T.Text → T.Text → m ()
drawText Text{..} text =
  drawableDrawText tDrawable tLayout (_tsColor tStyle) text

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
  drawableOf = tDrawable
  query port@Port{..} TextStyle{..} content = do
    let font = portFont' port _tsFontKey -- XXX: non-total
    (Just ∘ fromPU <$>) ∘ either errorT id <$> fontQuerySize font (convert (portDΠ port) _tsSizeSpec) (partial (≢ "") content)
  visualise port tStyle@TextStyle{..} area content = do
    let font = portFont' port _tsFontKey -- XXX: non-total
        tDim = fromUnit (portDΠ port) ∘ PUs <$> dimOf area
    tDrawable         ← makeDrawable port (dimOf area)
    (,) tFont tLayout ← drawableBindFontLayout (portDΠ port) tDrawable font tDim _tsSizeSpec
    pure Text{..}
  updateVisual _ = drawText

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
  drawableOf = tDrawable ∘ teText
  query port TextZipperStyle{..} tz =
    query port fromTextZipperStyle (zipperText tz)
  visualise port hsty area content = do
    TextZipper <$> visualise port (hsty & fromTextZipperStyle) area (zipperText content)
  updateVisual _ (TextZipper txt@Text{..}) content = do
    -- XXX: cursor position
    drawText txt (zipperText content)

tesTSStyle  ∷ Lens' (TextZipperStyle u) (TextStyle u)
tesTSStyle f (TextZipperStyle x) = TextZipperStyle <$> f x


-- visual ∷ (ReflexGLFWCtx t m, Holo a) ⇒ Settings PU → ObjectStream → StyleOf (Visual a) → Event t (a, b) → m (Event t (Holosome a, b))
-- visual stts holoStream holoStyle holoE =
--   performEvent (holoE <&> ((\(holo, x) → liftIO $ do
--                                -- XXX/expressivity:  this threading of 'x' is..
--                                holoVisual ← visualise stts holoStream holoStyle holo
--                                holoRef    ← IO.newIORef holo
--                                -- holoPosRef ← IO.newIORef pos
--                                pure (Holosome{..}, x))
--                           ))

-- update ∷ (MonadIO m, Holo a) ⇒ Settings PU → Holosome a → (a → a) → m ()
-- update stts Holosome{..} f = do
--   old ← liftIO $ IO.readIORef holoRef
--   let new = f old
--   liftIO $ IO.writeIORef holoRef new
--   updateVisual stts holoStream holoVisual new

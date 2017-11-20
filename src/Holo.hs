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
  , HoloStyle(..)
  , hsPlacement, hsPlace, hsStyle
  , WHolo(..)
  , showWHoloAt
  , KNode(..), Node(..)
  , defaultNodeStyle
  , holoVBox, holoHBox
  , holoLeaf
  , HoloItem
  , showHoloItem
  -- * Holo instances
  , StyleOf(..), VisualOf(..)
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
import           HoloFont
import           HoloCube                                 (Frame)
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
  visualise       ∷ (MonadIO m, FromUnit u) ⇒ Port u → HoloStyle (StyleOf u a) → a → m (VisualOf u a)
  -- * Question: what do we change, to allow animation of style?
  --
  -- The current model doesn't allow for it.
  updateVisual    ∷ (MonadIO m) ⇒ Port u →            VisualOf u a → a → m ()           -- ^ Update a visualisation of 'a'.
  drawableOf      ∷ VisualOf u a → Drawable

data HoloStyle a where
  HoloStyle ∷
    { _hsPlacement ∷ Flex.Style
    , _hsPlace     ∷ Flex.Place
    , _hsStyle     ∷ a
    } → HoloStyle a
makeLenses ''HoloStyle

instance Monoid a ⇒ Monoid (HoloStyle a) where
  mempty = HoloStyle mempty mempty mempty
  HoloStyle lpt lpl lst `mappend` HoloStyle rpt rpl rst =
    HoloStyle (lpt <> rpt) (lpl <> rpl) (lst <> rst)


-- | 'WHolo': (Wrap Holo) raison d'etre: type-free payload of Flex's item tree.
type HoloItem = Item WHolo

data WHolo where
  Box    ∷ (Holo u a, Monoid (StyleOf u a)) ⇒
    { _holo     ∷ a
    , _uptodate ∷ Bool
    , _style    ∷ HoloStyle (StyleOf u a)
    } → WHolo
  Visual ∷ (Holo u a, Monoid (StyleOf u a)) ⇒
    { _holo     ∷ a
    , _uptodate ∷ Bool
    , _style    ∷ HoloStyle (StyleOf u a)
    , _visual   ∷ VisualOf u a
    } → WHolo

showWHoloAt ∷ (MonadIO m) ⇒ Frame → WHolo → LU Double → m ()
showWHoloAt _      Box{..}    _   = pure ()
showWHoloAt frame  Visual{..} pos = do
  drawableContentToGPU   (drawableOf _visual)
  framePutDrawable frame (drawableOf _visual) (doubleToFloat <$> pos^.lu'po)

showHoloItem ∷ (MonadIO m) ⇒ Frame → HoloItem → m ()
showHoloItem frame item =
  showWHoloAt frame (item^.this) (lu $ item^.area)


-- * Internal nodes
data KNode
  = VBox
  | HBox

data Node (u ∷ UnitK) (t ∷ KNode) where
  HBoxN ∷ Node u HBox
  VBoxN ∷ Node u VBox

defaultNodeStyle ∷ Node u k → HoloStyle (StyleOf u (Node u k))
defaultNodeStyle HBoxN = HoloStyle sty mempty NodeStyle
  where sty = mempty & direction .~ DirColumn
defaultNodeStyle VBoxN = HoloStyle sty mempty NodeStyle
  where sty = mempty & direction .~ DirRow

instance FromUnit u ⇒ Holo   u (Node u (k ∷ KNode)) where
  data StyleOf  u (Node u k) = NodeStyle
  data VisualOf u (Node u k) = NodeVisual
  drawableOf         = (⊥)
  visualise    _ _ _ = pure NodeVisual
  updateVisual _ _ _ = pure ()

instance Monoid (StyleOf u (Node u k)) where
  mempty      = NodeStyle
  mappend _ _ = NodeStyle

-- XXX: this FromUnit constraint is a genuine pain.
holoBox  ∷ FromUnit u ⇒ Node u k → [HoloItem] → HoloItem
holoBox boxSelector chi = flip mkItem chi (Box boxSelector False (defaultNodeStyle boxSelector))

holoVBox, holoHBox ∷ [HoloItem] → HoloItem
holoHBox = holoBox (HBoxN ∷ Node PU HBox)
holoVBox = holoBox (VBoxN ∷ Node PU VBox)

holoLeaf ∷ (MonadIO m, Holo u a, FromUnit u) ⇒ Port u → HoloStyle (StyleOf u a) → a → m HoloItem
holoLeaf port sty holo = flip mkItem [] ∘ Visual holo False sty <$> visualise port sty holo


-- * Leaves
mkText ∷ (MonadIO m, FromUnit u) ⇒ Port u → TextStyle u → Maybe T.Text → m (VisualOf u T.Text)
mkText port@Port{..} tStyle@TextStyle{..} mText = do
  let Settings{..}  = portSettings
      font@Font{..} = lookupFont' portFontmap _tsFontKey
  tDim ← either errorT id <$> fontQuerySize font _tsSizeSpec mText
 
  let pixelSize = fromPU ∘ fromUnit sttsDΠ <$> tDim

  tDrawable             ← makeDrawable port pixelSize
  tFont@FontBinding{..} ← bindFont font $ dGIC tDrawable
  tLayout               ← makeTextLayout fbContext
  -- XXX: here we meet the need for layout for the first time..
  laySetSize        tLayout sttsDΠ tDim
  laySetHeightLimit tLayout (_tsSizeSpec^.tssHeight)
  pure Text{..}

drawText ∷ (MonadIO m) ⇒ VisualOf u T.Text → T.Text → m ()
drawText Text{..} text = do
  let TextStyle{..}                  = tStyle
      FontBinding{fbFont = Font{..}} = tFont
      Drawable{..}                   = tDrawable
  layDrawText dCairo dGIC tLayout (po 0 0) _tsColor text

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
      , tFont          ∷ Font Bound u
      , tLayout        ∷ GIP.Layout
      , tDim           ∷ Di (Unit u)
      } → TextVisual u
  drawableOf = tDrawable
  visualise port (HoloStyle _ place' sty) content = do
    case (place'^.size.di'v._x, sty^.tsSizeSpec.tssWidth) of
      (Just _geostyw, Just (Wi _textstyle)) → pure () --flip assert (pure ()) $ (geostyw ≡ fromPU ∘ fromUnit textstyle)
      _ → error "Geometry and font style sizes completely out of sync."
    mkText port sty $ partial (≢ "") content
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
  visualise port hsty content = do
    TextZipper <$> visualise port (hsty & hsStyle %~ fromTextZipperStyle) (zipperText content)
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

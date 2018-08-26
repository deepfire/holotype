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
  , Item, hiToken, hiArea
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
import           Data.Typeable
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
import qualified HoloCairo                         as Cr
import           HoloPort


-- * Item

instance Eq (Item a) where
  (==) a b = (≡) (hiToken a) (hiToken b)

instance Ord (Item a) where
  compare a b = compare (hiToken a) (hiToken b)

holotreeLeaves ∷ Item a → Map.Map IdToken (Item a)
holotreeLeaves root = Map.fromList $ walk root
  where walk x@(hiChildren → []) = [(hiToken x, x)]
        walk Item{..}        = concat $ walk <$> hiChildren


-- * Minimal
instance Holo () where
  data StyleOf  ()     = UnitStyle
  data VisualOf ()     = UnitVisual
  query        _ _ _     = pure $ Di $ V2 Nothing Nothing
  createVisual _ _ _ _ _ = pure UnitVisual
  renderVisual _ _ _     = pure ()
  freeVisualOf _ _       = pure ()
instance DefStyleOf (StyleOf ()) where
  defStyleOf = UnitStyle

-- instance Monoid    (Item PBlank) where
--   mempty      = Item () blankIdToken mempty UnitStyle [] (Di $ V2 Nothing Nothing) mempty ()

-- instance Semigroup (Item PLayout) where
--   l <> r = vbox [l, r]
-- instance Monoid    (Item PLayout) where
--   mempty      = Item () blankIdToken mempty UnitStyle [] (Di $ V2 Nothing Nothing) mempty ()
-- instance Monoid (Item Visual) where
--   mappend l r = holoVBox [l, r]
--   mempty      = Item () blankIdToken SPU mempty UnitStyle [] (Di $ V2 Nothing Nothing) mempty UnitVisual

emptyDrawable ∷ HasCallStack ⇒ Drawable
emptyDrawable = (⊥) -- hihi

emptyLayoutHolo ∷ Item PLayout
emptyLayoutHolo =
  Item () blankIdToken (Style UnitStyle (StyleGene 0)) mempty [] (Di $ V2 Nothing Nothing) mempty ()

emptyVisualHolo ∷ HasCallStack ⇒ Item PVisual
emptyVisualHolo =
  Item () blankIdToken (Style UnitStyle (StyleGene 0)) mempty [] (Di $ V2 Nothing Nothing) mempty (Visual UnitVisual (undefined) emptyDrawable)


instance Flex (Item PBlank) where
  geo      f hi@Item{..} = (\x→ hi {hiGeo=x})      <$> f hiGeo
  size     f hi@Item{..} = (\x→ hi {hiSize=x})     <$> f hiSize
  children f hi@Item{..} = (\x→ hi {hiChildren=x}) <$> f hiChildren
  -- area     f hi@Item{..} = (\x→ hi {hiArea=x})     <$> f hiArea

instance Flex (Item PLayout) where
  geo      f hi@Item{..} = (\x→ hi {hiGeo=x})      <$> f hiGeo
  size     f hi@Item{..} = (\x→ hi {hiSize=x})     <$> f hiSize
  children f hi@Item{..} = (\x→ hi {hiChildren=x}) <$> f hiChildren
  area     f hi@Item{..} = (\x→ hi {hiArea=x})     <$> f hiArea

instance Flex (Item PVisual) where
  geo      f hi@Item{..} = (\x→ hi {hiGeo=x})      <$> f hiGeo
  size     f hi@Item{..} = (\x→ hi {hiSize=x})     <$> f hiSize
  children f hi@Item{..} = (\x→ hi {hiChildren=x}) <$> f hiChildren
  area     f hi@Item{..} = (\x→ hi {hiArea=x})     <$> f hiArea


-- | 'Item': raison d'etre: type-free payload of Flex's item tree.
--
-- | Designate three passes over the tree:
--   1. Query
--   2. Layout
--   2. CreateVisual

queryHoloitem ∷ (MonadIO m) ⇒ Port → Item PBlank → [Item PLayout] → m (Item PLayout)
queryHoloitem port hoi children =
  case hoi of
    Item{..} → do
      size ← query port (_sStyle $ hiStyle) holo
      pure Item{hiSize=size, hiArea=mempty, hiChildren=children, ..}

renderHoloitem ∷ (MonadIO m) ⇒ Port → Item PVisual → m ()
renderHoloitem port Item{..} = do
  let drw = vDrawable hiVisual
  clearDrawable drw
  renderVisual port (vVisual hiVisual) holo
  drawableContentToGPU drw


queryHolotree ∷ (MonadIO m) ⇒ Port → Item PBlank → m (Item PLayout)
queryHolotree port hoi@Item{..} =
  queryHoloitem     port hoi =<< (sequence $ queryHolotree     port <$> hiChildren)

visualiseHolotree ∷ (MonadIO m) ⇒ Port → Item PLayout → m (Item PVisual)
visualiseHolotree port hoi@Item{..} =
  visualiseHoloitem port hoi =<< (sequence $ visualiseHolotree port <$> hiChildren)

renderHolotreeVisuals ∷ (MonadIO m) ⇒ Port → Item PVisual → m ()
renderHolotreeVisuals port hoi@Item{..} = do
  renderHoloitem port hoi
  forM_ hiChildren (renderHolotreeVisuals port)

drawHolotreeVisuals ∷ (MonadIO m) ⇒ Frame → Item PVisual → m ()
drawHolotreeVisuals frame root = loop (luOf (hiArea root)^.lu'po) root
  where
    loop offset Item{..} = do
      if null hiChildren
      then do
        framePutDrawable frame (vDrawable hiVisual) (doubleToFloat <$> (offset + luOf hiArea^.lu'po))
        -- liftIO $ putStrLn $ "draw -- " <> (show $ luOf hiArea^.lu'po) <> " " <> (TL.unpack $ rendCompact $ pretty'Area hiArea) <> " " <> (TL.unpack $ rendCompact $ pretty'Area (area'LU hiArea))
      else do
        forM_ hiChildren $ loop (luOf hiArea^.lu'po)


-- * Internal nodes
data KNode
  = VBox
  | HBox
  deriving Typeable

data Node (k ∷ KNode) where
  HBoxN ∷ Node HBox
  VBoxN ∷ Node VBox
  deriving Typeable

instance Typeable k ⇒ Holo   (Node (k ∷ KNode)) where
  data StyleOf  (Node k) = NodeStyle
  data VisualOf (Node k) = NodeVisual
  query        _ _ _     = pure $ Di $ V2 Nothing Nothing
  createVisual _ _ _ _ _ = pure NodeVisual
  renderVisual _ _ _     = pure ()
  freeVisualOf _ _       = pure ()
instance DefStyleOf (StyleOf (Node k)) where
  defStyleOf = NodeStyle

nodeGeo ∷ Node k → Geo
nodeGeo HBoxN = mempty & Flex.grow .~ 1 & Flex.direction .~ Flex.DirRow
nodeGeo VBoxN = mempty & Flex.grow .~ 1 & Flex.direction .~ Flex.DirColumn


-- * Layout tree Item constructors
--
-- XXX: this FromUnit constraint is a genuine pain.

item ∷ ∀ p a. (Holo a)
  ⇒ IdToken
  → Style a
  → a
  → Geo
  → HIArea p
  → HIVisual p a
  → [Item p]
  → Item p
item hiToken hiStyle holo hiGeo hiArea hiVisual hiChildren =
  let hiSize = Di (V2 Nothing Nothing)
  in Item{..}

node ∷ ∀ p a k. (Holo a, a ~ Node k)
  ⇒ IdToken
  → Style a
  → a
  → HIArea p
  → HIVisual p a
  → [Item p]
  → Item p
node idToken style holo area visual =
  item idToken style holo (nodeGeo holo) area visual

leaf ∷ ∀ a. (Holo a)
  ⇒ IdToken
  → Style a
  → a
  → Item PBlank
leaf idToken hiStyle holo =
  item idToken hiStyle holo mempty () () []


-- * Pre-package tree constructors
--
vbox, hbox ∷ [Item PLayout] → Item PLayout
-- XXX: here's trouble -- we're using blankIdToken!
hbox = node blankIdToken defStyle (HBoxN ∷ Node HBox) noArea ()
vbox = node blankIdToken defStyle (VBoxN ∷ Node VBox) noArea ()


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
  query     port _       Rect{..} = pure $ Just ∘ fromPU ∘ fromUnit (portDΠ port) <$> _rectDim
  createVisual port@Port{..} _ _area Rect{..} drw = do
    let dim = PUs <$> _area^.area'b.size'di
    drawableDrawRect port drw _rectColor dim
    pure $ RectVisual drw
  renderVisual port v@RectVisual{rectDrawable=Drawable{..}} Rect{..} =
    drawableDrawRect port (rectDrawable v) _rectColor _rectDim
  freeVisualOf _ _       = pure ()
instance DefStyleOf (StyleOf Rect) where
  defStyleOf = RectStyle


-- * This is a complicated story:
--
-- Actors:
--  1. u-free text style
--  2. PU-wired Fontmap from the Port
--
-- instance Semigroup TextStyle where
--   TextStyle lfk (TextSizeSpec lws lhl) lco <> TextStyle rfk (TextSizeSpec rws rhl) rco = TextStyle
--     { _tsFontKey     = choosePartially "default" lfk rfk
--     , _tsSizeSpec    = TextSizeSpec (lws <|> rws) (choosePartially OneLine lhl rhl)
--     , _tsColor       = lco <> rco
--     }
instance DefStyleOf (StyleOf T.Text) where
  defStyleOf = TextStyle
    { _tsFontKey     = "default"
    , _tsSizeSpec    = Cr.TextSizeSpec Nothing Cr.OneLine
    , _tsColor       = white
    }

type TextStyle  = StyleOf  T.Text
type TextVisual = VisualOf T.Text
instance Holo  T.Text where
  data StyleOf T.Text where
    TextStyle ∷
      { _tsFontKey     ∷ Cr.FontKey
      , _tsSizeSpec    ∷ Cr.TextSizeSpec PU
      , _tsColor       ∷ Co Double
      } → TextStyle
  data VisualOf T.Text where
    Text ∷
      { tStyle         ∷ TextStyle
      , tDrawable      ∷ Drawable
      , tFont          ∷ Cr.WFont Bound
      , tLayout        ∷ GIP.Layout
      , tDim           ∷ Di (Unit u)
      } → TextVisual
  query port@Port{..} TextStyle{..} content = do
    let font = portFont' port _tsFontKey -- XXX: non-total
    (Just ∘ fromPU <$>) ∘ either errorT id <$> Cr.fontQuerySize font (convert (portDΠ port) _tsSizeSpec) (partial (≢ "") content)
  createVisual port tStyle@TextStyle{..} area' _content tDrawable = do
    -- 1. find font, 2. bind font to GIC, 3. create layout
    -- Q: why not also draw here?  Reflow?
    let font = portFont' port _tsFontKey -- XXX: non-total
        tDim = fromUnit (portDΠ port) ∘ PUs <$> dimOf area'
    -- liftIO $ putStrLn $ printf "createVisual T.Text: %s → %s" (show _tsFontKey) (show font)
    (,) tFont tLayout ← drawableBindFontLayout (portDΠ port) tDrawable font tDim _tsSizeSpec
    -- drawableBindFontLayout allocates:
    --   GIPC.createContext gic  -- released by FFI finalizers
    --   GIP.layoutNew      gipc -- same as above
    pure Text{..}
  renderVisual _ Text{..} text =
    -- 1. execute GIP draw & GIPC composition
    drawableDrawText tDrawable tLayout (_tsColor tStyle) text
  freeVisualOf Text{..} _ =
    Cr.unbindFontLayout tFont tLayout

tsFontKey   ∷ Lens' TextStyle Cr.FontKey
tsFontKey  f ts@(TextStyle x _ _) = (\xx→ts{_tsFontKey=xx})  <$> f x
tsSizeSpec  ∷ Lens' TextStyle (Cr.TextSizeSpec PU)
tsSizeSpec f ts@(TextStyle _ x _) = (\xx→ts{_tsSizeSpec=xx}) <$> f x
tsColor     ∷ Lens' TextStyle (Co Double)
tsColor    f ts@(TextStyle _ _ x) = (\xx→ts{_tsColor=xx})    <$> f x


type TextZipperStyle  = StyleOf  (T.TextZipper T.Text)
type TextZipperVisual = VisualOf (T.TextZipper T.Text)

-- instance Semigroup TextZipperStyle where
--   TextZipperStyle l <> TextZipperStyle r = TextZipperStyle $ l <> r

instance DefStyleOf (StyleOf (T.TextZipper T.Text)) where
  defStyleOf = TextZipperStyle (defStyleOf ∷ StyleOf T.Text)

instance Holo   (T.TextZipper T.Text) where
  data StyleOf  (T.TextZipper T.Text) where
    TextZipperStyle ∷
      { fromTextZipperStyle ∷ TextStyle
      } → TextZipperStyle
  data VisualOf (T.TextZipper T.Text) where
    TextZipper ∷
      { teText ∷ VisualOf T.Text
      } → TextZipperVisual
  query port TextZipperStyle{..} tz =
    query port fromTextZipperStyle (zipperText tz)
  createVisual port hsty area' content drw = do
    TextZipper <$> createVisual port (hsty & fromTextZipperStyle) area' (zipperText content) drw
  renderVisual _ (TextZipper Text{..}) content = do
    -- XXX: cursor position
    drawableDrawText tDrawable tLayout (_tsColor tStyle) (zipperText content)
  freeVisualOf TextZipper{..} _ = freeVisualOf teText Proxy

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

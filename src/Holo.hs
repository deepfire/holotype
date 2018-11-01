{-# LANGUAGE AllowAmbiguousTypes, ExplicitForAll, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RankNTypes, UndecidableInstances #-}
{-# LANGUAGE DataKinds, GADTs, NoMonomorphismRestriction, TypeFamilies, TypeFamilyDependencies, TypeInType #-}
{-# LANGUAGE LambdaCase, OverloadedLists, OverloadedStrings, PackageImports, PartialTypeSignatures, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TupleSections, TypeOperators, ViewPatterns #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors -Wno-orphans -Wno-type-defaults #-}
module Holo
  (
  -- * Define this:
    Holo(..)
  -- * Get that:
  , StyleOf(..), VisualOf(..)
  , KNode(..), Node(..)
  , vbox, hbox
  , item, leafStyled, leaf
  , static
  -- , liftHolo
  -- , widget
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
  , emptyHolo, emptyLayoutHolo, emptyVisualHolo
  , blankIdToken
  -- * Holo instances
  , Rect(..)
  , TextStyle, TextVisual
  , tsFontKey, tsSizeSpec, tsColor
  )
where

import           Control.Monad
import           Data.Text                                (Text)
import           Data.Text.Zipper                         (TextZipper)
import           Data.Typeable
import           Linear
import           Prelude                           hiding ((.), id)
import           Reflex                            hiding (Query, Query(..))
import           Reflex.GLFW                              (RGLFW, InputU(..))
import qualified Data.Map.Strict                   as Map
import qualified Data.Text                         as T
import qualified Data.Text.Zipper                  as T
import qualified GI.Pango                          as GIP
import qualified "GLFW-b" Graphics.UI.GLFW         as GLFW
import qualified Reflex.GLFW                       as GLFW

-- Local imports
import           Elsewhere
import           Flatland
import           Flex                                     (Flex, Geo)
import           HoloPort
import           HoloPrelude
import           HoloTypes
import qualified Flex                              as Flex
import qualified HoloCairo                         as Cr


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
instance DefStyleOf (StyleOf ()) where
  defStyleOf           = UnitStyle
instance Holo   () where
  data StyleOf  ()     = UnitStyle
  data VisualOf ()     = UnitVisual
  compStyle        _   = UnitStyle
  query        _ _ _ _ = pure $ Di $ V2 Nothing Nothing

instance Semigroup (Item PBlank)  where _ <> _ = mempty
instance Monoid    (Item PBlank)  where mempty = Item () blankIdToken (initStyle UnitStyle) mempty [] (Di $ V2 Nothing Nothing) mempty ()
instance Semigroup (Item PLayout) where _ <> _ = mempty
instance Monoid    (Item PLayout) where mempty = Item () blankIdToken (initStyle UnitStyle) mempty [] (Di $ V2 Nothing Nothing) mempty ()

-- instance Semigroup (Item PLayout) where
--   l <> r = vbox [l, r]
-- instance Monoid    (Item PLayout) where
--   mempty      = Item () blankIdToken mempty UnitStyle [] (Di $ V2 Nothing Nothing) mempty ()
-- instance Monoid (Item Visual) where
--   mappend l r = holoVBox [l, r]
--   mempty      = Item () blankIdToken SPU mempty UnitStyle [] (Di $ V2 Nothing Nothing) mempty UnitVisual

emptyHolo ∷ (Monoid (HIArea a), Monoid (HIVisual a ())) ⇒ Item a
emptyHolo =
  Item () blankIdToken (Style UnitStyle (StyleGene 0)) mempty [] (Di $ V2 Nothing Nothing) mempty mempty

emptyLayoutHolo ∷ Item PLayout
emptyLayoutHolo =
  Item () blankIdToken (Style UnitStyle (StyleGene 0)) mempty [] (Di $ V2 Nothing Nothing) mempty ()

emptyVisualHolo ∷ HasCallStack ⇒ Item PVisual
emptyVisualHolo =
  Item () blankIdToken (Style UnitStyle (StyleGene 0)) mempty [] (Di $ V2 Nothing Nothing) mempty Nothing


instance Flex (Item PBlank) where
  geo      f hi@Item{..} = (\x→ hi {hiGeo=x})       <$> f hiGeo
  size     f hi@Item{..} = (\x→ hi {hiSize=x})      <$> f hiSize
  children f hi@Item{..} = (\x→ hi {hiChildren=x})  <$> f hiChildren
  area     f hi@Item{..} = (\_→ hi {hiArea=mempty}) <$> f mempty

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
      size ← query port (_sStyle $ hiStyle) children holo
      trev SIZE HOLO size (tokenHash hiToken)
      pure Item{hiSize=size, hiArea=mempty, hiChildren=children, ..}

renderHoloitem ∷ (MonadIO m) ⇒ Port → Item PVisual → m ()
renderHoloitem port Item{..} = do
  case hiVisual of
    Just Visual{vDrawable=Just drw, vVisual=Just vis} → do
      clearDrawable drw
      renderVisual port vis holo
      drawableContentToGPU drw
    _ → pure ()


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
drawHolotreeVisuals frame root = loop (luOf (hiArea root)^.lu'po) "" root
  where
    loop parOff pfx Item{..} = do
      -- liftIO $ putStrLn $ pfx <> show (offset ^.po'v) <> " " <> Flex.ppItemArea i
      let ourOff = parOff + luOf hiArea^.lu'po
      case hiVisual of
        Just Visual{vDrawable=Just drw} →
          framePutDrawable frame drw (doubleToFloat <$> ourOff)
        _ → pure ()
      forM_ hiChildren $ loop ourOff (pfx <> "  ")


-- * Internal nodes
data KNode
  = VBox
  | HBox
  deriving Typeable

data Node (k ∷ KNode) where
  HBoxN ∷ Node HBox
  VBoxN ∷ Node VBox
  deriving Typeable

boxAxis ∷ Node a → Axis
boxAxis = \case
  HBoxN → X
  VBoxN → Y

instance DefStyleOf (StyleOf (Node k)) where
  defStyleOf             = NodeStyle
instance Typeable k ⇒ Holo   (Node (k ∷ KNode)) where
  data StyleOf  (Node k) = NodeStyle
  data VisualOf (Node k) = NodeVisual
  query        _ _ xs box =
    -- Requirement is a sum of children requirements
    pure $ (Just <$>) $ _reqt'di $ foldl' (reqt'add $ boxAxis box) zero $ (\x→ Reqt (fromMaybe 0 <$> x^.Flex.size)) <$> xs

nodeGeo ∷ Node k → Geo
-- nodeGeo HBoxN = mempty & Flex.grow .~ 1 & Flex.direction .~ Flex.DirRow
-- nodeGeo VBoxN = mempty & Flex.grow .~ 1 & Flex.direction .~ Flex.DirColumn
nodeGeo HBoxN = mempty
                -- & Flex.grow .~ 1
                & Flex.direction .~ Flex.DirRow
                & Flex.align'content .~ Flex.AlignStart
nodeGeo VBoxN = mempty
                -- & Flex.grow .~ 1
                & Flex.direction .~ Flex.DirColumn
                & Flex.align'content .~ Flex.AlignStart


-- * Layout tree Item constructors
--
-- XXX: this FromUnit constraint is a genuine pain.

item ∷ ∀ a. (Holo a)
  ⇒ IdToken
  → Style a
  → a
  → Geo
  → [Item PBlank]
  → Item PBlank
item hiToken hiStyle holo hiGeo hiChildren =
  let hiSize   = Di (V2 Nothing Nothing)
      hiArea   = ()
      hiVisual = ()
  in Item{..}

node ∷ ∀ a k. (Holo a, a ~ Node k)
  ⇒ IdToken
  → Style a
  → a
  → [Item PBlank]
  → Item PBlank
node idToken style holo =
  item idToken style holo (nodeGeo holo)

leafStyled ∷ Holo a
  ⇒ IdToken
  → Style a
  → a
  → Item PBlank
leafStyled tok hiStyle holo =
  item tok hiStyle holo mempty []

leaf ∷ Holo a
  ⇒ IdToken
  → a
  → Item PBlank
leaf tok holo = leafStyled tok (initStyle $ compStyle holo) holo

static ∷ (Holo a, RGLFW t m)
  ⇒ a
  → m (Dynamic t HoloBlank)
static holo =
  constDyn ∘ flip leaf holo <$> newId

-- liftHolo ∷ ∀ t m a. (Holo a, ReflexGLFWCtx t m) ⇒ Dynamic t a → WidgetM t m HoloBlank
-- liftHolo h = do
--   tok ← newId
--   pure $ ( constDyn $ subscription (Proxy ∷ Proxy a) tok
--          , h <&> \x→ Holo.leafStyled tok (initStyle $ compStyle x) x)


-- * Pre-package tree constructors
--
vbox, hbox ∷ [Item PBlank] → Item PBlank
-- XXX: here's trouble -- we're using blankIdToken!
hbox = node blankIdToken (initStyle NodeStyle) (HBoxN ∷ Node HBox)
vbox = node blankIdToken (initStyle NodeStyle) (VBoxN ∷ Node VBox)


-- * Leaves

data Rect where
  Rect ∷
    { _rectDim   ∷ Di (Unit PU)
    , _rectColor ∷ Co Double
    } → Rect
-- makeLenses ''Rect

instance DefStyleOf (StyleOf Rect) where
  defStyleOf                = RectStyle $ Rect zero white
instance Holo   Rect where
  data StyleOf  Rect where RectStyle  ∷ Rect → StyleOf Rect
  data VisualOf Rect where RectVisual ∷ { rectDrawable ∷ Drawable } → VisualOf Rect
  query port (RectStyle Rect{..}) _ _ = pure $ Just ∘ fromPU ∘ fromUnit (portDΠ port) <$> _rectDim
  compStyle                 = RectStyle
  hasVisual               _ = True
  createVisual port@Port{..} _ _area drw Rect{..} = do
    let dim = PUs <$> _area^.area'b.size'di
    drawableDrawRect port drw _rectColor dim
    pure $ RectVisual drw
  renderVisual port v@RectVisual{rectDrawable=Drawable{..}} Rect{..} =
    drawableDrawRect port (rectDrawable v) _rectColor _rectDim
  freeVisualOf _ _       = pure ()


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
  compStyle _ = TextStyle
    { _tsFontKey     = "default"
    , _tsSizeSpec    = Cr.TextSizeSpec Nothing Cr.OneLine
    , _tsColor       = white
    }
  subscription _ tok = subSingleton tok editMaskKeys
  liftDyn initial ev =
    (zipperText <$>) <$> foldDyn (\Edit{..} tz → eeEdit tz) (textZipper [initial]) (translateEditEvent <$> ev)
  query port@Port{..} TextStyle{..} _ content = do
    let font = portFont' port _tsFontKey -- XXX: non-total
    (Just ∘ fromPU <$>) ∘ either errorT id <$> Cr.fontQuerySize font (convert (portDΠ port) _tsSizeSpec) (partial (≢ "") content)
  hasVisual _ = True
  createVisual port tStyle@TextStyle{..} area' tDrawable _content = do
    -- 1. find font, 2. bind font to GIC, 3. create layout
    -- Q: why not also draw here?  Reflow?
    let font = portFont' port _tsFontKey -- XXX: non-total
        tDim = fromUnit (portDΠ port) ∘ PUs <$> dimOf area'
    -- liftIO $ putStrLn $ printf "createVisual T.Text: %s → %s" (show _tsFontKey) (show font)
    (,) tFont tLayout ← drawableBindFontLayout (portDΠ port) tDrawable font tDim _tsSizeSpec
    -- drawableBindFontLayout allocates:
    --   GIPC.createContext gic  -- released by FFI finalizers
    --   GIP.layoutNew      gipc -- same as above
    pure $ Text{..}
  renderVisual _ Text{..} text =
    -- 1. execute GIP draw & GIPC composition
    drawableDrawText tDrawable tLayout (_tsColor tStyle) text
  freeVisualOf Text{..} _ =
    Cr.unbindFontLayout tFont tLayout

data EditEvent where
  Edit ∷
    { eeEdit ∷ TextZipper Text → TextZipper Text
    } → EditEvent

translateEditEvent ∷ Input → EditEvent
translateEditEvent = \case
  (Input (U (GLFW.EventChar _ c)))                                              → Edit $ T.insertChar c
  (Input (U (GLFW.EventKey  _ GLFW.Key'Enter     _ GLFW.KeyState'Pressed   _))) → Edit $ T.breakLine
  (Input (U (GLFW.EventKey  _ GLFW.Key'Backspace _ GLFW.KeyState'Pressed   _))) → Edit $ T.deletePrevChar
  (Input (U (GLFW.EventKey  _ GLFW.Key'Delete    _ GLFW.KeyState'Pressed   _))) → Edit $ T.deleteChar
  (Input (U (GLFW.EventKey  _ GLFW.Key'Left      _ GLFW.KeyState'Pressed   _))) → Edit $ T.moveLeft
  (Input (U (GLFW.EventKey  _ GLFW.Key'Up        _ GLFW.KeyState'Pressed   _))) → Edit $ T.moveUp
  (Input (U (GLFW.EventKey  _ GLFW.Key'Right     _ GLFW.KeyState'Pressed   _))) → Edit $ T.moveRight
  (Input (U (GLFW.EventKey  _ GLFW.Key'Down      _ GLFW.KeyState'Pressed   _))) → Edit $ T.moveDown
  (Input (U (GLFW.EventKey  _ GLFW.Key'Home      _ GLFW.KeyState'Pressed   _))) → Edit $ T.gotoBOL
  (Input (U (GLFW.EventKey  _ GLFW.Key'End       _ GLFW.KeyState'Pressed   _))) → Edit $ T.gotoEOL
  (Input (U (GLFW.EventKey  _ GLFW.Key'Enter     _ GLFW.KeyState'Repeating _))) → Edit $ T.breakLine
  (Input (U (GLFW.EventKey  _ GLFW.Key'Backspace _ GLFW.KeyState'Repeating _))) → Edit $ T.deletePrevChar
  (Input (U (GLFW.EventKey  _ GLFW.Key'Delete    _ GLFW.KeyState'Repeating _))) → Edit $ T.deleteChar
  (Input (U (GLFW.EventKey  _ GLFW.Key'Left      _ GLFW.KeyState'Repeating _))) → Edit $ T.moveLeft
  (Input (U (GLFW.EventKey  _ GLFW.Key'Up        _ GLFW.KeyState'Repeating _))) → Edit $ T.moveUp
  (Input (U (GLFW.EventKey  _ GLFW.Key'Right     _ GLFW.KeyState'Repeating _))) → Edit $ T.moveRight
  (Input (U (GLFW.EventKey  _ GLFW.Key'Down      _ GLFW.KeyState'Repeating _))) → Edit $ T.moveDown
  (Input (U (GLFW.EventKey  _ GLFW.Key'Home      _ GLFW.KeyState'Repeating _))) → Edit $ T.gotoBOL
  (Input (U (GLFW.EventKey  _ GLFW.Key'End       _ GLFW.KeyState'Repeating _))) → Edit $ T.gotoEOL
  x → error $ "Unexpected event (non-edit): " <> show x

tsFontKey   ∷ Lens' TextStyle Cr.FontKey
tsFontKey  f ts@(TextStyle x _ _) = (\xx→ts{_tsFontKey=xx})  <$> f x
tsSizeSpec  ∷ Lens' TextStyle (Cr.TextSizeSpec PU)
tsSizeSpec f ts@(TextStyle _ x _) = (\xx→ts{_tsSizeSpec=xx}) <$> f x
tsColor     ∷ Lens' TextStyle (Co Double)
tsColor    f ts@(TextStyle _ _ x) = (\xx→ts{_tsColor=xx})    <$> f x


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

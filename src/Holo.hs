{-# LANGUAGE AllowAmbiguousTypes, ConstraintKinds, InstanceSigs, ExplicitForAll, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RankNTypes, UndecidableInstances #-}
{-# LANGUAGE DataKinds, GADTs, NoMonomorphismRestriction, TypeApplications, TypeFamilies, TypeFamilyDependencies, TypeInType #-}
{-# LANGUAGE LambdaCase, OverloadedLists, OverloadedStrings, PackageImports, PartialTypeSignatures, QuantifiedConstraints, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TupleSections, TypeOperators, ViewPatterns #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors -Wno-orphans -Wno-type-defaults -fconstraint-solver-iterations=0 #-}
module Holo
  (
  -- * Define this:
    Holo(..)
  -- * Get that:
  , StyleOf(..), VisualOf(..)
  , Phase(..)
  , Item, hiToken, hiArea
  , holotreeLeaves
  , queryHoloitem
  , renderHoloitem
  --
  , liftRecord
  --
  , queryHolotree
  , visualiseHolotree
  , renderHolotreeVisuals
  , drawHolotreeVisuals
  -- * Holo instances
  , Rect(..)
  , TextStyle, TextVisual
  , tsFontKey, tsSizeSpec, tsColor
  )
where

import           Control.Arrow
import           Control.Compose
import           Control.Monad
import           Data.Functor.Misc                        (Const2(..))
import           Data.Text                                (Text)
import           Data.Text.Zipper                         (TextZipper)
import           Data.Typeable
import           Generics.SOP.Monadic
import           Linear
import           Prelude                           hiding ((.), id)
import           Reflex                            hiding (Query, Query(..))
import           Reflex.GLFW                              (RGLFW, InputU(..))
import qualified Data.Map.Strict                   as Map
import qualified Data.Text                         as T
import qualified Data.Text.Zipper                  as T
import qualified Generics.SOP                      as SOP
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
--
holotreeLeaves ∷ Item a → Map.Map IdToken (Item a)
holotreeLeaves root = Map.fromList $ walk root
  where walk x@(hiChildren → []) = [(hiToken x, x)]
        walk Item{..}        = concat $ walk <$> hiChildren


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


-- * Lifted records
--
liftRecord ∷ ∀ t m a. (Holo a, RGLFW t m, Record t m a) ⇒ InputMux t → a → m (W t a)
liftRecord eventsV initialV = unO $ recover (Proxy @(RGLFW t m)) (Proxy @t) (eventsV, initialV)

-- instance {-# OVERLAPPABLE #-} (Typeable a, DefStyleOf (StyleOf a)) ⇒ Holo a where
--   type CLiftW t m a = MonadicRecord t m a
--   liftW ∷ (RGLFW t m, CLiftW t m a) ⇒ InputMux t → a → m (W t a)
--   liftW = liftRecord

instance Functor (Derived t) where
  fmap f (W (subs, vals)) = W (subs, (f *** id) <$> vals)

instance Reflex t ⇒ Applicative (Derived t) where
  pure x = W (mempty, constDyn (x, vbox []))
  W (fsubs, fvals) <*> W (xsubs, xvals) =
    W $ (,)
    (zipDynWith (<>) fsubs xsubs)
    (zipDynWith ((\(f,   fhb)
                   (  x, xhb@Item{..})→
                   (f x, xhb { hiChildren = fhb : hiChildren })))
      fvals xvals)

instance {-# OVERLAPPABLE #-} (Holo a, d ~ Derived t, RGLFW t m, MonadicRecord t m u) ⇒ Field t m u a where
  fieldCtx _ (mux, x) proj = (mux, proj x)
  readField _ _ (mux, initV) (FieldName fname) = O $ do
    tok ← liftIO newId
    let package x = hbox [leaf tok defStyle (fname <> ": "), x]
    W ∘ (id *** (<&> (id *** package))) ∘ fromW <$> liftW mux initV

instance ( Monad m, SOP.Generic a, SOP.HasDatatypeInfo a
         , (∀ xs. (SOP.Code a ~ '[xs], SOP.All (Field t m a) xs))
         , RGLFW t m
         ) ⇒ Record t m a where
  type RecordCtx t a = (InputMux t, a)
  prefixChars _ = 3
  consCtx _ _ _ = id
  toFieldName _ = (⊥)
  nameMap       = (⊥)


-- * Leaves
--
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
  subscription tok _ = subSingleton tok editMaskKeys
  liftHoloDyn initial ev =
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

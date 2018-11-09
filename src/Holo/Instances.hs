{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors -Wno-orphans -Wno-type-defaults -fconstraint-solver-iterations=0 #-}
module Holo.Instances
  ( liftRecord
  , Rect(..)
  , TextStyle, TextVisual
  , tsFontKey, tsSizeSpec, tsColor
  , ConsCtx, FieldCtx, Derived(..)
  )
where

import           Control.Arrow
import           Control.Compose
import           Control.Monad
import           Data.Proxy
import           Data.Text                                (Text)
import           Data.Text.Zipper                         (TextZipper)
import           Generics.SOP.Monadic
import           Linear
import           Prelude                           hiding ((.), id)
import           Reflex                            hiding (Query, Query(..))
import           Reflex.GLFW                              (RGLFW, InputU(..))
import qualified Data.Text                         as T
import qualified Data.Text.Zipper                  as T
import qualified Generics.SOP                      as SOP
import qualified GI.Pango                          as GIP
import qualified "GLFW-b" Graphics.UI.GLFW         as GLFW
import qualified Reflex.GLFW                       as GLFW

-- Local imports
import           Elsewhere
import           Flatland
import           HoloPrelude
import           Holo
import           HoloCairo                            (FKind(..))
import qualified HoloCairo                         as Cr
import qualified HoloPort                          as Port


-- * Lifted records
--
instance SOP.Generic         Port.Settings
instance SOP.HasDatatypeInfo Port.Settings

liftRecord ∷ ∀ t m a xs.
  ( RGLFW t m, Record t m a
  , SOP.Code a ~ '[xs]
  , SOP.All (Field t m a) xs
  ) ⇒ InputMux t → a → m (W t a)
liftRecord eventsV initialV = unO $ recover (Proxy @(RGLFW t m)) (Proxy @t) (eventsV, initialV)

-- instance {-# OVERLAPPABLE #-}
--   (Typeable a
--   , DefStyleOf (StyleOf a)
--   , ∀ xs. SOP.Code a ~ '[xs]
--   ) ⇒ Holo a where
--   type CLiftW t m a = ()
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

instance {-# OVERLAPPABLE #-} (Holo a, d ~ Derived t, RGLFW t m) ⇒ Field t m u a where
  fieldCtx _ (mux, x) proj = (mux, proj x)
  readField _ _ (mux, initV) (FieldName fname) = O $ do
    tok ← liftIO Port.newId
    let package x = hbox [leaf tok defStyle (fname <> ": "), x]
    W ∘ (id *** (<&> (id *** package))) ∘ fromW <$> liftW mux initV

instance ( Monad m, SOP.Generic a, SOP.HasDatatypeInfo a
         , RGLFW t m
         ) ⇒ Record t m a where
  type RecordCtx t a = (InputMux t, a)
  prefixChars _ = 3
  consCtx _ _ _ = id
  -- toFieldName _ = (⊥)
  -- nameMap       = (⊥)


-- * Leaves
--
data Rect where
  Rect ∷
    { _rectDim   ∷ Di (Unit PU)
    , _rectColor ∷ Co Double
    } → Rect
-- makeLenses ''Rect

instance Holo   Rect where
  data StyleOf  Rect where RectStyle  ∷ Rect → StyleOf Rect
  data VisualOf Rect where RectVisual ∷ { rectDrawable ∷ Drawable } → VisualOf Rect
  query port (RectStyle Rect{..}) _ _ = pure $ Just ∘ fromPU ∘ fromUnit (Port.portDΠ port) <$> _rectDim
  defStyleOf _              = RectStyle $ Rect zero white
  compStyleOf               = RectStyle
  hasVisual               _ = True
  createVisual port _ _area drw Rect{..} = do
    let dim = PUs <$> _area^.area'b.size'di
    Port.drawableDrawRect port drw _rectColor dim
    pure $ RectVisual drw
  renderVisual port v@RectVisual{rectDrawable=Drawable{..}} Rect{..} =
    Port.drawableDrawRect port (rectDrawable v) _rectColor _rectDim
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
type TextStyle  = StyleOf  T.Text
type TextVisual = VisualOf T.Text
instance Holo  T.Text where
  data StyleOf T.Text where
    TextStyle ∷
      { _tsFontKey     ∷ Cr.FontKey
      , _tsSizeSpec    ∷ Cr.TextSizeSpec PU
      , _tsColor       ∷ Co Double
      } → TextStyle
  defStyleOf _ = TextStyle
    { _tsFontKey     = "default"
    , _tsSizeSpec    = Cr.TextSizeSpec Nothing Cr.OneLine
    , _tsColor       = white
    }
  data VisualOf T.Text where
    Text ∷
      { tStyle         ∷ TextStyle
      , tDrawable      ∷ Drawable
      , tFont          ∷ Cr.WFont Bound
      , tLayout        ∷ GIP.Layout
      , tDim           ∷ Di (Unit u)
      } → TextVisual
  compStyleOf _ = TextStyle
    { _tsFontKey     = "default"
    , _tsSizeSpec    = Cr.TextSizeSpec Nothing Cr.OneLine
    , _tsColor       = white
    }
  subscription tok _ = subSingleton tok editMaskKeys
  liftHoloDyn initial ev =
    (zipperText <$>) <$> foldDyn (\Edit{..} tz → eeEdit tz) (textZipper [initial]) (translateEditEvent <$> ev)
  query port TextStyle{..} _ content = do
    let font = Port.portFont' port _tsFontKey -- XXX: non-total
    (Just ∘ fromPU <$>) ∘ either errorT id <$> Cr.fontQuerySize font (convert (Port.portDΠ port) _tsSizeSpec) (partial (≢ "") content)
  hasVisual _ = True
  createVisual port tStyle@TextStyle{..} area' tDrawable _content = do
    -- 1. find font, 2. bind font to GIC, 3. create layout
    -- Q: why not also draw here?  Reflow?
    let font = Port.portFont' port _tsFontKey -- XXX: non-total
        tDim = fromUnit (Port.portDΠ port) ∘ PUs <$> dimOf area'
    -- liftIO $ putStrLn $ printf "createVisual T.Text: %s → %s" (show _tsFontKey) (show font)
    (,) tFont tLayout ← Port.drawableBindFontLayout (Port.portDΠ port) tDrawable font tDim _tsSizeSpec
    -- drawableBindFontLayout allocates:
    --   GIPC.createContext gic  -- released by FFI finalizers
    --   GIP.layoutNew      gipc -- same as above
    pure $ Text{..}
  renderVisual _ Text{..} text =
    -- 1. execute GIP draw & GIPC composition
    Port.drawableDrawText tDrawable tLayout (_tsColor tStyle) text
  freeVisualOf _ Text{..} =
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

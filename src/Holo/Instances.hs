{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
  ( liftWRecord
  , Rect(..)
  , TextStyle, TextVisual
  , tsFontKey, tsSizeSpec, tsColor
  , ConsCtx, FieldCtx
  )
where

import           Control.Arrow
import           Control.Compose
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


-- * Lifted records (depends on Holo Text instance)
--
instance SOP.Generic         Port.Settings
instance SOP.HasDatatypeInfo Port.Settings

liftWRecord ∷ ∀ a t m s xs.
  ( RGLFW t m, Record t m a, s ~ Structure a
  , SOP.Code s ~ '[xs]
  , SOP.All (HasReadField t m a) xs
  ) ⇒ RecordCtx t a → m (Widget t s)
liftWRecord ctxR = unO $ recover (Proxy @(RGLFW t m)) (Proxy @(t, a)) ctxR

-- Design derivation for Holo lifts:
-- 1. P: low-friction definition for structure types
-- 2. P: low-friction definition for structure field types
-- 3. 1+2 ?→ Field definition not proportional to structure and field types
-- 4. …

instance ( RGLFW t m
         , Holo a
         , d ~ Result t
         , ConsCtx t u ~ (InputMux t, Structure u)) ⇒
         HasFieldCtx t m u a where
  type instance FieldCtx t a  = (InputMux t, a)
  fieldCtx _ (mux, x) proj = (mux, proj x)

instance ( RGLFW t m
         , Holo a
         , d ~ Result t
         , ConsCtx t u ~ (InputMux t, Structure u)) ⇒
         HasReadField t m u a where
  readField _ _ (mux, initV) (FieldName fname) = O $ do
    tok ← liftIO Port.newId
    let package x = hbox [leaf tok defStyle (fname <> ": "), x]
    W ∘ (id *** (<&> (id *** package))) ∘ fromW <$> liftW mux initV

-- record lifting for Dynamic initials
type instance ConsCtx  t (Dynamic t a) = Dynamic t a
type instance Structure  (Dynamic _ a) = a


type instance ConsCtx  t (Static t a)  = (InputMux t, a)
type instance Structure  (Static _ a)  = a

instance ( Monad m, SOP.Generic a, SOP.HasDatatypeInfo a
         , RGLFW t m
         ) ⇒ Record t m (Static t a) where
  type RecordCtx t (Static t a) = (InputMux t, a)
  prefixChars _ = 3
  consCtx _ _ _ (mux, a) = (mux, a)

instance ( Monad m, SOP.Generic a, SOP.HasDatatypeInfo a
         , RGLFW t m
         ) ⇒ Record t m (Dynamic t a) where
  type RecordCtx t (Dynamic t a) = Dynamic t a
  prefixChars _ = 3
  consCtx _ _ _ x = x
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
      { tStyle         ∷ TextStyle      -- XXX
      , tDrawable      ∷ Drawable       -- XXX
      , tFont          ∷ Cr.WFont Bound
      , tLayout        ∷ GIP.Layout
      , tDim           ∷ Di (Unit u)    -- XXX
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


-- * Settings
--

instance Holo a ⇒ Holo (Port.ScreenDim a) where
  subscription tok _ = subSingleton tok $ InputMask GLFW.eventMaskFramebufferSize

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
import           FlatDraw
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
         , ConsCtx t u ~ (InputEventMux t, Structure u)) ⇒
         HasFieldCtx t m u a where
  type instance FieldCtx t a  = (InputEventMux t, a)
  fieldCtx _ (mux, x) proj = (mux, proj x)

instance ( RGLFW t m
         , Holo a
         , d ~ Result t
         , ConsCtx t u ~ (InputEventMux t, Structure u)) ⇒
         HasReadField t m u a where
  readField _ _ (mux, initV) (FieldName fname) = O $ do
    tok ← liftIO Port.newId
    let package x = hbox [leaf tok defStyle (fname <> ": "), x]
    W ∘ (id *** (<&> (id *** package))) ∘ fromW <$> liftW mux initV

-- record lifting for Dynamic initials
type instance ConsCtx  t (Dynamic t a) = Dynamic t a
type instance Structure  (Dynamic _ a) = a


type instance ConsCtx  t (Static t a)  = (InputEventMux t, a)
type instance Structure  (Static _ a)  = a

instance ( Monad m, SOP.Generic a, SOP.HasDatatypeInfo a
         , RGLFW t m
         ) ⇒ Record t m (Static t a) where
  type RecordCtx t (Static t a) = (InputEventMux t, a)
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

type instance StyleOf  Rect = Rect
type instance VisualOf Rect = ()

instance Vis Rect where
  sizeRequest port Rect{..} _ _ = pure $ Just ∘ fromPU ∘ fromUnit (Port.portDΠ port) <$> _rectDim
  defStyleOf _              = Rect zero white
  compStyleOf               = id
  setupVisual Port.Port{portSettings=Port.Settings{..}} _ _area drw@Drawable{..} Rect{..} = do
    let dim = fromUnit sttsDΠ <$> _rectDim
    Cr.runCairo dCairo $ do
      paintRect _rectColor dim -- $ PUs <$> _area^.area'b.size'di
    Port.drawableContentToGPU drw
  render Port.Port{portSettings=Port.Settings{..}} _ _ drw@Drawable{..} Rect{..} = do
    let dim = fromUnit sttsDΠ <$> _rectDim
    Cr.runCairo dCairo $ do
      paintRect _rectColor dim -- $ PUs <$> _area^.area'b.size'di
    Port.drawableContentToGPU drw

instance Holo Rect where
  hasVisual               _ = True


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
data TextStyle where
  TextStyle ∷
    { _tsFontKey     ∷ Cr.FontKey
    , _tsSizeSpec    ∷ Cr.TextSizeSpec PU
    , _tsColor       ∷ Co Double
    } → TextStyle
data TextVisual where
  TextVisual ∷
    { tFont          ∷ Cr.WFont Bound
    , tLayout        ∷ GIP.Layout
    } → TextVisual

type instance StyleOf  T.Text = TextStyle
type instance VisualOf T.Text = TextVisual

instance Vis T.Text where
  defStyleOf _ = TextStyle
    { _tsFontKey     = "default"
    , _tsSizeSpec    = Cr.TextSizeSpec Nothing Cr.OneLine
    , _tsColor       = white
    }
  compStyleOf = const $ defStyleOf $ Proxy @T.Text
  sizeRequest port TextStyle{..} _ content = do
    let font = Port.portFont' port _tsFontKey -- XXX: non-total
    (Just ∘ fromPU <$>) ∘ either errorT id <$> Cr.fontQuerySize font (convert (Port.portDΠ port) _tsSizeSpec) (partial (≢ "") content)
  setupVisual port TextStyle{..} area' drw _content = do
    -- 1. find font, 2. bind font to GIC, 3. create layout
    -- Q: why not also draw here?  Reflow?
    let font = Port.portFont' port _tsFontKey -- XXX: non-total
        tDim = fromUnit (Port.portDΠ port) ∘ PUs <$> dimOf area'
    -- liftIO $ putStrLn $ printf "setupVisual T.Text: %s → %s" (show _tsFontKey) (show font)
    (,) tFont tLayout ← Port.drawableBindFontLayout (Port.portDΠ port) drw font tDim _tsSizeSpec
    -- drawableBindFontLayout allocates:
    --   GIPC.createContext gic  -- released by FFI finalizers
    --   GIP.layoutNew      gipc -- same as above
    pure $ TextVisual{..}
  render _ TextStyle{..} TextVisual{..} drw text =
    -- 1. execute GIP draw & GIPC composition
    Port.drawableDrawText drw tLayout _tsColor text
  freeVisualOf _ TextVisual{..} =
    Cr.unbindFontLayout tFont tLayout

instance Holo T.Text where
  hasVisual _ = True
  subscription tok _ = subSingleton tok editMaskKeys
  liftHoloDyn initial ev =
    (zipperText <$>) <$> foldDyn (\Edit{..} tz → eeEdit tz) (textZipper [initial]) (translateEditEvent <$> ev)

data EditEvent where
  Edit ∷
    { eeEdit ∷ TextZipper Text → TextZipper Text
    } → EditEvent

translateEditEvent ∷ InputEvent → EditEvent
translateEditEvent = \case
  (InputEvent (U (GLFW.EventChar _ c)))                                              → Edit $ T.insertChar c
  (InputEvent (U (GLFW.EventKey  _ GLFW.Key'Enter     _ GLFW.KeyState'Pressed   _))) → Edit $ T.breakLine
  (InputEvent (U (GLFW.EventKey  _ GLFW.Key'Backspace _ GLFW.KeyState'Pressed   _))) → Edit $ T.deletePrevChar
  (InputEvent (U (GLFW.EventKey  _ GLFW.Key'Delete    _ GLFW.KeyState'Pressed   _))) → Edit $ T.deleteChar
  (InputEvent (U (GLFW.EventKey  _ GLFW.Key'Left      _ GLFW.KeyState'Pressed   _))) → Edit $ T.moveLeft
  (InputEvent (U (GLFW.EventKey  _ GLFW.Key'Up        _ GLFW.KeyState'Pressed   _))) → Edit $ T.moveUp
  (InputEvent (U (GLFW.EventKey  _ GLFW.Key'Right     _ GLFW.KeyState'Pressed   _))) → Edit $ T.moveRight
  (InputEvent (U (GLFW.EventKey  _ GLFW.Key'Down      _ GLFW.KeyState'Pressed   _))) → Edit $ T.moveDown
  (InputEvent (U (GLFW.EventKey  _ GLFW.Key'Home      _ GLFW.KeyState'Pressed   _))) → Edit $ T.gotoBOL
  (InputEvent (U (GLFW.EventKey  _ GLFW.Key'End       _ GLFW.KeyState'Pressed   _))) → Edit $ T.gotoEOL
  (InputEvent (U (GLFW.EventKey  _ GLFW.Key'Enter     _ GLFW.KeyState'Repeating _))) → Edit $ T.breakLine
  (InputEvent (U (GLFW.EventKey  _ GLFW.Key'Backspace _ GLFW.KeyState'Repeating _))) → Edit $ T.deletePrevChar
  (InputEvent (U (GLFW.EventKey  _ GLFW.Key'Delete    _ GLFW.KeyState'Repeating _))) → Edit $ T.deleteChar
  (InputEvent (U (GLFW.EventKey  _ GLFW.Key'Left      _ GLFW.KeyState'Repeating _))) → Edit $ T.moveLeft
  (InputEvent (U (GLFW.EventKey  _ GLFW.Key'Up        _ GLFW.KeyState'Repeating _))) → Edit $ T.moveUp
  (InputEvent (U (GLFW.EventKey  _ GLFW.Key'Right     _ GLFW.KeyState'Repeating _))) → Edit $ T.moveRight
  (InputEvent (U (GLFW.EventKey  _ GLFW.Key'Down      _ GLFW.KeyState'Repeating _))) → Edit $ T.moveDown
  (InputEvent (U (GLFW.EventKey  _ GLFW.Key'Home      _ GLFW.KeyState'Repeating _))) → Edit $ T.gotoBOL
  (InputEvent (U (GLFW.EventKey  _ GLFW.Key'End       _ GLFW.KeyState'Repeating _))) → Edit $ T.gotoEOL
  x → error $ "Unexpected event (non-edit): " <> show x

tsFontKey   ∷ Lens' TextStyle Cr.FontKey
tsFontKey  f ts@(TextStyle x _ _) = (\xx→ts{_tsFontKey=xx})  <$> f x
tsSizeSpec  ∷ Lens' TextStyle (Cr.TextSizeSpec PU)
tsSizeSpec f ts@(TextStyle _ x _) = (\xx→ts{_tsSizeSpec=xx}) <$> f x
tsColor     ∷ Lens' TextStyle (Co Double)
tsColor    f ts@(TextStyle _ _ x) = (\xx→ts{_tsColor=xx})    <$> f x


-- * Bool!!!
--
data BoolStyle
  = BoolStyle
  { bsRadius     ∷ Double
  , bsTolerance  ∷ Double
  , bsLineWeight ∷ Double
  , bsInterfocal ∷ Double
  , bsColorOn    ∷ Co Double
  , bsColorOff   ∷ Co Double
  } -- specifying scale as part of style is a smell
--
--  Height: 2*(radius + line-weight + tolerance + line-weight + 0.5*(line-weight + tolerance))
--  Width:  2*(0.5*interfocal + radius + line-weight + tolerance + line-weight + 0.5*(line-weight + tolerance))
--
--     ,------T-----------------L------.
--    /       V  Tolerance              \
--   /   /~~~~~~`\              Line    `\
--  /   /          \            weight    \
--  /  /            \                     \
--  T→ |     o--R--→|  Radius             |
--  \  \            /                     /
--  \   \          /                     /
--   \   \_______/`                     /`
--    \                                /
--     `------------------------------'
--
type instance StyleOf  Bool = BoolStyle
type instance VisualOf Bool = ()

instance Vis Bool where
  defStyleOf _ = BoolStyle
    { bsRadius     = 10
    , bsTolerance  = 2    -- pixel counting → 2
    , bsLineWeight = 2    -- pixel counting → 3
    , bsInterfocal = 12
    , bsColorOn    = green
    , bsColorOff   = gray 0.4 1.0
    }
  sizeRequest _ BoolStyle{..} _ _ =
    let padding = 0.5 * (bsLineWeight + bsTolerance)
        h = 2 * (bsRadius + bsTolerance + bsLineWeight + padding)
        w = h + bsInterfocal
    in pure ∘ (Just <$>) ∘ Di $ V2 w h
  setupVisual _ _ _ _ _ = pure ()
  render _ BoolStyle{..} () d@Drawable{..} val = do
    Cr.runCairo dCairo $ do
      -- paintDebugColorFrames dDi
      let padding  = 0.5 * (bsLineWeight + bsTolerance)
          rrRadius = bsRadius + bsTolerance + bsLineWeight
          hCenter  = rrRadius + padding
      paintRoundedRect (white, if val then bsColorOn else bsColorOff)
        (Th bsLineWeight) (R rrRadius) (Wi bsInterfocal) (Pad padding)
      paintCircle white (R bsRadius) (Po $ V2 (hCenter + if val then 0 else bsInterfocal) hCenter)
    Port.drawableContentToGPU d
-- paintRoundedRect color lw@(Th lineWeight) r@(R radius) (Wi interfocal) (Pad pad) = do

instance Holo Bool where
  hasVisual _ = True
  subscription tok _ = subSingleton tok inputMaskClick1Press
  liftHoloDyn initial ev = foldDyn (\_ v → not v) initial ev


-- * Settings
--
instance Vis  a ⇒ Vis  (Port.ScreenDim a) where
instance Holo a ⇒ Holo (Port.ScreenDim a) where
  subscription tok _ = subSingleton tok $ InputEventMask GLFW.eventMaskFramebufferSize

instance Vis  Port.ScreenMode where
instance Holo Port.ScreenMode where
  subscription tok _ = subSingleton tok editMaskKeys

instance Vis  (Cr.FontPreferences PU) where
instance Holo (Cr.FontPreferences PU) where
  subscription tok _ = subSingleton tok editMaskKeys

instance Vis a  ⇒ Vis  (Di a) where
instance Holo a ⇒ Holo (Di a) where
  subscription tok _ = subSingleton tok editMaskKeys

instance Vis  DΠ where
instance Holo DΠ where
  subscription tok _ = subSingleton tok editMaskKeys

instance Vis  Int where
instance Holo Int where
  subscription tok _ = subSingleton tok editMaskKeys

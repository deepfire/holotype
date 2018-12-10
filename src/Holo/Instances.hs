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
{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors -Wno-orphans -Wno-type-defaults #-}
module Holo.Instances
  ( Rect(..)
  --
  , TextS(..), TextVisual, TextLine(..)
  , tsFontKey, tsSizeSpec, tsColor
  --
  , SwitchS(..), Switch(..)
  --
  , Labelled(..)
  )
where

import           Data.Proxy
import           Data.Text                                (Text)
import           Data.Text.Zipper                         (TextZipper)
import           GHC.Types
import           Linear
import           Prelude                           hiding ((.), id)
import           Reflex                            hiding (Query, Query(..))
import           Reflex.GLFW                              (InputU(..))
import qualified Data.Text.Zipper                  as T
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



-- * Co Double - Rect - Di (Unit PU) - Interp (Di (Unit PU))
--
data Rect = Rect

instance As Rect where
  type Denoted Rect              = Di (Unit PU)
  type Sty     Rect              = Co Double
  defAs                        _ = Rect
  defSty                       _ = blue
  sizeRequest port Rect dim _sty = pure $ (,) () $ Just ∘ fromPU ∘ fromUnit (Port.portDΠ port) <$> dim
  setupVis         _ _ _ _ _ _ _ = pure ()
  render Port.Port{portSettings=Port.Settings{..}} Rect dim' color () shift drw@Drawable{..} () = do
    let dim = fromUnit sttsDΠ <$> dim'
    Cr.runCairo dCairo $ do
      Cr.crMoveTo shift
      paintRect color dim -- $ PUs <$> _area^.area'b.size'di
    Port.drawableContentToGPU drw


-- * TextS - TextVisual - TextLine - Text - Interp Text a
--
data TextS where
  TextS ∷
    { _tsFontKey     ∷ Cr.FontKey
    , _tsSizeSpec    ∷ Cr.TextSizeSpec PU
    , _tsColor       ∷ Co Double
    } → TextS
tsFontKey   ∷ Lens' TextS Cr.FontKey
tsFontKey  f ts@(TextS x _ _) = (\xx→ts{_tsFontKey=xx})  <$> f x
tsSizeSpec  ∷ Lens' TextS (Cr.TextSizeSpec PU)
tsSizeSpec f ts@(TextS _ x _) = (\xx→ts{_tsSizeSpec=xx}) <$> f x
tsColor     ∷ Lens' TextS (Co Double)
tsColor    f ts@(TextS _ _ x) = (\xx→ts{_tsColor=xx})    <$> f x
data TextVisual where
  TextVisual ∷
    { tFont          ∷ Cr.WFont Bound
    , tLayout        ∷ GIP.Layout
    } → TextVisual

data TextLine = TextLine

instance As TextLine where
  type Denoted TextLine = Text
  type Sty     TextLine = TextS
  type Vis     TextLine = TextVisual
  defAs            _ = TextLine
  defSty           _ = TextS
    { _tsFontKey     = "default"
    , _tsSizeSpec    = Cr.TextSizeSpec Nothing Cr.OneLine
    , _tsColor       = white
    }
  sizeRequest port TextLine content TextS{..} = do
    let font = Port.portFont' port _tsFontKey -- XXX: non-total
    (,) () ∘ (Just ∘ fromPU <$>) ∘ either errorT id <$> Cr.fontQuerySize font (convert (Port.portDΠ port) _tsSizeSpec) (partial (≢ "") content)
  setupVis port TextLine _content TextS{..} () area' drw = do
    -- 1. find font, 2. bind font to GIC, 3. create layout
    let font = Port.portFont' port _tsFontKey -- XXX: non-total
        tDim = fromUnit (Port.portDΠ port) ∘ PUs <$> dimOf area'
    -- liftIO $ putStrLn $ printf "setupVisual T.Text: %s → %s" (show _tsFontKey) (show font)
    (,) tFont tLayout ← Port.drawableBindFontLayout (Port.portDΠ port) drw font tDim _tsSizeSpec
    -- drawableBindFontLayout allocates:
    --   GIPC.createContext gic  -- released by FFI finalizers
    --   GIP.layoutNew      gipc -- same as above
    pure $ TextVisual{..}
  render _ TextLine content TextS{..} () shift drw TextVisual{..} =
    -- 1. execute GIP draw & GIPC composition
    Port.drawableDrawText drw tLayout _tsColor shift content
  freeVis _ TextVisual{..} =
    Cr.unbindFontLayout tFont tLayout

instance Mutable Text where
  subscription tok _ = subSingleton tok editMaskKeys
  mutate initial ev =
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


-- * Switch - Bool - Interp Bool x
--
data SwitchS
  = SwitchS
  { ssRadius     ∷ Double
  , ssTolerance  ∷ Double
  , ssLineWeight ∷ Double
  , ssInterfocal ∷ Double
  , ssColorOn    ∷ Co Double
  , ssColorOff   ∷ Co Double
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
--     ```--------------------------'''
--
-- defSty      ∷               Proxy r             → Sty r
-- compSty     ∷                     r             → Sty r
-- sizeRequest ∷ MonadIO m ⇒ VPort → r → Denoted r → Sty r → m (Di (Maybe Double))
-- setupVis    ∷ MonadIO m ⇒ VPort → r → Denoted r → Sty r → Area'LU Double → Drawable → m (Vis r)
-- render      ∷ MonadIO m ⇒ VPort → r → Denoted r → Sty r                  → Drawable → Vis r → m () -- ^ Update visual.
-- freeVis     ∷ MonadIO m ⇒   Proxy r                                                 → Vis r → m ()
data Switch = Switch

instance As Switch where
  type Denoted Switch = Bool
  type Sty     Switch = SwitchS
  defAs   _ = Switch
  defSty  _ = SwitchS
    { ssRadius     = 10
    , ssTolerance  = 2    -- pixel counting → 2
    , ssLineWeight = 2    -- pixel counting → 3
    , ssInterfocal = 12
    , ssColorOn    = green
    , ssColorOff   = gray 0.4 1.0
    }
  sizeRequest _ Switch _ SwitchS{..} =
    let padding = 0.5 * (ssLineWeight + ssTolerance)
        h = 2 * (ssRadius + ssTolerance + ssLineWeight + padding)
        w = h + ssInterfocal
    in pure ∘ (,) () ∘ (Just <$>) ∘ Di $ V2 w h
  setupVis _ _ _ _ _ _ _ = pure ()
  render _ Switch val SwitchS{..} () shift d@Drawable{..} () = do
    let padding  = 0.5 * (ssLineWeight + ssTolerance)
        rrRadius = ssRadius + ssTolerance + ssLineWeight
        hCenter  = rrRadius + padding
    Cr.runCairo dCairo $ do
      -- paintDebugColorFrames dDi
      Cr.crMoveTo shift
      paintRoundedRect (white, if val then ssColorOn else ssColorOff)
        (Th ssLineWeight) (R rrRadius) (Wi ssInterfocal) (Pad padding)
      paintCircle white (R ssRadius) (Po $ V2 (hCenter + if val then 0 else ssInterfocal) hCenter)
    Port.drawableContentToGPU d
-- paintRoundedRect color lw@(Th lineWeight) r@(R radius) (Wi interfocal) (Pad pad) = do

instance Mutable Bool where
  subscription tok _ = subSingleton tok inputMaskClick1Press
  mutate  initial ev = foldDyn (\_ v → not v) initial ev


-- * Text → As → As
--
newtype Labelled a = Labelled (Text, a)

instance As a ⇒ As (Labelled a) where
  type Denoted (Labelled a) = Denoted a
  type Sty     (Labelled a) = (TextS,      Sty a)
  type IStruc  (Labelled a) = (Wi Double, IStruc a)
  type Vis     (Labelled a) = (TextVisual, Vis a)
  defAs                   _ = Labelled ("fi", defAs Proxy)
  defSty                  _ = (defSty $ Proxy @TextLine, defSty $ Proxy @a)
  sizeRequest p (Labelled (label, asA)) content (tSty, aSty) = do
    let effLabel = label <> ": "
    (_, rTL@(Di (V2 w _))) ← ((fromMaybe 0 <$>) <$>) <$> sizeRequest p TextLine effLabel tSty
    (aS, rA)               ← ((fromMaybe 0 <$>) <$>) <$> sizeRequest p asA      content  aSty
    pure $ ((Wi w, aS),) $ Just <$> (_reqt'di $ reqt'add X (Reqt rTL) (Reqt rA))
  setupVis        p (Labelled (label, asA)) content (sT, sA) (Wi cShift, isA) area' drw = do
    let (areaL, areaR) = area'split'start X cShift area'
    let effLabel = label <> ": "
    vT ← setupVis p TextLine                effLabel sT      ()               areaL drw
    vA ← setupVis p asA                     content      sA              isA  areaR drw
    pure $ (,) vT vA
  render          p (Labelled (label, asA)) content (sT, sA) (Wi cShift, isA) shift drw (tV, aV) = do
    let effLabel = label <> ": "
    render        p TextLine   effLabel              sT      ()               shift drw  tV
    render        p asA                     content      sA  isA             (shift & po'd X %~ (+cShift)) drw aV
  freeVis _ (tV, aV) = do
    freeVis (Proxy @TextLine) tV
    freeVis (Proxy @a) aV


-- * Axis → a → b → (,) a b
--
--  XXX:  what could be a meaning that wouldn't involve managing input focus internally?
--        1. two Mutables with non-conflicting subscriptions?
instance (As a, As b) ⇒ As (Axis, (a, b)) where
  type Denoted (Axis, (a, b)) = (Denoted a, Denoted b)
  type Sty     (Axis, (a, b)) = (Sty     a,     Sty b)
  type IStruc  (Axis, (a, b)) = (Double
                                ,(IStruc a,  IStruc b))
  type Vis     (Axis, (a, b)) = (Vis     a,     Vis b)
  defAs                     _ = (X, (defAs Proxy, defAs Proxy))
  defSty                    _ = (defSty $ Proxy @a, defSty $ Proxy @b)
  sizeRequest p (ax, (asA, asB)) (cA, cB) (sA, sB) = do
    (isA, rA@(Di v)) ← ((fromMaybe 0 <$>) <$>) <$> sizeRequest p asA cA sA
    (isB, rB)        ← ((fromMaybe 0 <$>) <$>) <$> sizeRequest p asB cB sB
    pure $ ((v2'proj ax v, (isA, isB)),) $ Just <$> (_reqt'di $ reqt'add ax (Reqt rA) (Reqt rB))
  setupVis    p (ax, (asA, asB)) (cA, cB) (sA, sB) (shift, (isA, isB)) area' drw = do
    let (aLU, aRB) = area'split'start ax shift area'
    vA ← setupVis p asA           cA       sA               isA        aLU   drw
    vB ← setupVis p asB               cB       sB                isB   aRB   drw
    pure $ (,) vA vB
  render      p (ax, (asA, asB)) (cA, cB) (sA, sB) (shiftN, (isA, isB)) shift drw (aV, bV) = do
    render        p asA           cA       sA                isA        shift drw  aV
    render        p asB               cB       sB                 isB  (shift & po'd ax %~ (+shiftN)) drw bV
  freeVis _ (aV, bV) = do
    freeVis (Proxy @a) aV
    freeVis (Proxy @b) bV


-- * Holo
--
instance Holo Bool
instance Holo Double
instance Holo Float
instance Holo Int
instance Holo Integer
instance Holo String
instance Holo Text
instance Holo (Unit PU)

-- So, liftWRecord works for (a, b) now, even if it's assigning empty field names.
-- Next -- shall we:
--   1. rename liftWProduct ← liftWRecord
--   2. make Holo instances for everything liftWProduct-able?

instance Holo a ⇒ Holo (V2 a)

instance Holo (Di (Unit PU)) where


-- * Settings
--

-- * Plan for ScreenDim
-- 1. Need a new As for two fields
-- 2. Co-opt lifted records?
-- 3. If not, what?
-- instance Holo a ⇒ Holo (Port.ScreenDim a) where
--   subscription tok _ = subSingleton tok $ InputEventMask GLFW.eventMaskFramebufferSize

-- instance Holo Port.ScreenMode where
--   subscription tok _ = subSingleton tok editMaskKeys

-- instance Holo (Cr.FontPreferences PU) where
--   subscription tok _ = subSingleton tok editMaskKeys

-- instance Holo a ⇒ Holo (Di a) where
--   subscription tok _ = subSingleton tok editMaskKeys

-- instance Holo DΠ where
--   subscription tok _ = subSingleton tok editMaskKeys

-- instance Holo Int where
--   subscription tok _ = subSingleton tok editMaskKeys

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
  ( liftWRecord
  --
  , Rect(..)
  --
  , TextS(..), TextVisual, TextLine(..)
  , tsFontKey, tsSizeSpec, tsColor
  --
  , SwitchS(..), Switch(..)
  )
where

import           Control.Arrow
import           Control.Compose
import           Data.Proxy
import           Data.Text                                (Text)
import           Data.Text.Zipper                         (TextZipper)
import           Data.Typeable
import           Generics.SOP.Monadic
import           Linear
import           Prelude                           hiding ((.), id)
import           Reflex                            hiding (Query, Query(..))
import           Reflex.GLFW                              (RGLFW, InputU(..))
import qualified Data.Text.Zipper                  as T
import qualified Data.TypeMap.Dynamic              as TM
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

-- * A style map of types to their As instances.
data WXs where
  WXs ∷ (As a, Mutable (Denoted a)) ⇒ a → WXs

data TypeAsTag
type instance TM.Item TypeAsTag a = WXs
newtype TypeAs = AsMap (TM.TypeMap TypeAsTag)

instance ( RGLFW t m
         , Holo a
         , d ~ Result t
         , ConsCtx t u ~ (InputEventMux t, TypeAs, Structure u)) ⇒
         HasFieldCtx t m u a where
  type instance FieldCtx t a  = (InputEventMux t, TypeAs, a)
  fieldCtx _ (mux, tas, x) proj =
    let fP = proxy $ proj x
    in (mux, tas, proj x)

instance ( RGLFW t m
         , Holo a
         , d ~ Result t
         , ConsCtx t u ~ (InputEventMux t, TypeAs, Structure u)) ⇒
         HasReadField t m u a where
  readField _ _ (mux, AsMap tam, initV ∷ a) (FieldName fname) = O $ do
    tok ← liftIO $ Port.newId $ "record label '" <> fname <> "'"
    let addLabel x = hbox [ defLeaf tok TextLine (fname <> ": ")
                          , x
                          ]
        fP = Proxy @a
    case TM.lookup fP tam of
      Nothing      → error $ printf "Record recovery has no As element for field of type %s." (show $ typeRep fP)
      Just (WXs x) →
        W ∘ (id *** (<&> (id *** addLabel))) ∘ fromW <$> liftW mux (defAs $ proxy x) initV
        -- liftW ∷ (As n, Denoted n ~ a, Mutable a, Interp a b, Named a b, Holo b, RGLFW t m)
        -- ⇒ InputEventMux t → n → b → m (Widget t b)

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

instance Holo Text where
  hasVisual _ = True

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

instance Holo Bool where
  hasVisual            _ = True


-- -- * Named tuple
-- --
-- data NamedTuple a b
--   = NamedTuple ((Text, a), (Text, b))


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

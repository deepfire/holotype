{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Weverything #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors -Wno-missing-import-lists -Wno-implicit-prelude -Wno-monomorphism-restriction -Wno-name-shadowing -Wno-all-missed-specialisations -Wno-unsafe -Wno-missing-export-lists -Wno-type-defaults -Wno-partial-fields -Wno-missing-local-signatures -Wno-orphans #-}

module Holo
  ( module HoloVis
  --
  , liftWDynamic, liftWSeed, liftWStatic
  , Frame(..)
  , VPort
  --
  , compToken
  , InputEvent(..)
  , inputMatch
  , InputEventMask(..), Subscription(..), subSingleton
  , inputMaskKeys, inputMaskButtons, inputMaskChars, editMaskKeys
  --
  , Holo(..), hiHasVisual, hiLeaves
  , HoloBlank
  , ensureHolotreeVisuals
  --
  , Structure, Result(..)
  , WH, wWH
  , Widget
  --
  , Static(..)
  --
  , InputEventMux
  )
where

import           Control.Arrow
import           Control.Newtype.Generics
import           Data.Foldable
import           Data.Functor.Misc                        (Const2(..))
import           Data.Maybe
import           Data.Typeable
import           Generics.SOP                             (Proxy)
import           Generics.SOP.Monadic
import           GHC.Types                                (Constraint)
import           Linear
import           Reflex                            hiding (Query, Query(..))
import           Reflex.GLFW                              (RGLFW)
import "GLFW-b"  Graphics.UI.GLFW                  as GL
import qualified Data.Map.Monoidal.Strict          as MMap
import qualified Data.Map.Strict                   as Map
import qualified Data.Sequence                     as Seq
import qualified Data.Set                          as Set
import qualified Reflex.GLFW                       as GLFW

-- Local imports
import           HoloPrelude

import           Flex                                     (Geo)
import qualified Flex                              as Flex

import           Flatland
import           HoloPort                                 (IdToken, Drawable, Frame)
import qualified HoloPort                          as Port
import           HoloVis


class Input iv a where

class (Typeable a, Vis a) ⇒ Holo a where
  -- type CLiftW   t (m ∷ Type → Type) a ∷ Constraint
  hasVisual       ∷                                                               Proxy a → Bool
  liftHoloDyn     ∷ (RGLFW t m) ⇒                                  a → Event t InputEvent → m (Dynamic t a)
  liftHoloItem    ∷                                                           IdToken → a → Item Holo PBlank
  subscription    ∷                                                     IdToken → Proxy a → Subscription
  liftDynW        ∷ (Reflex t)  ⇒                                   IdToken → Dynamic t a → Widget t a
  liftW           ∷ (RGLFW t m) ⇒                                     InputEventMux t → a → m (Widget t a)
  --
  hasVisual       = const False          -- no visual by default
  liftHoloDyn     = liftHoloDynStatic    -- no value change in response to events
  liftHoloItem    = liftItemStatic       -- static style and geometry
  subscription    = const mempty         -- ignore events
  liftDynW        = liftDynWStaticSubs
  liftW           = liftWSeed

compToken ∷ ∀ m a. (Holo a, MonadIO m) ⇒ Proxy a → m IdToken
compToken (hasVisual → True) = Port.newId
compToken _                  = pure Port.blankIdToken


-- * The final lift:  W(-idget)
--
type HoloBlank      = Item Holo PBlank
type WH       t     = (Dynamic t Subscription, Dynamic t HoloBlank)

-- Result of the lifts -- the Widget:
type Widget     t a = Result t a
data instance Result t a
  = Reflex t ⇒ W
    { fromW ∷ (Dynamic t Subscription, Dynamic t (a, HoloBlank))
    }

wWH ∷ Reflex t ⇒ Widget t a → WH t
wWH = (id *** (snd <$>)) ∘ fromW

instance Functor (Result t) where
  fmap f (W (subs, vals)) = W (subs, (f *** id) <$> vals)

instance Reflex t ⇒ Applicative (Result t) where
  pure x = W (mempty, constDyn (x, vbox []))
  W (fsubs, fvals) <*> W (xsubs, xvals) =
    W $ (,)
    (zipDynWith (<>) fsubs xsubs)
    (zipDynWith ((\(f,   fhb)
                   (  x, xhb@Item{..})→
                   (f x, xhb { hiChildren = fhb : hiChildren })))
      fvals xvals)

-- record lifting for unchanging values
-- type instance ConsCtx  t (Static t a)  = (InputEventMux t, a)
-- type instance FieldCtx t (Static t a)  = (InputEventMux t, a)
newtype Static t a = Static a -- XXX: once we're successful with the lift, let's drop the 't'
  deriving newtype (Newtype)

-- instance {-# OVERLAPPABLE #-}
--   (Typeable a
--   , DefStyleOf (StyleOf a)
--   , ∀ xs. SOP.Code a ~ '[xs]
--   ) ⇒ Holo a where
--   type CLiftW t m a = ()
--   liftW ∷ (RGLFW t m, CLiftW t m a) ⇒ InputEventMux t → a → m (W t a)
--   liftW = liftRecord

liftItemStatic ∷ ∀ a. (Holo a) ⇒ IdToken → a → Item Holo PBlank
liftItemStatic tok x = leaf tok (initStyle $ compStyleOf x) x

liftHoloDynStatic ∷ (RGLFW t m) ⇒ a → Event t InputEvent → m (Dynamic t a)
liftHoloDynStatic init _ev = pure $ constDyn init

liftWStatic ∷ ∀ t m a. (Holo a, RGLFW t m) ⇒ InputEventMux t → Static t a → m (Widget t a)
liftWStatic _imux (Static initial) = do
  tok ← compToken $ Proxy @a
  pure $ W ( constDyn $ subscription tok (Proxy @a)
           , constDyn (initial, liftHoloItem tok initial))

liftWSeed   ∷ ∀ t m a. (Holo a, RGLFW t m) ⇒ InputEventMux t → a → m (Widget t a)
liftWSeed imux initial = do
  tok ← compToken $ Proxy @a
  liftDynW tok <$> (liftHoloDyn initial $ select imux $ Const2 tok)

liftDynWStaticSubs ∷ ∀ t a. (Holo a, Reflex t) ⇒ IdToken → Dynamic t a → Widget t a
liftDynWStaticSubs tok valD =
  W ( constDyn $ subscription tok (Proxy @a)
    , valD <&> (id &&& liftHoloItem tok))

liftWDynamic ∷ ∀ a t m. (Holo a, RGLFW t m) ⇒ Dynamic t a → m (Widget t a)
liftWDynamic h = do
  tok ← compToken $ Proxy @a
  pure $ liftDynWStaticSubs tok h


hiHasVisual ∷ Item Holo p → Bool
hiHasVisual x =
  -- XXX: we're losing type safety here..
  case x of Item{holo=_holo ∷ a, ..} → hasVisual (Proxy @a)

hiEnsureVisual ∷ (HasCallStack, c ~ Holo, MonadIO m) ⇒ VPort → Item c PLayout → [Item c PVisual] → m (Item c PVisual)
hiEnsureVisual port hi children = case hi of
  Item{..} →
    if not $ hasVisual (proxy holo)
    then pure $ hiUnvisual    hi children
    else hiMandateVisual port hi children



ensureHolotreeVisuals ∷ (c ~ Holo, MonadIO m) ⇒ VPort → Item c PLayout → m (Item c PVisual)
ensureHolotreeVisuals port hoi@Item{..} =
  hiEnsureVisual port hoi =<< (sequence $ ensureHolotreeVisuals port <$> hiChildren)


-- * InputEvent & Subscription
--
type InputEventMux t     = EventSelector t (Const2 IdToken InputEvent)

data InputEvent where
  InputEvent ∷
    { inInputEvent ∷ GLFW.InputU
    } → InputEvent
  deriving (Show)

data InputEventMask where
  InputEventMask ∷
    { inputMask ∷ GLFW.EventMask
    } → InputEventMask
  deriving (Eq, Ord)
instance Show InputEventMask where
  show InputEventMask{..} = ("(IM "<>) ∘ (<>")") $ show inputMask

inputMatch ∷ InputEventMask → InputEvent → Bool
inputMatch InputEventMask{..} = \case
  InputEvent{inInputEvent=GLFW.U x} → GLFW.eventMatch inputMask x

instance Semigroup InputEventMask where
  InputEventMask a <> InputEventMask b = InputEventMask $ a <> b
instance Monoid InputEventMask where
  mempty = InputEventMask mempty

newtype Subscription = Subscription (MMap.MonoidalMap GLFW.EventType (Seq.Seq (IdToken, InputEventMask)))
instance Show Subscription where
  show (Subscription map) = ("(Subs"<>) ∘ (<>")") $ concat $
    [ " "<>show et<>"::"<> intercalate "+" [ printf "0x%x:%s" tok (show em)
                                           | (Port.tokenHash → tok,(InputEventMask em)) ← toList subs]
    | (et, subs) ← MMap.toList map]

instance Semigroup Subscription where
  Subscription a <> Subscription b = Subscription $ a <> b

deriving newtype instance Monoid Subscription

subSingleton ∷ IdToken → InputEventMask → Subscription
subSingleton tok im@(InputEventMask em) = Subscription $
  MMap.fromList [ (evty, Seq.singleton (tok, im))
                | evty ← GLFW.eventMaskTypes em ]

inputMaskKeys    ∷ Set.Set GL.Key → Set.Set GL.KeyState → GL.ModifierKeys → InputEventMask
-- inputMaskKeys = InputEventMask ∘ GLFW.eventMaskKeys .:: GLFW.KeyEventMask
inputMaskKeys ks kss mks = InputEventMask $ GLFW.eventMaskKeys $ GLFW.KeyEventMask ks kss mks

inputMaskChars   ∷ InputEventMask
inputMaskChars   = InputEventMask $ GLFW.eventMaskChars

inputMaskButtons ∷ GLFW.ButtonEventMask → InputEventMask
inputMaskButtons = InputEventMask ∘ GLFW.eventMaskButtons

editMaskKeys ∷ InputEventMask
editMaskKeys = (inputMaskChars <>) $ InputEventMask $ GLFW.eventMaskKeys $ GLFW.KeyEventMask
  (Set.fromList
   [ GL.Key'Up, GL.Key'Down, GL.Key'Left, GL.Key'Right, GL.Key'Home, GL.Key'End
   , GL.Key'Backspace, GL.Key'Delete, GL.Key'Enter
   ])
  (Set.fromList [GL.KeyState'Pressed, GL.KeyState'Repeating])
  (mempty)



-- * Concrete, minimal case, to keep us in check
--
instance Holo () where

instance Semigroup (Item Holo PBlank)  where _ <> _ = mempty
instance Monoid    (Item Holo PBlank)  where mempty = Item () Proxy Port.blankIdToken (initStyle ()) mempty [] (Di $ V2 Nothing Nothing) mempty ()
instance Semigroup (Item Holo PLayout) where _ <> _ = mempty
instance Monoid    (Item Holo PLayout) where mempty = Item () Proxy Port.blankIdToken (initStyle ()) mempty [] (Di $ V2 Nothing Nothing) mempty ()

deriving anyclass instance Typeable k ⇒ Holo (Node k)

-- instance Semigroup (Item PLayout) where
--   l <> r = vbox [l, r]
-- instance Monoid    (Item PLayout) where
--   mempty      = Item () blankIdToken mempty UnitStyle [] (Di $ V2 Nothing Nothing) mempty ()
-- instance Monoid (Item Visual) where
--   mappend l r = holoVBox [l, r]
--   mempty      = Item () blankIdToken SPU mempty UnitStyle [] (Di $ V2 Nothing Nothing) mempty UnitVisual

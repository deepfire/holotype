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
  ( module AsNameItem
  --
  , liftWDynamic, liftWSeed, liftWStatic
  , Frame(..)
  --
  , compToken
  , InputEvent(..)
  , inputEventType, inputMatch
  , InputEventMask(..), Subscription(..), subSingleton
  , inputMaskKeys, inputMaskButtons, inputMaskChars, editMaskKeys
  , inputMaskClick, inputMaskClick1Press, inputMaskClick1Release
  --
  , Input
  --
  , Holo(..)
  , HoloBlank
  , ensureTreeVisuals
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
import           Data.Typeable
import           Generics.SOP                             (Proxy)
import           Generics.SOP.Monadic
import           Reflex                            hiding (Query, Query(..))
import           Reflex.GLFW                              (RGLFW)
import "GLFW-b"  Graphics.UI.GLFW                  as GL
import qualified Data.Map.Monoidal.Strict          as MMap
import qualified Data.Sequence                     as Seq
import qualified Data.Set                          as Set
import qualified Reflex.GLFW                       as GLFW

-- Local imports
import           HoloPrelude

import           HoloPort                                 (IdToken, Drawable, Frame)
import qualified HoloPort                          as Port
import           AsNameItem


class (As a, Denoted a ~ b) ⇒ Input a b where

class (As (DefaultName a), Denoted (DefaultName a) ~ a, Typeable a) ⇒ Holo a where
  type DefaultName a ∷ Type
  -- type CLiftW   t (m ∷ Type → Type) a ∷ Constraint
  compName     ∷ As n ⇒                              Proxy a → IdToken → n → Name n
  hasVisual    ∷                                                   Proxy a → Bool
  liftHoloDyn  ∷ (RGLFW t m) ⇒                      a → Event t InputEvent → m (Dynamic t a)
  subscription ∷                                         IdToken → Proxy a → Subscription
  liftDynW     ∷ (As n, Denoted n~a, Reflex t) ⇒ IdToken → n → Dynamic t a → Widget t a
  liftW        ∷ (As n, Denoted n~a, RGLFW t m) ⇒  InputEventMux t → n → a → m (Widget t a)
  --
  compName     = defCompName
  hasVisual    = const False          -- no visual by default
  liftHoloDyn  = liftHoloDynStatic    -- no value change in response to events
  subscription = const mempty         -- ignore events
  liftDynW     = liftDynWStaticSubs
  liftW        = liftWSeed

defCompName ∷ As n ⇒ Proxy a → IdToken → n → Name n
defCompName _ tok n = Name tok (initStyle $ compSty n) defGeo n

compToken ∷ ∀ m a. (Holo a, MonadIO m) ⇒ Proxy a → m IdToken
compToken (hasVisual → True) = Port.newId $ showT $ typeRep (Proxy @a)
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
  -- To allow nodes to have unique IdTokens
  -- ← must allow executing newId here
  -- ← work out how to unpack W
  pure x = W (mempty, constDyn (x, vbox []))
  W (fsubs, fvals) <*> W (xsubs, xvals) =
    W $ (,)
    (zipDynWith (<>) fsubs xsubs)
    (zipDynWith ((\(f,   fhb)
                   (  x, xhb)→
                   (f x, xhb & children %~ (fhb :))))
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

liftWStatic ∷ ∀ t m n a. (As n, Denoted n ~ a, Holo a, RGLFW t m) ⇒ InputEventMux t → n → Static t a → m (Widget t a)
liftWStatic _imux n (Static initial) = do
  tok ← compToken $ Proxy @a
  pure $ W ( constDyn $ subscription tok (Proxy @a)
           , constDyn (initial, leaf (compName (Proxy @a) tok n) initial))

liftWDynamic ∷ ∀ t m n a. (As n, Denoted n ~ a, Holo a, RGLFW t m) ⇒ n → Dynamic t a → m (Widget t a)
liftWDynamic n da = do
  tok ← compToken $ Proxy @a
  pure $ liftDynWStaticSubs tok n da


liftHoloDynStatic ∷ (RGLFW t m) ⇒ a → Event t InputEvent → m (Dynamic t a)
liftHoloDynStatic init _ev = pure $ constDyn init

liftDynWStaticSubs ∷ ∀ t n a. (As n, Denoted n ~ a, Holo a, Reflex t) ⇒ IdToken → n → Dynamic t a → Widget t a
liftDynWStaticSubs tok n da =
  let name = compName (Proxy @a) tok n
  in W ( constDyn $ subscription tok (Proxy @a)
       , da <&> (id &&& leaf name))
-- ← liftHoloDynStatic
-- ← liftDynWStaticSubs
liftWSeed   ∷ ∀ t m n a. (As n, Denoted n ~ a, Holo a, RGLFW t m) ⇒ InputEventMux t → n → a → m (Widget t a)
liftWSeed imux n initial = do
  tok ← compToken $ Proxy @a
  liftDynW tok n <$> (liftHoloDyn initial $ select imux $ Const2 tok)
-- → liftW


-- * Treewise ops
--
ensureTreeVisuals ∷ (c ~ Holo, MonadIO m) ⇒ VPort → Item c PLayout → m (Item c PVisual)
ensureTreeVisuals port i = case i of
  Node{..} → iUnvisual i <$> (sequence $ ensureTreeVisuals port <$> denoted)
  Leaf{..} → if not $ hasVisual (proxy denoted)
    then pure $ iUnvisual    i []
    else iMandateVisual port i []


-- * InputEvent & Subscription
--
type InputEventMux t     = EventSelector t (Const2 IdToken InputEvent)

data InputEvent where
  InputEvent ∷
    { inInputEvent       ∷ GLFW.InputU
    } → InputEvent
  ClickEvent ∷
    { inMouseButtonEvent ∷ GLFW.Input GLFW.MouseButton
    , inIdToken          ∷ IdToken
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
  ClickEvent{inMouseButtonEvent=x}  → GLFW.eventMatch inputMask x

inputEventType ∷ InputEvent → GLFW.EventType
inputEventType = \case
  InputEvent{inInputEvent=GLFW.U x} → GLFW.eventType x
  ClickEvent{inMouseButtonEvent=_x} → GLFW.MouseButton


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

inputMaskClick ∷ GL.MouseButton → GL.MouseButtonState → InputEventMask
inputMaskClick btn state = InputEventMask $ GLFW.eventMaskButtons $ GLFW.ButtonEventMask (Set.singleton btn) (Set.singleton state) mempty

inputMaskClick1Press, inputMaskClick1Release ∷ InputEventMask
inputMaskClick1Press   = inputMaskClick GL.MouseButton'1 GL.MouseButtonState'Pressed
inputMaskClick1Release = inputMaskClick GL.MouseButton'1 GL.MouseButtonState'Released

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
  type DefaultName () = ()

instance Semigroup (Item Holo PBlank)  where _ <> _ = mempty
instance Monoid    (Item Holo PBlank)  where mempty = Leaf (Name Port.blankIdToken (initStyle ()) defGeo ()) () diNothing mempty mempty
instance Semigroup (Item Holo PLayout) where _ <> _ = mempty
instance Monoid    (Item Holo PLayout) where mempty = Leaf (Name Port.blankIdToken (initStyle ()) defGeo ()) () diNothing mempty mempty

-- instance (Typeable c, Typeable p) ⇒ Holo (Node c k p) where
--   type DefaultName (Node c k p) = ()

-- instance Semigroup (Item PLayout) where
--   l <> r = vbox [l, r]
-- instance Monoid    (Item PLayout) where
--   mempty      = Item () blankIdToken mempty UnitStyle [] (Di $ V2 Nothing Nothing) mempty ()
-- instance Monoid (Item Visual) where
--   mappend l r = holoVBox [l, r]
--   mempty      = Item () blankIdToken SPU mempty UnitStyle [] (Di $ V2 Nothing Nothing) mempty UnitVisual

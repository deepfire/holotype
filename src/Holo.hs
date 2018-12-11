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
{-# LANGUAGE QuantifiedConstraints #-}
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
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Weverything #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors -Wno-missing-import-lists -Wno-implicit-prelude -Wno-monomorphism-restriction -Wno-name-shadowing -Wno-all-missed-specialisations -Wno-unsafe -Wno-missing-export-lists -Wno-type-defaults -Wno-partial-fields -Wno-missing-local-signatures -Wno-orphans #-}

module Holo
  ( module AsNameItem
  --
  , Mutable(..), immutable
  , InputEvent(..)
  , inputEventType, inputMatch
  , InputEventMask(..)
  , inputMaskKeys, inputMaskButtons, inputMaskChars, editMaskKeys
  , inputMaskClick, inputMaskClick1Press, inputMaskClick1Release
  , Subscription(..), subSingleton
  , InputEventMux
  --
  , BlankHolo, Holo(..), HoloAPI, HoloAPIt, HoloAPIm, HGLFW
  , Blank,     Unconstr
  , liftDynamic
  , liftPureDynamic
  --
  , WH, wWH
  , Widget
  --
  , interpretate
  , liftDynWStaticSubs
  , liftWSeed

  -- * Re-exports
  , Structure, Result(..)
  , Frame(..)
  )
where

import           Control.Arrow
import           Data.Foldable
import           Data.Functor.Misc                        (Const2(..))
import           Data.Typeable
import           Generics.SOP                             (Proxy)
import           Generics.SOP.Monadic
import           GHC.TypeLits
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


-- * Mutability:  Mutable, InputEvent & Subscription
--
class Mutable a where
  subscription ∷                    IdToken → Proxy a → Subscription
  subscription = const mempty         -- ignore events
  mutate       ∷ (RGLFW t m) ⇒ a → Event t InputEvent → m (Dynamic t a)
  mutate       = immutable

immutable ∷ (RGLFW t m) ⇒ a → Event t InputEvent → m (Dynamic t a)
immutable init _ev = pure $ constDyn init

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


-- * Subscription
--
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


class (Typeable b) ⇒ Holo i b where
  liftDynW     ∷ (As n, Denoted n ~ a, Mutable a, Interp a b, Named a b, Holo i b, HGLFW i t m) ⇒ IdToken → n → Dynamic t a → m (Widget i b)
  liftW        ∷ (As n, Denoted n ~ a, Mutable a, Interp a b, Named a b, Holo i b, HGLFW i t m) ⇒   InputEventMux t → n → b → m (Widget i b)
  --
  liftDynW     = liftDynWStaticSubs
  liftW        = liftWSeed


data HoloAPI t m

type family HoloAPIt a ∷ Type where
  HoloAPIt (HoloAPI t _) = t
  HoloAPIt _             = TypeError ('Text "HoloAPIt on non-HoloAPI.")

type family HoloAPIm a ∷ (Type → Type) where
  HoloAPIm (HoloAPI _ m) = m
  HoloAPIm _             = TypeError ('Text "HoloAPIm on non-HoloAPI.")

-- interpretate needed 'Holo a', because:
--   Could not deduce (Holo a) arising from a use of ‘leaf’
--   ..which needs it because it's an Item, which is constrained on Holo.
interpretate ∷ ∀ i t m n a b.
  (As n, Denoted n ~ a,            Interp a b,            HGLFW i t m)
  ⇒ Name n → Dynamic t a → m (Dynamic t (b, Blank i))
interpretate name dyn = scanDynMaybe (fromMaybe (error "Cannot interpret initial value.") *** id)
                        (\x _ -> case x of
                                   (Just val, item) → Just (val, item)
                                   _                → Nothing) $
                        (interp &&& leaf name) <$> dyn

-- liftDynWStaticSubs needed 'Holo a', because:
--   interpretate needed it
liftDynWStaticSubs ∷ ∀ i t m n a b.
  (As n, Denoted n ~ a, Mutable a, Interp a b, Named a b, HGLFW i t m)
  ⇒ IdToken → n → Dynamic t a → m (Widget i b)
liftDynWStaticSubs tok n db = do
  let name ∷ Name n = compName (Proxy @(a, b)) tok n
  int ← interpretate @i name db
  pure $ W ( constDyn $ subscription tok (Proxy @a)
           , int)

type HGLFW i t m = (t ~ (HoloAPIt i), m ~ (HoloAPIm i), RGLFW t m)
-- ← liftHoloDynStatic
-- ← liftDynWStaticSubs
liftWSeed ∷ ∀ i t m n a b.
  (As n, Denoted n ~ a, Mutable a, Interp a b, Named a b, Holo i b, HGLFW i t m)
  ⇒ InputEventMux t → n → b → m (Widget i b)
liftWSeed imux n initial = do
  tok ← iNewToken $ Proxy @b
  mut ← mutate (forget initial) $ select imux $ Const2 tok
  liftDynW tok n mut
-- → liftW

liftDynamic ∷ ∀ i t m n a b.
  (As n, Denoted n ~ a, Mutable a, Interp a b, Named a b, Holo i b, HGLFW i t m)
  ⇒ n → Dynamic t a → m (Widget i b)
liftDynamic n da = do
  tok ← iNewToken $ Proxy @b
  liftDynW tok n da

liftPureDynamic ∷ ∀ i t m n a b.
  (As n, Denoted n ~ a,            Interp a b, Named a b, Holo i b, HGLFW i t m)
  ⇒ n → Dynamic t a → m (Widget i b)
liftPureDynamic n db = do
  tok ← iNewToken $ Proxy @b
  let name ∷ Name n = compName (Proxy @(a, b)) tok n
  int ← interpretate @i name db
  pure $ W ( constDyn mempty
           , int)


-- * The final lift:  W(-idget)
--
class Unconstr a where
instance Unconstr a where

type Blank     i = Item Unconstr PBlank
type BlankHolo i = Item (Holo i) PBlank
type WH        i = (Dynamic (HoloAPIt i) Subscription, Dynamic (HoloAPIt i) (Blank i))

-- Result of the lifts -- the Widget:
type Widget    i a = Result i a
data instance Result i a
  = Reflex (HoloAPIt i) ⇒ W
    { fromW ∷ (Dynamic (HoloAPIt i) Subscription, Dynamic (HoloAPIt i) (a, Blank i))
    }

wWH ∷ Reflex (HoloAPIt i) ⇒ Widget i a → WH i
wWH = (id *** (snd <$>)) ∘ fromW

instance Functor (Result i) where
  fmap f (W (subs, vals)) = W (subs, (f *** id) <$> vals)

instance Reflex (HoloAPIt i) ⇒ Applicative (Result i) where
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

-- instance {-# OVERLAPPABLE #-}
--   (Typeable a
--   , DefStyleOf (StyleOf a)
--   , ∀ xs. SOP.Code a ~ '[xs]
--   ) ⇒ Holo a where
--   type CLiftW t m a = ()
--   liftW ∷ (RGLFW t m, CLiftW t m a) ⇒ InputEventMux t → a → m (W t a)
--   liftW = liftRecord

-- liftWStatic ∷ ∀ t m n a. (As n, Denoted n ~ a, Holo a, RGLFW t m) ⇒ InputEventMux t → n → Static t a → m (Widget t a)
-- liftWStatic _imux n (Static initial) = do
--   tok ← compToken $ Proxy @a
--   pure $ W ( constDyn $ subscription tok (Proxy @a)
--            , constDyn (initial, leaf (compName (Proxy @a) tok n) initial))



-- * Concrete, minimal case, to keep us in check
--
instance Mutable () where
  mutate = immutable

instance Holo i () where

instance Semigroup (Item Unconstr PBlank)  where _ <> _ = mempty
instance Monoid    (Item Unconstr PBlank)  where mempty = Leaf (Name Port.blankIdToken (initStyle ()) defGeo ()) () () diNothing mempty mempty
instance Semigroup (Item Unconstr PLayout) where _ <> _ = mempty
instance Monoid    (Item Unconstr PLayout) where mempty = Leaf (Name Port.blankIdToken (initStyle ()) defGeo ()) () () diNothing mempty mempty

-- instance (Typeable c, Typeable p) ⇒ Holo (Node c k p) where

-- instance Semigroup (Item PLayout) where
--   l <> r = vbox [l, r]
-- instance Monoid    (Item PLayout) where
--   mempty      = Item () blankIdToken mempty UnitStyle [] (Di $ V2 Nothing Nothing) mempty ()
-- instance Monoid (Item Visual) where
--   mappend l r = holoVBox [l, r]
--   mempty      = Item () blankIdToken SPU mempty UnitStyle [] (Di $ V2 Nothing Nothing) mempty UnitVisual

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
  , Named(..), defCompName
  --
  , Holo(..), compToken
  --
  , WH, wWH
  , Widget
  , HoloBlank
  --
  , Static
  , liftWDynamic
  , interpretate
  , liftDynWStaticSubs
  , liftWSeed

  -- * Treewise
  , ensureTreeVisuals

  -- * Re-exports
  , Structure, Result(..)
  , Frame(..)
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


-- * Named, choice of presentation
--
class Named a b where
  compName     ∷ (As n, Denoted n ~ a) ⇒        Proxy (a, b) → IdToken → n → Name n
  compName     = defCompName

defCompName ∷ As n ⇒ Proxy a → IdToken → n → Name n
defCompName _ tok n = Name tok (initStyle $ compSty n) defGeo n


class (Typeable b) ⇒ Holo b where
  hasVisual    ∷                                                                                                        Proxy b → Bool
  liftDynW     ∷ (As n, Denoted n ~ a, Mutable a, Interp a b, Named a b, Holo b, RGLFW t m) ⇒ IdToken → n → Dynamic t a → m (Widget t b)
  liftW        ∷ (As n, Denoted n ~ a, Mutable a, Interp a b, Named a b, Holo b, RGLFW t m) ⇒   InputEventMux t → n → b → m (Widget t b)
  --
  hasVisual    = const False          -- no visual by default
  liftDynW     = liftDynWStaticSubs
  liftW        = liftWSeed

compToken ∷ ∀ m a. (Holo a, MonadIO m) ⇒ Proxy a → m IdToken
compToken (hasVisual → True) = Port.newId $ showT $ typeRep (Proxy @a)
compToken _                  = pure Port.blankIdToken


-- * The final lift:  W(-idget)
--
class Unconstr a where
instance Unconstr a where

type HoloBlank      = Item Unconstr PBlank
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

-- liftWStatic ∷ ∀ t m n a. (As n, Denoted n ~ a, Holo a, RGLFW t m) ⇒ InputEventMux t → n → Static t a → m (Widget t a)
-- liftWStatic _imux n (Static initial) = do
--   tok ← compToken $ Proxy @a
--   pure $ W ( constDyn $ subscription tok (Proxy @a)
--            , constDyn (initial, leaf (compName (Proxy @a) tok n) initial))

liftWDynamic ∷ ∀ m t n a b.
  (As n, Denoted n ~ a, Mutable a, Interp a b, Named a b, Holo b, RGLFW t m)
  ⇒ n → Dynamic t a → m (Widget t b)
liftWDynamic n da = do
  tok ← compToken $ Proxy @b
  liftDynWStaticSubs tok n da


-- interpretate needed 'Holo a', because:
--   Could not deduce (Holo a) arising from a use of ‘leaf’
--   ..which needs it because it's an Item, which is constrained on Holo.
interpretate ∷ (As n, Denoted n ~ a, Interp a b, RGLFW t m) ⇒ Name n → Dynamic t a → m (Dynamic t (b, HoloBlank))
interpretate name dyn = scanDynMaybe (fromMaybe (error "Cannot interpret initial value.") *** id)
                        (\x _ -> case x of
                                   (Just val, item) → Just (val, item)
                                   _                → Nothing) $
                        (interp &&& leaf name) <$> dyn

-- liftDynWStaticSubs needed 'Holo a', because:
--   interpretate needed it
liftDynWStaticSubs ∷ ∀ m t n a b.
  (As n, Denoted n ~ a, Mutable a, Interp a b, Named a b, RGLFW t m)
  ⇒ IdToken → n → Dynamic t a → m (Widget t b)
liftDynWStaticSubs tok n db = do
  let name ∷ Name n = compName (Proxy @(a, b)) tok n
  int ← interpretate name db
  pure $ W ( constDyn $ subscription tok (Proxy @a)
           , int)
-- ← liftHoloDynStatic
-- ← liftDynWStaticSubs
liftWSeed ∷ ∀ m t n a b.
  (As n, Denoted n ~ a, Mutable a, Interp a b, Named a b, Holo b, RGLFW t m)
  ⇒ InputEventMux t → n → b → m (Widget t b)
liftWSeed imux n initial = do
  tok ← compToken $ Proxy @b
  mut ← mutate (forget initial) $ select imux $ Const2 tok
  liftDynW tok n mut
-- → liftW


-- * Treewise ops
--
ensureTreeVisuals ∷ (c ~ Holo, MonadIO m) ⇒ VPort → Item c PLayout → m (Item c PVisual)
ensureTreeVisuals port i = case i of
  Node{..} → iUnvisual i <$> (sequence $ ensureTreeVisuals port <$> denoted)
  Leaf{..} → if not $ hasVisual (proxy denoted)
    then pure $ iUnvisual    i []
    else iMandateVisual port i []



-- * Concrete, minimal case, to keep us in check
--
instance Mutable () where
  mutate = immutable

instance Holo () where

instance Semigroup (Item As PBlank)  where _ <> _ = mempty
instance Monoid    (Item As PBlank)  where mempty = Leaf (Name Port.blankIdToken (initStyle ()) defGeo ()) () diNothing mempty mempty
instance Semigroup (Item As PLayout) where _ <> _ = mempty
instance Monoid    (Item As PLayout) where mempty = Leaf (Name Port.blankIdToken (initStyle ()) defGeo ()) () diNothing mempty mempty

-- instance (Typeable c, Typeable p) ⇒ Holo (Node c k p) where

-- instance Semigroup (Item PLayout) where
--   l <> r = vbox [l, r]
-- instance Monoid    (Item PLayout) where
--   mempty      = Item () blankIdToken mempty UnitStyle [] (Di $ V2 Nothing Nothing) mempty ()
-- instance Monoid (Item Visual) where
--   mappend l r = holoVBox [l, r]
--   mempty      = Item () blankIdToken SPU mempty UnitStyle [] (Di $ V2 Nothing Nothing) mempty UnitVisual

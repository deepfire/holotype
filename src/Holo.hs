{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
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
  , Definition(..), Vocab(..), namely, namely'
  --
  , Interact(..), API, APIt, APIm, HGLFW
  , Widget
  , WH, WF, wSubD, wItemD, wValD, stripW, mapWSubs, mapWItem, mapWVal
  , Blank, BlankWy, Unconstr
  , newDynWidget
  , liftPureDynamic
  --
  , Present(..)
  , interpretate

  -- * Re-exports
  , Result(..)
  , Frame(..)
  )
where

import           Data.Foldable
import           Data.Functor.Misc                        (Const2(..))
import           Data.Typeable
import qualified Data.TypeMap.Dynamic              as TM
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
  subscription = const mempty         -- declare ignorance..
  mutate       ∷ (RGLFW t m) ⇒ a → Event t InputEvent → m (Dynamic t a)
  mutate       = immutable            -- ..then effectuate it

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


-- * Subscription of IdTokens to InputEventMasks
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



-- * The final type class assembly.
--
-- | Vocabulary stores two kinds of entries: interpretation
data Definition i a where
  WName  ∷ (As n, Denoted n ~ a, Mutable a, Named a, Interact i a) ⇒             n → Definition i a
  IName  ∷ (As n, Denoted n ~ a, Mutable a, Named a, Interact i a, Interp a b) ⇒ n → Definition i b
  IWName ∷ (As n, Denoted n ~ a, Mutable a, Named a, Interact i a, Interp a a) ⇒ n → Definition i a

-- | 'Vocab' establishes names for a bunch of types.
newtype Vocab i c = Vocab (TM.TypeMap (HoloTag i))
type instance              TM.Item    (HoloTag i) a = Definition i a
data                                   HoloTag i

-- | Construct a singleton vocabulary easily.
namely  ∷ ∀ b n a i c. (As n, Denoted n ~ a, Mutable a, Named a, Interact i a, Interp a b, Typeable b) ⇒ n → Vocab i c
namely  n = Vocab $ TM.insert (Proxy @a) (WName n)
                  $ TM.insert (Proxy @b) (IName n) TM.empty

namely' ∷ ∀ a n i c. (As n, Denoted n ~ a, Mutable a, Named a, Interact i a, Interp a a) ⇒ n → Vocab i c
namely' n = Vocab $ TM.insert (Proxy @a) (IWName n) TM.empty

vocInteractName ∷ Typeable a ⇒ Proxy a → Vocab i c → Maybe (Definition i a)
vocInteractName p (Vocab tm) = case TM.lookup p tm of
  Nothing           → Nothing
  Just (IName    _) → Nothing
  Just d@(WName  _) → Just d
  Just d@(IWName _) → Just d

vocInterpName ∷ Typeable a ⇒ Proxy a → Vocab i c → Maybe (Definition i a)
vocInterpName  p (Vocab tm) = case TM.lookup p tm of
  Nothing           → Nothing
  Just (WName    _) → Nothing
  Just d@(IName  _) → Just d
  Just d@(IWName _) → Just d


-- | Global context.
type HGLFW i t m = (t ~ (APIt i), m ~ (APIm i), RGLFW t m)

-- | Package types supplying necessary global context (see 'HGLFW') into a single abstract type
--   available for threading.
data API t m

type family APIt a ∷ Type where
  APIt (API t _) = t
  APIt _         = TypeError ('Text "APIt on non-API.")

type family APIm a ∷ (Type → Type) where
  APIm (API _ m) = m
  APIm _         = TypeError ('Text "APIm on non-API.")


-- | Turn values into interactive widgets.
--
class (Typeable a) ⇒ Interact i a where
  dynWidget    ∷ (HGLFW i t m, Typeable a, As n, Denoted n ~ a, Mutable a, Named a, Interact i a)
               ⇒         IdToken → n                 → Dynamic t a → m (Widget i a)
  widget       ∷ (HGLFW i t m, Typeable a, HasCallStack)
               ⇒ InputEventMux t → Vocab i (Present i) →        a → m (Widget i a)
  --
  widget       = newMutatedSeedWidget
  dynWidget    = dynWidgetStaticSubs

newMutatedSeedWidget ∷ ∀ i t m a. (HGLFW i t m, Typeable a, HasCallStack)
          ⇒ InputEventMux t → Vocab i (Present i) → a → m (Widget i a)
newMutatedSeedWidget imux voc initial = case vocInteractName (Proxy @a) voc of
  Nothing        → error $ printf "Lift has no visual name for value of type %s." (show $ typeRep (Proxy @a))
  Just (IName _) → error $ printf "Lift has no visual name for value of type %s." (show $ typeRep (Proxy @a))
  Just (WName n ∷ Definition i a) → do
    tok ← iNewToken $ Proxy @a
    mut ← mutate (forget initial) $ select imux $ Const2 tok
    dynWidget tok n mut
  Just (IWName n ∷ Definition i a) → do
    tok ← iNewToken $ Proxy @a
    mut ← mutate (forget initial) $ select imux $ Const2 tok
    dynWidget tok n mut

dynWidgetStaticSubs ∷ ∀ i t m n a.
  (As n, Denoted n ~ a, Mutable a, Named a, HGLFW i t m)
  ⇒ IdToken → n → Dynamic t a → m (Widget i a)
dynWidgetStaticSubs tok n da = do
    let name ∷ Name n = compName (Proxy @a) tok n
    pure $ W ( constDyn $ subscription tok (Proxy @a)
             , leaf name <$> da
             , da)


-- * Present:  one up from Interact -- with interpretation
--
class (Typeable a) ⇒ Present i a where
  present     ∷ (HGLFW i t m, HasCallStack)
              ⇒ InputEventMux t → Vocab i (Present i) →           a → m (Widget i a)
  present     = presentDef

presentDef ∷ ∀ i a t m
           . (HGLFW i t m, HasCallStack, Typeable a)
           ⇒ InputEventMux t → Vocab i (Present i) →        a → m (Widget i a)
presentDef mux voc seed = case vocInterpName (Proxy @a) voc of
  Nothing        → error $ printf "Lift has no interpretive name for value of type %s." (show $ typeRep (Proxy @a))
  Just (WName _) → error $ printf "Lift has no interpretive name for value of type %s." (show $ typeRep (Proxy @a))
  Just (IName (_ ∷ n) ∷ Definition i a) → do
    W (sD,iD,vD) ← widget @i @(Denoted n) mux voc (forget seed)
    ivD ← interpretate @i vD
    pure $ W (sD,iD,ivD)
  Just (IWName (_ ∷ n) ∷ Definition i a) → do
    W (sD,iD,vD) ← widget @i @(Denoted n) mux voc (forget seed)
    ivD ← interpretate @i vD
    pure $ W (sD,iD,ivD)

-- | Interpretation from the Denoted type and widget creation.
interpretate ∷ ∀ i t m a b.
  (Interp a b, HGLFW i t m, Typeable b)
  ⇒ Dynamic t a → m (Dynamic t b)
interpretate dyn = scanDynMaybe (fromMaybe $ error $ "Cannot interpret initial value into type " <> show (typeRep $ Proxy @b))
                   const
                   (interp <$> dyn)


-- | Full lift pipeline: initial value to a widget.
--
-- ← default: immutable
-- ← default: liftDynWStaticSubs
-- → default: liftW

newDynWidget ∷ ∀ i t m n a.
  (As n, Denoted n ~ a, Mutable a, Named a, Interact i a, HGLFW i t m)
  ⇒ n → Dynamic t a → m (Widget i a)
newDynWidget n da = do
  tok ← iNewToken $ Proxy @a
  dynWidget tok n da

liftPureDynamic ∷ ∀ i t m n a.
  (As n, Denoted n ~ a,            Named a, Interact i a, HGLFW i t m)
  ⇒ n → Dynamic t a → m (Widget i a)
liftPureDynamic n da = do
  tok ← iNewToken $ Proxy @a
  let name ∷ Name n = compName (Proxy @a) tok n
  int ← interpretate @i da
  pure $ W ( constDyn mempty
           , leaf name <$> da
           , int)


-- * The final lift:  W(-idget)
--
class    Unconstr a where
instance Unconstr a where

type Blank   i   = Item Unconstr PBlank
type BlankWy i   = Item (Interact i) PBlank -- XXX: dead code?
type WH      i   = (Dynamic (APIt i) Subscription, Dynamic (APIt i) (Blank i))
type WF      i b = (Dynamic (APIt i) Subscription, Dynamic (APIt i) (Blank i), Dynamic (APIt i) b)

type Widget    i b = Result i b
data instance        Result i b =
  Reflex (APIt i) ⇒ W { fromW ∷ WF i b }

wSubD  ∷ Widget i a → Dynamic (APIt i) Subscription
wSubD  (W (x,_,_)) = x

wItemD ∷ Widget i a → Dynamic (APIt i) (Blank i)
wItemD (W (_,x,_)) = x

wValD  ∷ Widget i a → Dynamic (APIt i) a
wValD  (W (_,_,x)) = x

stripW ∷ Widget i a → WH i
stripW (W (subs, item, _value)) = (subs, item)

mapWSubs ∷ Reflex (APIt i) ⇒ (Subscription → Subscription) → Widget i b → Widget i b
mapWSubs f (W (s,i,v)) = W (f <$> s,i,v)

mapWItem ∷ Reflex (APIt i) ⇒ (Blank i → Blank i) → Widget i b → Widget i b
mapWItem f (W (s,i,v)) = W (s,f <$> i,v)

mapWVal  ∷ Reflex (APIt i) ⇒ (a → b) → Widget i a → Widget i b
mapWVal  f (W (s,i,v)) = W (s,i,f <$> v)

instance Functor (Result i) where
  fmap f (W (subs, item, val)) = W (subs, item, f <$> val)

instance Reflex (APIt i) ⇒ Applicative (Result i) where
  -- To allow nodes to have unique IdTokens
  -- ← must allow executing newId here
  -- ← work out how to unpack W
  pure x = W (mempty, constDyn (vbox []), constDyn x)
  W (fsubs, fitem, fvals) <*> W (xsubs, xitem, xvals) =
    W $ (,,)
    (zipDynWith (<>) fsubs xsubs)
    (zipDynWith ((\fhb xhb→ xhb & children %~ (fhb :)))
                     fitem xitem)
    (zipDynWith ($)  fvals xvals)

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

instance Interact i () where
instance Present  i () where

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

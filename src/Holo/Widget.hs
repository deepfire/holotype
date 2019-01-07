{-# OPTIONS_GHC -Weverything #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors -Wno-missing-import-lists -Wno-implicit-prelude -Wno-monomorphism-restriction -Wno-name-shadowing -Wno-all-missed-specialisations -Wno-unsafe -Wno-missing-export-lists -Wno-type-defaults -Wno-partial-fields -Wno-missing-local-signatures -Wno-orphans #-}
module Holo.Widget
  -- ( module Holo.Classes
  -- , module Holo.Item
  -- --
  -- , immutable
  -- , InputEvent(..)
  -- , inputEventType, inputMatch
  -- , InputEventMask(..)
  -- , inputMaskKeys, inputMaskButtons, inputMaskChars, editMaskKeys
  -- , inputMaskClick, inputMaskClick1Press, inputMaskClick1Release
  -- , Subscription(..), subSingleton
  -- , InputEventMux
  -- --
  -- , Definition(..), Vocab(..)
  -- , ppVocab, traceVocab
  -- , desNDen, desDen
  -- , vocDesig, vocDenot
  -- --
  -- , Widget, dynWidget
  -- , WH, WF, wSubD, wItemD, wValD, stripW, mapWSubs, mapWItem, mapWVal
  -- , Blank
  -- , liftPureDynamic
  -- --
  -- , interpretate

  -- -- * Re-exports
  -- , Result(..)
  -- , Frame(..)
  -- )
where

import           Data.Functor.Misc                        (Const2(..))
import           Data.Typeable
import qualified Data.TypeMap.Dynamic              as TM
import           Generics.SOP                             (Proxy, Top)
import           Generics.SOP.Monadic
import           GHC.TypeLits
import           Reflex                            hiding (Query, Query(..))
import           Reflex.GLFW                              (RGLFW)
import qualified Data.Text                         as T

-- Local imports
import           Graphics.Flex
import {-# SOURCE #-}
                 Holo.Classes
import           Holo.Instances()
import           Holo.Input
import           Holo.Item
import {-# SOURCE #-}
                 Holo.Name
import           Holo.Port
import           Holo.Prelude



-- * The final type class assembly.
--
-- | Vocabulary stores two kinds of entries: interpretation
data Definition (i ∷ Type) (a ∷ Type) where
  Denot      ∷ (Typeable a, As n, Denoted n ~ a, Mutable a, Named a, Widgety i a) ⇒             n → Definition i a
  Desig      ∷ (Typeable a, As n, Denoted n ~ b, Mutable b, Named b, Widgety i b, Interp b a) ⇒ n → Definition i a
  DesigDenot ∷ (Typeable a, As n, Denoted n ~ a, Mutable a, Named a, Widgety i a, Interp a a) ⇒ n → Definition i a

ppDefinition ∷ ∀ (i ∷ Type) t. Proxy t → TM.Item (HoloTag i) t → TM.Item (TM.OfType T.Text) t
ppDefinition _p x = case x of
  Denot (_ ∷ n)      → "Denot "<>showT (typeRep (Proxy @n))<>" → "<>showT (typeRep (Proxy @(Denoted n)))
  Desig (_ ∷ n)      → "Desig "<>showT (typeRep (Proxy @n))<>" → "<>showT (typeRep (Proxy @(Denoted n)))<>" → "<>showT (typeRep (Proxy @t))
  DesigDenot (_ ∷ n) → "DesDe "<>showT (typeRep (Proxy @n))<>" → "<>showT (typeRep (Proxy @(Denoted n)))<>" → "<>showT (typeRep (Proxy @t))

-- | 'Vocab' establishes names for a bunch of types.
newtype Vocab i c = Vocab (TM.TypeMap (HoloTag i))
type instance              TM.Item    (HoloTag i) a = Definition i a
data                                   HoloTag (i ∷ Type)

ppVocab ∷ ∀ (i ∷ Type) c. Vocab i c → T.Text
ppVocab (Vocab (tm ∷ TM.TypeMap (HoloTag i))) = T.intercalate "\n" $
  TM.collapse (ppDefinition @i) tm

traceVocab ∷ ∀ (i ∷ Type) c. String → Vocab i c → Vocab i c
traceVocab desc voc = trace (desc <>" "<> T.unpack (ppVocab voc)) voc

-- | Construct a singleton vocabulary easily.
desNDen  ∷ ∀ b n a i c. (As n, Denoted n ~ a, Mutable a, Named a, Widgety i a, Interp a b, Typeable b) ⇒ n → Vocab i c
desNDen  n = Vocab $ TM.insert (Proxy @a) (Denot n)
                   $ TM.insert (Proxy @b) (Desig n) TM.empty

desDen ∷ ∀ a n i c. (As n, Denoted n ~ a, Mutable a, Named a, Widgety i a, Interp a a) ⇒ n → Vocab i c
desDen   n = Vocab $ TM.insert (Proxy @a) (DesigDenot n) TM.empty

-- XXX: consider inlining, since there's no users beyond mapDe*
vocDenot ∷ Typeable a ⇒ Proxy a → Vocab i c → Maybe (Definition i a)
vocDenot p (Vocab tm) = case TM.lookup p tm of
  Nothing               → Nothing
  Just (Desig        _) → Nothing
  Just d@(Denot      _) → Just d
  Just d@(DesigDenot _) → Just d

vocDesig ∷ Typeable a ⇒ Proxy a → Vocab i c → Maybe (Definition i a)
vocDesig  p (Vocab tm) = case TM.lookup p tm of
  Nothing               → Nothing
  Just (Denot        _) → Nothing
  Just d@(Desig      _) → Just d
  Just d@(DesigDenot _) → Just d

vocErr ∷ Typeable a ⇒ Proxy a → Vocab i c → String → String → String → b
vocErr pA v env miss desc = error $ printf "%s has no %s for value of type %s (%s).\n%s" env miss (show $ typeRep pA) desc (ppVocab v)

mapDenot ∷ ∀ i a b. (HasCallStack, Typeable a) ⇒ Vocab i (Present i) → (∀ n. (As n, Denoted n ~ a, Mutable a, Named a, Widgety i a) ⇒ n → b) → b
mapDenot v f =
  let pA = Proxy @a
  in case vocDenot pA v of
       Nothing        → vocErr pA v "Lift" "Denot" "Nothing"
       Just (Desig _) → vocErr pA v "Lift" "Denot" "Just Desig"
       Just (Denot      n) → f n
       Just (DesigDenot n) → f n

mapDesig ∷ ∀ i a b. (HasCallStack, Typeable a) ⇒ Vocab i (Present i) → (∀ n c. (Typeable a, As n, Denoted n ~ c, Mutable c, Named c, Widgety i c, Interp c a) ⇒ n → b) → b
mapDesig v f =
  let pA = Proxy @a
  in case vocDesig pA v of
       Nothing        → vocErr pA v "Lift" "Desig" "Nothing"
       Just (Denot _) → vocErr pA v "Lift" "Desig" "Just Denot"
       Just (Desig      n) → f n
       Just (DesigDenot n) → f n


dynWidget
  ∷ ∀ i t m a
  . (HasCallStack, HGLFW i t m, Named a, Widgety i a)
  ⇒ Vocab i (Present i) → Dynamic t a → m (Widget i a)
dynWidget voc dyn = do
  tok ← iNewToken $ Proxy @a
  dynWidget' tok voc dyn

widgetDef
  ∷ ∀ i t m a
  . (HGLFW i t m, Typeable a, HasCallStack)
  ⇒ InputEventMux t → Vocab i (Present i) → a → m (Widget i a)
widgetDef imux voc initial =
  mapDenot @i @a voc
  \_→ do
    tok ← iNewToken $ Proxy @a
    mut ← mutate (forget initial) $ select imux $ Const2 tok
    dynWidget' tok voc mut

dynWidget'Def ∷ ∀ i t m a.
  (Typeable a, Mutable a, Named a, HGLFW i t m)
  ⇒ IdToken → Vocab i (Present i) → Dynamic t a → m (Widget i a)
dynWidget'Def tok voc da =
  mapDenot @i @a voc
  \n→ let name = compName (Proxy @a) tok n
      in pure $ W ( constDyn $ subscription tok (Proxy @a)
                    , leaf name <$> da
                    , da)

presentDef ∷ ∀ i a t m
           . (HGLFW i t m, HasCallStack, Typeable a)
           ⇒ InputEventMux t → Vocab i (Present i) → a → m (Widget i a)
presentDef mux voc seed =
  mapDesig @i @a voc
  \(_ ∷ n)→ do
    W (sD,iD,vD) ← widget @i @(Denoted n) mux voc (forget seed)
    ivD ← interpretate @i vD
    pure $ W (sD,iD,ivD)

dynPresentDef ∷ ∀ i a t m
           . (HGLFW i t m, HasCallStack, Typeable a)
           ⇒ InputEventMux t → Vocab i (Present i) → Dynamic t a → m (Widget i a)
dynPresentDef _mux voc da =
  mapDesig @i @a voc
  \(n ∷ n)→ do
    tok ← iNewToken $ Proxy @a
    let name = compName (Proxy @(Denoted n)) tok n
    pure $ W ( constDyn $ subscription tok (Proxy @(Denoted n))
             , leaf name ∘ forget <$> da
             , da)

-- | Interpretation from the Denoted type and widget creation.
interpretate ∷ ∀ i t m a b.
  (Interp a b, HGLFW i t m, Typeable b)
  ⇒ Dynamic t a → m (Dynamic t b)
interpretate dyn = scanDynMaybe (fromMaybe $ error $ "Cannot interpret initial value into type " <> show (typeRep $ Proxy @b))
                   const
                   (interp <$> dyn)

liftPureDynamic ∷ ∀ i t m n a.
  (As n, Denoted n ~ a,            Named a, Widgety i a, HGLFW i t m)
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
type HGLFW i t m = (t ~ (APIt i), m ~ (APIm i), RGLFW t m)

data API (t ∷ Type) (m ∷ Type → Type)

type family APIt a ∷ Type where
  APIt (API t _) = t
  APIt _         = TypeError ('Text "APIt on non-API.")

type family APIm a ∷ (Type → Type) where
  APIm (API _ m) = m
  APIm _         = TypeError ('Text "APIm on non-API.")

type Blank   i   = Item Top PBlank
type WH      i   = (Dynamic (APIt i) Subscription, Dynamic (APIt i) (Blank i))
type WF      i b = (Dynamic (APIt i) Subscription, Dynamic (APIt i) (Blank i), Dynamic (APIt i) b)

type Widget  i b = Result i b
data instance      Result i b =
  Reflex (APIt i) ⇒ W { fromW ∷ WF i b }

wSubD  ∷ Widget i a → Dynamic (APIt i) Subscription
wSubD  (W (x,_,_)) = x

wItemD ∷ Widget i a → Dynamic (APIt i) (Blank i)
wItemD (W (_,x,_)) = x

wValD  ∷ Widget i a → Dynamic (APIt i) a
wValD  (W (_,_,x)) = x

stripW ∷ Widget i a → WH i
stripW (W (subs, item, _value)) = (subs, item)

mapWSubs  ∷ Reflex (APIt i) ⇒ (Subscription → Subscription) → Widget i b → Widget i b
mapWSubs  f (W (s,i,v)) = W (f <$> s,i,v)

mapWItem  ∷ Reflex (APIt i) ⇒ (Blank i → Blank i) → Widget i b → Widget i b
mapWItem  f (W (s,i,v)) = W (s,f <$> i,v)

mapWVal   ∷ Reflex (APIt i) ⇒ (a → b) → Widget i a → Widget i b
mapWVal   f (W (s,i,v)) = W (s,i,f <$> v)

traceWVal ∷ (Reflex (APIt i), Show a) ⇒ String → Widget i a → Widget i a
traceWVal m (W (s,i,v)) = W (s,i,traceDyn m v)

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

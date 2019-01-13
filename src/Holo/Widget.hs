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

import           Control.Monad.Reader
import           Control.Monad.Trans.Reader               (ReaderT)
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
import           Tracer
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

mapDenot ∷ ∀ i a b. (Typeable a) ⇒ Vocab i (Present i) → (∀ n. (As n, Denoted n ~ a, Mutable a, Named a, Widgety i a) ⇒ n → b) → b
mapDenot v f =
  let pA = Proxy @a
  in case vocDenot pA v of
       Nothing        → vocErr pA v "Lift" "Denot" "Nothing"
       Just (Desig _) → vocErr pA v "Lift" "Denot" "Just Desig"
       Just (Denot      n) → f n
       Just (DesigDenot n) → f n

mapDesig ∷ ∀ i a b. (Typeable a) ⇒ Vocab i (Present i) → (∀ n c. (Typeable a, As n, Denoted n ~ c, Mutable c, Named c, Widgety i c, Interp c a) ⇒ n → b) → b
mapDesig v f =
  let pA = Proxy @a
  in case vocDesig pA v of
       Nothing        → vocErr pA v "Lift" "Desig" "Nothing"
       Just (Denot _) → vocErr pA v "Lift" "Desig" "Just Denot"
       Just (Desig      n) → f n
       Just (DesigDenot n) → f n


widgetDef
  ∷ ∀ i t r m a
  . (MonadW i t r m, Typeable a, HasCallStack)
  ⇒ AElt → Vocab i (Present i) → a → m (Widget i a)
widgetDef ae voc initial =
  mapDenot @i @a voc
  \_→ do
    tok   ← iNewToken $ Proxy @a
    input ← getInput @i
    mut   ← mutate (Proxy @i) (forget initial) $ select (fromEvMux $ inMux input) $ Const2 tok
    dynWidget' ae tok voc mut

dynWidget'Def ∷ ∀ i t r m a.
  (MonadW i t r m, Typeable a, Mutable a, Named a)
  ⇒ AElt → IdToken → Vocab i (Present i) → Dynamic t a → m (Widget i a)
dynWidget'Def ae tok voc da =
  mapDenot @i @a voc
  \n→ let name = compName (Proxy @a) tok n
      in do
        input ← getInput @i
        lbs   ← getSubLBinds @i ae
        pure $ Widget' ( ae
                       , (subscription tok (Proxy @a) <>) <$> resolveSubs input tok (lbsSubs lbs)
                       , leaf name <$> da
                       , da)

dynWidget
  ∷ ∀ i t r m a
  . (MonadW i t r m, HasCallStack, Named a, Widgety i a)
  ⇒ AElt → Vocab i (Present i) → Dynamic t a → m (Widget i a)
dynWidget ae voc dyn = do
  tok ← iNewToken $ Proxy @a
  dynWidget' ae tok voc dyn

presentDef ∷ ∀ i a t r m
  . (MonadW i t r m, HasCallStack, Typeable a)
  ⇒ AElt → Vocab i (Present i) → a → m (Widget i a)
presentDef ae voc seed =
  mapDesig @i @a voc
  \(_ ∷ n)→ do
    Widget' (_, sD,iD,vD) ← widget @i @(Denoted n) ae voc (forget seed)
    ivD ← interpretate @i vD
    pure $ Widget'
      ( ae
      , sD
      , iD
      , ivD)

dynPresentDef ∷ ∀ i a t r m
  . (MonadW i t r m, Typeable a)
  ⇒ AElt → Vocab i (Present i) → Dynamic t a → m (Widget i a)
dynPresentDef ae voc da =
  mapDesig @i @a voc
  \(n ∷ n)→ do
    tok ← iNewToken $ Proxy @a
    let name = compName (Proxy @(Denoted n)) tok n
    input ← getInput @i
    lbs   ← getSubLBinds @i ae
    pure $ Widget'
      ( ae
      , (subscription tok (Proxy @(Denoted n)) <>) <$> resolveSubs input tok (lbsSubs lbs)
      , leaf name ∘ forget <$> da
      , da)

-- | Interpretation from the Denoted type and widget creation.
interpretate ∷ ∀ i t r m a b.
  (MonadW i t r m, Interp a b, Typeable b)
  ⇒ Dynamic t a → m (Dynamic t b)
interpretate dyn = scanDynMaybe (fromMaybe $ error $ "Cannot interpret initial value into type " <> show (typeRep $ Proxy @b))
                   const
                   (interp <$> dyn)

liftPureDynamic ∷ ∀ i t r m n a.
  (MonadW i t r m, As n, Denoted n ~ a, Named a, Widgety i a)
  ⇒ AElt → n → Dynamic t a → m (Widget i a)
liftPureDynamic ae n da = do
  tok ← iNewToken $ Proxy @a
  let name ∷ Name n = compName (Proxy @a) tok n
  int ← interpretate @i da
  pure $ Widget' ( ae
                 , constDyn mempty
                 , leaf name <$> da
                 , int)


-- * The final lift:  W(-idget)
--
class    ( RGLFW t m
         , HAPI i t r m
         , MonadTrace r m
         , r ~ MonadWCtx t
         , Has (Input t) r
         , Has LBinds r
         ) ⇒
  MonadW i t r m
instance ( RGLFW t m
         , HAPI i t r m
         , MonadTrace r m
         , r ~ MonadWCtx t
         , Has (Input t) r
         , Has LBinds r
         ) ⇒
  MonadW i t r m

type HAPI i t r m = (t ~ APIt i, r ~ APIr i, m ~ APIm i)
data API (t ∷ Type) (r ∷ Type) (m ∷ Type → Type)

type family APIt a ∷ Type where
  APIt (API t _ _) = t
  APIt _           = TypeError ('Text "APIt on non-API.")

type family APIr a ∷ Type where
  APIr (API _ r _) = r
  APIr _           = TypeError ('Text "APIr on non-API.")

type family APIm a ∷ (Type → Type) where
  APIm (API _ _ m) = m
  APIm _           = TypeError ('Text "APIm on non-API.")

type Blank   i   = Item Top PBlank
type WH      i   = (AElt, Dynamic (APIt i) Subscription, Dynamic (APIt i) (Blank i))
type WF      i b = (AElt, Dynamic (APIt i) Subscription, Dynamic (APIt i) (Blank i), Dynamic (APIt i) b)

data instance
     Result  i b = Reflex (APIt i)
                 ⇒ Widget' { fromWidget ∷ WF i b } -- LRR ≡ Lifted Record Result
type Widget  i b = Result i b
newtype
     FinalH  i   = FinalH  { fromFinalW ∷ WH i }

wAElt  ∷ Widget i a → AElt
wAElt  (Widget' (ae,_,_,_)) = ae

wItemD ∷ Widget i a → Dynamic (APIt i) (Blank i)
wItemD (Widget' (_,_,x,_)) = x

wValD  ∷ Widget i a → Dynamic (APIt i) a
wValD  (Widget' (_,_,_,x)) = x

stripW ∷ Widget i a → WH i
stripW (Widget' (ae, subs, item, _value)) = (ae, subs, item)

mapWItem  ∷ Reflex (APIt i) ⇒ (Blank i → Blank i) → Widget i b → Widget i b
mapWItem  f (Widget' (a,s,i,v)) = Widget' (a,s,f <$> i,v)

mapWVal   ∷ Reflex (APIt i) ⇒ (a → b) → Widget i a → Widget i b
mapWVal   f (Widget' (a,s,i,v)) = Widget' (a,s,i,f <$> v)

traceWVal ∷ (Reflex (APIt i), Show a) ⇒ String → Widget i a → Widget i a
traceWVal m (Widget' (a,s,i,v)) = Widget' (a,s,i,traceDyn m v)

instance Functor (Result i) where
  fmap f (Widget' (ae, subs, item, val)) = Widget' (ae, subs, item, f <$> val)

instance Reflex (APIt i) ⇒ Applicative (Result i) where
  -- To allow nodes to have unique IdTokens
  -- ← must allow executing newId here
  -- ← work out how to unpack W
  pure x = Widget' ("", mempty, constDyn (vbox []), constDyn x)
  Widget' (ael, fsubs, fitem, fvals) <*> Widget' (_aer, xsubs, xitem, xvals) =
    Widget' $ (,,,)
    ael
    (zipDynWith (<>) fsubs xsubs)
    (zipDynWith ((\fhb xhb→ xhb & children %~ (fhb :)))
                     fitem xitem)
    (zipDynWith ($)  fvals xvals)


-- * The Widget monad
--
data MonadWCtx t where
  MonadWCtx ∷
    { wcTrace  ∷ !(Trace IO)
    , wcInput  ∷ !(Maybe (Input t))
    , wcLBinds ∷ !(Maybe LBinds)
    } → MonadWCtx t

type MonadWCtxReaderT t m = ReaderT (MonadWCtx t) m

instance Has (Trace IO) (MonadWCtx t) where
  sliceReader = wcTrace

instance Has LBinds (MonadWCtx i) where
  sliceReader = wcLBinds `rcomp` fromMaybe
    (error $ printf "MonadW: LBinds not initialised yet.")

instance Has (Input t) (MonadWCtx t) where
  sliceReader = wcInput `rcomp` fromMaybe
    (error $ printf "MonadW: LBinds not initialised yet.")

getLBinds ∷ ∀ i t r m. MonadW i t r m ⇒ m LBinds
getLBinds = ask <&> sliceReader

getInput  ∷ ∀ i t r m. MonadW i t r m ⇒ m (Input t)
getInput  = ask <&> sliceReader

switchCtxBinds ∷ LBinds → MonadWCtx t → MonadWCtx t
switchCtxBinds lbs (MonadWCtx tr input _) = MonadWCtx tr input (Just lbs)

-- deriving newtype instance Functor        m ⇒ Functor        (WidgetM i m)
-- deriving newtype instance Applicative    m ⇒ Applicative    (WidgetM i m)
-- deriving newtype instance Monad          m ⇒ Monad          (WidgetM i m)
-- deriving newtype instance                    MonadTrans     (WidgetM i)
-- deriving newtype instance MonadIO        m ⇒ MonadIO        (WidgetM i m)
-- deriving newtype instance MonadRef       m ⇒ MonadRef       (WidgetM i m)
-- deriving newtype instance MonadFix       m ⇒ MonadFix       (WidgetM i m)
-- deriving newtype instance HGLFW      i t m ⇒ MonadHold    t (WidgetM i m)
-- deriving newtype instance PostBuild    t m ⇒ PostBuild    t (WidgetM i m)
-- deriving newtype instance PerformEvent t m ⇒ PerformEvent t (WidgetM i m)
-- deriving newtype instance MonadSample  t m ⇒ MonadSample  t (WidgetM i m)
-- deriving newtype instance MonadTrace     m ⇒ MonadReader (Trace (WidgetM i m)) m
-- deriving newtype instance (MonadReader (Trace (WidgetM i m)) (WidgetM i m))
-- deriving newtype instance MonadIO m ⇒ MonadTrace     (WidgetM i m)
runTracing ∷ ∀ t m a. (MonadIO m) ⇒ T.Text → MonadWCtxReaderT t m a → m a
-- runWithTracing ∷ (MonadIO m, MonadTrace (Trace IO) m') ⇒ Text → m' a → m a
runTracing desc act = do
  tr ← liftIO $ mkTrace desc
  runTracing' tr act

-- MonadTrace (MonadWCtx t) (ReaderT (MonadWCtx t) (Performable m))
-- class (MonadIO m, MonadReader r m, Has (Trace IO) r) ⇒ MonadTrace r m
runTracing' ∷ Trace IO → MonadWCtxReaderT t m a → m a
runTracing' tr act = do
  runReaderT act $ MonadWCtx tr Nothing Nothing
{-# INLINE runTracing' #-}

upgradeMonadW ∷ ∀ i t r m a. MonadW i t r m ⇒ AElt → Input t → m a → m a
upgradeMonadW ae input act =
  local (\m→ m
          { wcInput  = Just input
          , wcLBinds = Just $ emptyLBinds ae
          })
  act

runWidgetMLBinds ∷ ∀ i t r m a. (MonadW i t r m) ⇒ LBinds → m a → m a
runWidgetMLBinds lbs act =
  local (switchCtxBinds lbs) act

getSubLBinds ∷ ∀ i t r m. MonadW i t r m ⇒ AElt → m LBinds
getSubLBinds ae = do
  lbs ← getLBinds @i
  liftIO $ putStrLn (descBindsQuery "getSubLBinds" ae lbs)
  pure $ childBinds ae lbs

module Holo.Widget
where

import           Control.Monad.Fix
import           Control.Monad.Ref
import           Control.Monad.Trans
import           Control.Monad.Trans.Reader
import           Data.Kind                                (Type)
import           Data.Typeable                            (Typeable)
import qualified Data.TypeMap.Dynamic              as TM
import           Generics.SOP                             (Top)
import           Generics.SOP.Monadic                     (Result)
import           GHC.Stack
import           GHC.TypeLits
import           Reflex
import           Reflex.GLFW                              (RGLFW)

import           Holo.Input                               (AElt, Input, LBinds, SemSubs, Subscription)
import {-# SOURCE #-}
                 Holo.Item
import           Tracer


type role Definition nominal nominal
data Definition  (i ∷ Type) (a ∷ Type)

newtype Vocab i c = Vocab (TM.TypeMap (HoloTag i))
data                                   HoloTag (i ∷ Type)
type role HoloTag phantom

type Blank   i     = Item Top PBlank

type HGLFW (i ∷ Type) t m   = (t ~ (APIt i), m ~ (APIm i), RGLFW t m)

type role API phantom phantom
data API (t ∷ Type) (m ∷ Type → Type)

type family APIt a ∷ Type where
  APIt (API t _) = t
  APIt _         = TypeError ('Text "APIt on non-API.")

type family APIm a ∷ (Type → Type) where
  APIm (API _ m) = m
  APIm _         = TypeError ('Text "APIm on non-API.")

type WH      i   = (AElt, Dynamic (APIt i) Subscription, Dynamic (APIt i) (Blank i))
type WF      i b = (AElt, Dynamic (APIt i) Subscription, Dynamic (APIt i) (Blank i), Dynamic (APIt i) b)

type Widget  i b = Result i b

data WidgetMCtx i where
  WidgetMCtx ∷
    { wcInput  ∷ !(Input (APIt i))
    , wcLBinds ∷ !LBinds
    } → WidgetMCtx i

newtype (Monad m) ⇒ WidgetM i m a = WidgetM (ReaderT (WidgetMCtx i) m a)

type WM i m a = WidgetM i m a

instance Functor        m ⇒ Functor        (WidgetM i m)
instance Applicative    m ⇒ Applicative    (WidgetM i m)
instance Monad          m ⇒ Monad          (WidgetM i m)
instance                    MonadTrans     (WidgetM i)
instance MonadIO        m ⇒ MonadIO        (WidgetM i m)
instance MonadRef       m ⇒ MonadRef       (WidgetM i m)
instance MonadFix       m ⇒ MonadFix       (WidgetM i m)
instance HGLFW      i t m ⇒ MonadHold    t (WidgetM i m)
instance PostBuild    t m ⇒ PostBuild    t (WidgetM i m)
instance PerformEvent t m ⇒ PerformEvent t (WidgetM i m)
instance MonadSample  t m ⇒ MonadSample  t (WidgetM i m)

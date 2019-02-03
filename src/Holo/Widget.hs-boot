module Holo.Widget
where
import qualified Data.TypeMap.Dynamic              as TM
import           ExternalImports

import           Generics.SOP.Monadic
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

class ( RGLFW t m
      , HAPI i t r m
      , MonadTrace r m
      , r ~ (MonadWCtx t)
      , Has (Input t) r
      , Has LBinds r
      ) ⇒
  MonadW i t r m

type HAPI i t r m = (t ~ APIt i, r ~ APIr i, m ~ APIm i)

type role API phantom phantom phantom
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

type WH      i   = (AElt, Dynamic (APIt i) Subscription, Dynamic (APIt i) (Blank i))
type WF      i b = (AElt, Dynamic (APIt i) Subscription, Dynamic (APIt i) (Blank i), Dynamic (APIt i) b)

type Widget  i b = Result i b

data MonadWCtx t where
  MonadWCtx ∷
    { wcTrace  ∷ !(Trace IO)
    , wcInput  ∷ !(Maybe (Input t))
    , wcLBinds ∷ !(Maybe LBinds)
    } → MonadWCtx t

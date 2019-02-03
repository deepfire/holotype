module Holo.Item
where
import           ExternalImports

import qualified Holo.Port                         as Port
import           Tracer


data Phase
  = PBlank
  | PLayout
  | PVisual

type role Item nominal nominal
data Item (c ∷ Type → Constraint) (p ∷ Phase)

data KNode

type role Node phantom nominal phantom
data Node (c ∷ Type → Constraint) (k ∷ KNode) (p ∷ Phase)


iNewToken ∷ (MonadTrace r m, Typeable a) ⇒ Proxy a → m Port.IdToken

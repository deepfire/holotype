module Holo.Item
where

import           Control.Monad.IO.Class                   (MonadIO)
import           Data.Kind                                (Constraint, Type)
import           Data.Proxy                               (Proxy)
import           Data.Typeable                            (Typeable)

import qualified Holo.Port                         as Port


data Phase
  = PBlank
  | PLayout
  | PVisual

type role Item nominal nominal
data Item (c ∷ Type → Constraint) (p ∷ Phase)

data KNode

type role Node phantom nominal phantom
data Node (c ∷ Type → Constraint) (k ∷ KNode) (p ∷ Phase)


iNewToken ∷ (MonadIO m, Typeable a) ⇒ Proxy a → m Port.IdToken

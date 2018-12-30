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

data Item (c ∷ Type → Constraint) (p ∷ Phase)

data KNode

data Node c (k ∷ KNode) (p ∷ Phase)


iNewToken ∷ (MonadIO m, Typeable a) ⇒ Proxy a → m Port.IdToken

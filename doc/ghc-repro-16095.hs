{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

import Generics.SOP (HasDatatypeInfo)

data family   TF i a :: *
data instance TF i a = R

class C i a where
  method :: TF i a

instance C i () where

instance HasDatatypeInfo a => C i a where
  method = undefined function

function :: C i a => TF i a
function = method

main = undefined

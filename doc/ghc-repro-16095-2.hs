{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Repro
where

import           Generics.SOP

recover :: forall a xs.
           (Code a ~ '[xs], HasDatatypeInfo a)
        => a
recover =
  case datatypeInfo (Proxy @a) :: DatatypeInfo '[xs] of
    Newtype _ _ _ ->
      let sop :: NP [] xs =
            (undefined
              :: forall c xs
              .  All c xs
              => NP [] xs)
      in undefined

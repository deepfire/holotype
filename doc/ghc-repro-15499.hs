{-# LANGUAGE DataKinds, GADTs, KindSignatures #-}
module Holo ()
where

data ADT (p :: Integer) where
  ADT ::
    { a :: a
    , b :: Integer
    } -> ADT p

foo = undefined {b=undefined}

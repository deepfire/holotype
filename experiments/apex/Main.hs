{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Wall -Wno-unused-imports -Wno-unticked-promoted-constructors -Wno-type-defaults -Wno-missing-signatures #-}

module Main where

import           Control.Applicative
import           Control.Applicative.Free
import           Prelude.Unicode
import           Text.Printf              (printf)


data CF a
  = C (CA a)
  | L a
  deriving (Functor)

type CA a = Ap CF a

mkC ∷ CA a → CA a
mkC = liftAp ∘ C
mkL ∷   a → CA a
mkL = liftAp ∘ L

interpIO ∷ CF a → IO a
interpIO (L x)  = pure x
interpIO (C ap) = runAp interpIO ap -- shed a layer of C

interpId ∷ CF a → CF a
interpId (L x)  = L x
interpId (C ap) = C $ runAp (liftAp ∘ interpId) ap

interpX ∷ CF a → [] a
interpX (L x)  = [x]
interpX (C ap) = runAp interpX ap


main ∷ IO ()
main = do

  putStrLn "You are standing at the end of a road before a small brick building."
  putStrLn "Around you is a forest.  A small stream flows out of the building and"
  putStrLn "down a gully."

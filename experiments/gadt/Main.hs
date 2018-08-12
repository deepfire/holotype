{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Wall -Wno-unused-imports -Wno-unticked-promoted-constructors -Wno-type-defaults -Wno-missing-signatures -Wno-partial-type-signatures #-}

module Main where

import           Data.Monoid
import           Prelude.Unicode



data TC a where
  TC ∷ a → TC a
  deriving Show

data GADT a where
  FGADT ∷ Show a ⇒    a → GADT (TC a)
  GADT  ∷ Show a ⇒ TC a → GADT (TC a)
  -- PGADT ∷ Show a ⇒ TC a → GADT a

deriving instance Show (GADT a)

-- instance Functor GADT where
--   fmap f (GADT (TC x)) = GADT $ TC (f x)

-- newtype GADTW a = GADTW (GADT )

x ∷ _
x = GADT $ TC 1


main ∷ IO ()
main = do

  putStrLn "You are standing at the end of a road before a small brick building."
  putStrLn "Around you is a forest.  A small stream flows out of the building and"
  putStrLn "down a gully."
  putStrLn ""
  putStrLn $ "x: " <> show x


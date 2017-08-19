{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Wall -Wno-unused-imports -Wno-unticked-promoted-constructors -Wno-type-defaults -Wno-missing-signatures #-}

module Main where

-- | Applicative configuration DSL.

import           Control.Applicative.Free (Ap, hoistAp, liftAp)
import           Data.Text (Text)
import           Prelude.Unicode

-- | Part of a computation which produces configuration.
data ConfigF :: * -> * -> * where
  Text :: k -> (Text -> a) -> ConfigF k a
  Int :: k -> (Int -> a) -> ConfigF k a
  Optional :: Ap (ConfigF k) a -> ConfigF k (Maybe a)
  Prefix :: k -> Ap (ConfigF k) a -> ConfigF k a

-- | Configuration part which produces text.
text :: k -> Ap (ConfigF k) Text
text = liftAp . flip Text id

-- | Configuration part which produces an integer.
int :: k -> Ap (ConfigF k) Int
int = liftAp . flip Int id

-- | Configuration part which makes configuration optional.
optional :: Ap (ConfigF k) a -> Ap (ConfigF k) (Maybe a)
optional = liftAp . Optional

-- | Configuration part which nests other configuration.
prefix :: k -> Ap (ConfigF k) a -> Ap (ConfigF k) a
prefix = (liftAp .) . Prefix

-- | Change the keys of a configuration computation.
mapKeys :: (k -> l) -> Ap (ConfigF k) a -> Ap (ConfigF l) a
mapKeys f = hoistAp $ \case
  Text k next -> Text (f k) next
  Int k next -> Int (f k) next
  Optional next -> Optional (mapKeys f next)
  Prefix k next -> Prefix (f k) (mapKeys f next)


main ∷ IO ()
main = do

  putStrLn "You are standing at the end of a road before a small brick building."
  putStrLn "Around you is a forest.  A small stream flows out of the building and"
  putStrLn "down a gully."

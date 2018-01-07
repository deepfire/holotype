{-# LANGUAGE NoImplicitPrelude #-}

module Data.TypeMap.Dynamic
  ( TypeMap()
  , Item
  , empty
  , null
  , size
  , insert
  , lookup
  , delete
  , union
  , difference
  , intersection
  , map
  , traverse
  , OfType
  ) where

import Data.TypeMap.Internal.Dynamic

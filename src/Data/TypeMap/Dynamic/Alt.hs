{-# LANGUAGE NoImplicitPrelude #-}

-- | Type-application-based interface.

module Data.TypeMap.Dynamic.Alt
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
  ( TypeMap
  , Item
  , empty
  , null
  , size
  , union
  , difference
  , intersection
  , OfType
  )

import Data.TypeMap.Internal.Dynamic.Alt

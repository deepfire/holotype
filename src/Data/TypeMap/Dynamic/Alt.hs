{-# LANGUAGE NoImplicitPrelude #-}

-- | Type-application-based interface.

module Data.TypeMap.Dynamic.Alt
  ( TypeMap()
  , Item
  , empty
  , null
  , insert
  , lookup
  , map
  , traverse
  , OfType
  ) where

import Data.TypeMap.Internal.Dynamic
  ( TypeMap
  , Item
  , empty
  , null
  , OfType
  )

import Data.TypeMap.Internal.Dynamic.Alt

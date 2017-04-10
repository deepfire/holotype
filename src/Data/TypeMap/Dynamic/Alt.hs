{-# LANGUAGE NoImplicitPrelude #-}

-- | Type-application-based interface.

module Data.TypeMap.Dynamic.Alt
  ( TypeMap()
  , Item
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
  , null
  , OfType
  )

import Data.TypeMap.Internal.Dynamic.Alt

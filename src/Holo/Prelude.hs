{-# OPTIONS_GHC -Wextra #-}
{-# OPTIONS_GHC -Wno-implicit-prelude -Wno-missing-import-lists -Wno-missed-specialisations #-}
{-# OPTIONS_GHC -Wno-unsafe #-}
module Holo.Prelude
  ( module Control.Applicative
  , module Control.Lens
  , module Data.Function
  , module Data.Functor
  , module Data.List
  , module Elsewhere
  , module GHC.Types
  , module Numeric
  , module Prelude.Unicode
  , module Pretty
  , module Tracer
 --
  , Generic
  , HasCallStack
  , IsString
  , MonadIO, liftIO
  --
  , assert
  , doubleToFloat
  , either
  , filterM
  , fromMaybe
  , unless
  , when
  )
where
import           ExternalImports

import           Elsewhere
import           Pretty
import           Tracer

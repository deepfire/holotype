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
  , (<>)
  , assert
  , doubleToFloat
  , either
  , filterM
  , fromMaybe
  , unless
  , when
  )
where

import           Control.Applicative
import           Control.Lens                      hiding (children, As)
import           Control.Monad                            (unless, when, filterM)
import           Control.Monad.IO.Class                   (MonadIO, liftIO)
import           Control.Exception                        (assert)
import           Data.Either                              (either)
import           Data.Function
import           Data.Functor
import           Data.List                         hiding (uncons)
import           Data.Maybe                               (fromMaybe)
import           Data.Monoid                              ((<>))
import           Data.String                              (IsString)
import           Debug.Trace                              (trace)
import           GHC.Generics                             (Generic)
import           GHC.Stack                                (HasCallStack)
import           GHC.Types                         hiding (Constraint, Word)
import           Numeric
import           Numeric.Extra                            (doubleToFloat)
import           Prelude                           hiding ((.), Word, words)
import           Prelude.Unicode
import           Text.Printf                              (printf)

import           Elsewhere
import           Pretty
import           Tracer

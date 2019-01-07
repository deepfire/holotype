{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors #-}
module Holo.System
  (
    Sec(..), getTime
  , gc, gcKBytesUsed
  , unbufferStdout
  )
where

import           Prelude.Unicode

import           Control.Monad.IO.Class                   (MonadIO, liftIO)

import qualified GHC.Stats                         as Sys
import qualified System.Clock                      as Sys
import qualified System.IO                         as Sys
import qualified System.Mem                        as Sys


newtype Sec = Sec { fromSec ∷ Double } deriving (Eq, Fractional, Num, Ord, Real, RealFrac, Show)

timespecToSecs ∷ Sys.TimeSpec → Double
timespecToSecs = (/ 1000000000.0) ∘ fromIntegral ∘ Sys.toNanoSecs

getTime ∷ (MonadIO m) ⇒ m Sec
getTime = liftIO $ Sec ∘ timespecToSecs <$> Sys.getTime Sys.Monotonic


gc ∷ (MonadIO m) ⇒ m ()
gc = liftIO $ Sys.performGC

gcKBytesUsed ∷ (MonadIO m) ⇒ m Integer
gcKBytesUsed = liftIO $ (`div` 1024) ∘ fromIntegral ∘ Sys.gcdetails_mem_in_use_bytes ∘ Sys.gc <$> Sys.getRTSStats


unbufferStdout ∷ (MonadIO m) ⇒ m ()
unbufferStdout = liftIO $ Sys.hSetBuffering Sys.stdout Sys.NoBuffering

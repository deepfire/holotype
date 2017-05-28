-- Usage:
--   ghc -threaded -eventlog -rtsopts -isrc --make HSstress.hs && ./HSstress +RTS -T -ls -N2
--
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Wall -Wno-type-defaults -rtsopts #-}

import           Control.Monad                            (when)
import           Control.Monad.IO.Class                   (MonadIO, liftIO)
import           Data.List
import qualified GHC.Stats                         as Sys
import qualified System.Clock                      as Sys
import qualified System.Environment                as Sys
import qualified System.Mem                        as Sys
import           Text.Printf                              (printf)

import Data.IORef


data Scenario
  = Empty
  | ModifyIORef
  deriving (Eq, Read, Show)

experiment ∷ Scenario → IORef Int → IO ()
experiment scen ref = do
  case scen of
    Empty         → pure ()
    ModifyIORef   → modifyIORef' ref (1+)


--- Mostly boring parts below
---
main ∷ IO ()
main = do
  args ← Sys.getArgs
  let scen = case args of
        []  → ModifyIORef
        x:_ → read x
  timeStart           ← getTime

  ioref ← newIORef 0
  let navg = 10
      loop (iterN, timePre) avgPre preKB = do

        experiment scen ioref

        timePreGC ← getTime
        gc
        new       ← gcKBytesUsed
        timePost  ← getTime
        let dt     = timePost  - timePre
            nonGCt = timePreGC - timePre
            avgPost@(avgVal, _) = avgStep dt avgPre
        when (0 == mod iterN 40) $
          printf " frame  used dFrMem avgFrMem avgFrTime frTimeNonGC   scenario: %s\n" (show scen)
        when (preKB /= new) $
          printf "%5dn %dk %4ddK %5dK/f    %4.2fms      %4.2fms\n"
                 iterN new (new - preKB) (ceiling $ (fromIntegral new / fromIntegral iterN) ∷ Int)
                 (avgVal * 1000) (nonGCt * 1000)
        loop (iterN + 1, timePost) avgPost new
  loop (0 ∷ Integer, timeStart) (0.0, (navg, 0, [])) =<< gcKBytesUsed

timespecToSecs ∷ Sys.TimeSpec → Double
timespecToSecs = (/ 1000000000.0) . fromIntegral . Sys.toNanoSecs

getTime ∷ IO Double
getTime = timespecToSecs <$> Sys.getTime Sys.Monotonic

gc ∷ IO ()
gc = liftIO $ Sys.performGC

gcKBytesUsed ∷ (MonadIO m) ⇒ m Integer
gcKBytesUsed = liftIO $ (`div` 1024) . fromIntegral . Sys.currentBytesUsed <$> Sys.getGCStats

type Avg a = (Int, Int, [a])
avgStep ∷ Fractional a ⇒ a → (a, Avg a) → (a, Avg a)
avgStep x (_, (lim, cur, xs)) =
  let (ncur, nxs) = if cur < lim
                    then (cur + 1, x:xs)
                    else (lim,     x:Data.List.init xs)
  in ((sum nxs) / fromIntegral ncur, (lim, ncur, nxs))

-- Usage:
--   cabal build Holostress && ./dist/build/Holostress/Holostress +RTS -T -RTS
--
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
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Wextra -Wno-unticked-promoted-constructors -Wno-type-defaults #-}

import           HoloPrelude

import qualified Data.List
import qualified Data.Map                          as Map
import qualified Data.Vector                       as V

import           Linear

import qualified GI.GObject.Objects.Object         as GI
import qualified GI.PangoCairo.Functions           as GIPC
import qualified Graphics.Rendering.Cairo          as GRC
import qualified GI.Pango                          as GIP


import qualified Data.Text                         as T
import qualified Data.Text.Zipper                  as T


import           Flatland
import           HoloTypes

import           HoloCube
import           HoloCairo
import qualified HoloCube                          as HC
import qualified HoloOS                            as HOS
import           HoloPort


main ∷ IO ()
main = do
  HOS.unbufferStdout

  -- * Holo
  Settings{..}        ← defaultSettings

  timeStart           ← HOS.fromSec <$> HOS.getTime
  let dim@(Di (V2 w h)) = di 256 256
      navg = 10
      loop (iterN, timePre) avgPre preKB = do
        do
          -- build stack
          crSurf       ← GRC.createImageSurface GRC.FormatARGB32 w h
          cr           ← cairoCreate        crSurf
          gic          ← cairoToGICairo     cr
          gipc         ← GIPC.createContext gic
          -- gipl         ← GIP.layoutNew      gipc
          -- GIP.layoutSetWidth  lay $ fromIntegral w
          -- GIP.layoutSetHeight lay $ fromIntegral h
          -- GIP.layoutSetText   lay "lol" (-1)
          -- free stack
          -- GI.objectUnref gipl
          GI.objectUnref gipc
          -- cairoDestroy cr
          GRC.surfaceFinish crSurf
          pure ()

        --- do stats
        timePreGC ← HOS.fromSec <$> HOS.getTime
        HOS.gc
        new       ← HOS.gcKBytesUsed
        timePost  ← HOS.fromSec <$> HOS.getTime
        let dt     = timePost  - timePre
            nonGCt = timePreGC - timePre
            avgPost@(avgVal, _) = avgStep dt avgPre
        when (0 == mod iterN 40) $
          printf " frame  used dFrMem avgFrMem avgFrTime frTimeNonGCasdsa\n"
        -- when (preKB /= new) $
        printf "%5dn %dk %4ddK %5dK/f    %4.2fms      %4.2fms\n"
                 iterN new (new - preKB) (ceiling $ (fromIntegral new / fromIntegral iterN) ∷ Int)
                 (avgVal ⋅ 1000) (nonGCt ⋅ 1000)
        loop (iterN + 1, timePost) avgPost new
  loop (0 ∷ Integer, timeStart) (0.0, (navg, 0, [])) =<< HOS.gcKBytesUsed

type Avg a = (Int, Int, [a])
avgStep ∷ Fractional a ⇒ a → (a, Avg a) → (a, Avg a)
avgStep x (_, (lim, cur, xs)) =
  let (ncur, nxs) = if cur < lim
                    then (cur + 1, x:xs)
                    else (lim,     x:Data.List.init xs)
  in ((sum nxs) / fromIntegral ncur, (lim, ncur, nxs))

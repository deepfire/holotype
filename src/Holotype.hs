{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Wall -Wno-type-defaults -Wno-unticked-promoted-constructors #-}
module Holotype where

-- Basis
import           HoloPrelude                       hiding ((<>))
import           Prelude                           hiding (id, Word)

-- Generic
import           Control.Monad

-- Reflex
import           Reflex                            hiding (Query, Query(..))
import           Reflex.GLFW

import qualified Data.Text                         as T

-- Local imports
import           Flatland
import           Flex

import           HoloTypes

import qualified Holo
import           HoloCube
import           HoloPort

holotype ∷ ∀ t m. ReflexGLFWGuest t m
holotype win _evCtl _setupE windowFrameE _ = do
  portV            ← portCreate win =<< defaultSettings
  frameE           ← performEvent $ portNextFrame <$> (portV <$ windowFrameE)
  frameNoD         ← count frameE
  let initV         = "0....5...10...15...20...25...30...35...40...45...50...55...60...65...70...75...80...85...90...95..100"
  tokenV           ← newId $ printf "%d: %s" (T.length initV) (T.unpack initV)
  let drawE         = attachPromptlyDyn frameNoD frameE
  _                ← performEvent $ drawE <&>
                     \(frameNo, f@Frame{..}) → do
                       let holo   = Holo.leaf tokenV defStyle initV
                       text ← (flip $ Holo.queryHoloitem portV) [] holo
                       let leaves = Holo.holotreeLeaves tree
                           dim    = (PUs <$>) ∘ join unsafe'di ∘ fromIntegral ∘ max 1 ∘ flip mod 200 $ frameNo
                           tree   = layout (Size $ fromPU <$> dim) text
                       portGarbageCollectVisuals portV leaves
                       tree' ← Holo.visualiseHolotree portV tree
                       Holo.renderHolotreeVisuals portV tree'
                       Holo.drawHolotreeVisuals f tree'

  hold False never

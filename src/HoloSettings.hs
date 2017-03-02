{-# LANGUAGE DataKinds, GADTs #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}
module HoloSettings
where

import qualified System.IO.Unsafe as UN

import Flatland
import HoloFont

defaultFontPreferences ∷ FontPreferences PU
defaultFontPreferences =
  FontPreferences
  [ ("default",     Left $ Alias "defaultMono" )
  -- , ("defaultSans", Right $ [ FontSpec "Aurulent Sans" "Regular" $ FSROutline (PUs 12) ])
  , ("defaultMono", Right $ [ FontSpec "Terminus"      "Regular" $ FSRBitmap  (PUs 12) LT ])
  ]

data Settings u where
  Settings ∷
    { dπ      ∷ DΠ
    , fontmap ∷ FontMap u
    } → Settings u

defaultSettings ∷ IO (Settings PU)
defaultSettings = do
  let dπ = 96
  fontmap ← makeFontMap dπ fmDefault defaultFontPreferences
  pure Settings{..}

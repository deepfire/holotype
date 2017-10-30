{-# LANGUAGE DataKinds, GADTs #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards, StandaloneDeriving #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors #-}
module HoloSettings
where

import           Control.Monad.IO.Class                   (MonadIO)

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
    -- ⋅ XXX/hardcode ⋅
    , defaultWidth ∷ Wi (Unit PU)
    } → Settings u
deriving instance Show (Settings u)

defaultSettings ∷ (MonadIO m) ⇒ m (Settings PU)
defaultSettings = do
  let dπ           = 96
      defaultWidth = 256
  fontmap ← makeFontMap dπ fmDefault defaultFontPreferences
  pure Settings{..}

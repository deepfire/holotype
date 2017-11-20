
{-# LANGUAGE DataKinds, GADTs #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards, StandaloneDeriving #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors #-}
module HoloSettings
where

import           Control.Applicative.Free
import           Control.Monad.IO.Class                   (MonadIO)

import Flatland
import HoloFont


data Settings where
  Settings ∷
    { sttsDΠ              ∷ DΠ
    , sttsFontPreferences ∷ FontPreferences PU
    } → Settings
    deriving (Eq, Show)

defaultSettings ∷ (MonadIO m) ⇒ m Settings
defaultSettings = do
  let sttsDΠ              = 96
      sttsFontPreferences = FontPreferences
        [ ("default",     Left $ Alias "defaultMono" )
        , ("defaultSans", Right $ [ FontSpec "Aurulent Sans" "Regular" $ FSROutline (PUs 12) ])
        , ("defaultMono", Right $ [ FontSpec "Terminus"      "Regular" $ FSRBitmap  (PUs 12) LT ])
        ]
  pure Settings{..}


data System u where
  System ∷
    { sysFontmap ∷ FontMap u
    } → System u

systemSetup ∷ (MonadIO m) ⇒ Settings → m (System PU)
systemSetup Settings{..} = do
  sysFontmap ← makeFontMap sttsDΠ fmDefault sttsFontPreferences
  pure System{..}

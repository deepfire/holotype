module Holo.Instances.Present
where

import           Data.Text                                (Text)

import qualified Graphics.Cairo                    as Cr
import           Graphics.Flatland

import {-# SOURCE #-}
                 Holo.Classes
import qualified Holo.Port                         as Port




instance Present i ()
instance Present i (Unit PU)
instance Present i (Unit PUI)
instance Present i (Unit Pt)
instance Present i Bool
instance Present i Double
instance Present i DΠ
instance Present i Float
instance Present i Int
instance Present i Integer
instance Present i Text
instance Present i a ⇒ Present  i [a]

instance Present i Cr.FaceName
instance Present i Cr.FamilyName
instance Present i Cr.FontAlias
instance Present i Cr.FontKey
instance Present i Port.ScreenMode
instance Present i Port.WaitVSync

module Holo.Instances.Present
where

import Graphics.Flatland

import Holo.Classes


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

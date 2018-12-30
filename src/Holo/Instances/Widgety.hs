module Holo.Instances.Widgety
where

import Graphics.Flatland

import Holo.Classes


instance Widgety i ()
instance Widgety i (Unit PU)
instance Widgety i (Unit PUI)
instance Widgety i (Unit Pt)
instance Widgety i Bool
instance Widgety i Double
instance Widgety i DΠ
instance Widgety i Float
instance Widgety i Int
instance Widgety i Integer
instance Widgety i Text

instance Widgety i (Di (Unit PU))

module Holo.Instances.Widgety
where

import           Data.Text                                (Text)

import           Graphics.Flatland

import {-# SOURCE #-}
                 Holo.Classes
import           Holo.Instances.As
import           Holo.Instances.Mutable
import           Holo.Port


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
instance Widgety i a ⇒ Widgety  i [a]

instance Widgety i (Di (Unit PU))

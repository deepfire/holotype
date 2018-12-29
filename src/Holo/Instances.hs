{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors -Wno-orphans #-}
module Holo.Instances
  ( module Holo.Classes

  , module Holo.Instances.As
  , module Holo.Instances.Mutable
  )
where

import           Holo.Classes
import           Holo.Instances.As
import           Holo.Instances.Mutable

instance Present i Bool
instance Widgety i Bool
instance Present i Double
instance Widgety i Double
instance Present i DΠ
instance Widgety i DΠ
instance Present i Float
instance Widgety i Float
instance Present i Int
instance Widgety i Int
instance Present i Integer
instance Widgety i Integer
instance Present i Text
instance Widgety i Text
instance Present i (Unit PU)
instance Widgety i (Unit PU)
instance Present i (Unit PUI)
instance Widgety i (Unit PUI)
instance Present i (Unit Pt)
instance Widgety i (Unit Pt)

instance Present i a ⇒ Present  i [a]

instance Widgety i (Di (Unit PU))

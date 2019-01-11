module Holo.Instances.Widgety
where

import           Data.Text                                (Text)
import           Data.Typeable                            (Typeable)

import qualified Graphics.Cairo                    as Cr
import           Graphics.Flatland

import {-# SOURCE #-}
                 Holo.Classes
import           Holo.Instances.As
import           Holo.Instances.Mutable
import qualified Holo.Port                         as Port


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

instance Widgety i Cr.FaceName
instance Widgety i Cr.FamilyName
instance Widgety i Cr.FontAlias
instance Widgety i Cr.FontKey
instance Typeable a ⇒
         Widgety i (Port.ScreenDim a)
instance Widgety i Port.ScreenMode
instance Widgety i Port.WaitVSync

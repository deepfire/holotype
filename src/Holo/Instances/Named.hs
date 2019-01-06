module Holo.Instances.Named
where

import           Data.Text                                (Text)

import           Graphics.Flatland

import {-# SOURCE #-}
                 Holo.Classes


instance {-# OVERLAPPABLE #-} Named a
instance {-# OVERLAPPABLE #-} Named Text

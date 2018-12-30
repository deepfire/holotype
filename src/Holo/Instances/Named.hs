module Holo.Instances.Named
where

import Graphics.Flatland

import Holo.Classes


instance {-# OVERLAPPABLE #-} Holo.Named a
instance {-# OVERLAPPABLE #-} Holo.Named Text

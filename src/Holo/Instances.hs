{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors -Wno-orphans #-}
module Holo.Instances
  ( module Holo.Classes

  , module Holo.Instances.As
  , module Holo.Instances.Interp
  , module Holo.Instances.Mutable
  , module Holo.Instances.Named
  , module Holo.Instances.Present
  , module Holo.Instances.Widgety
  )
where

import     Holo.Classes

import     Holo.Instances.As
import     Holo.Instances.Interp
import     Holo.Instances.Mutable
import     Holo.Instances.Named
import     Holo.Instances.Present
import     Holo.Instances.Widgety

{-# LANGUAGE UnicodeSyntax #-}

module Main (main) where

import Control.Wire.Controller (control)
import Holotype                (holotype)

-- | A FRP implementation meets a FRP network.
main ∷ IO ()
main = control holotype

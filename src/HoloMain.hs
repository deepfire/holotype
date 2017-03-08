{-# LANGUAGE UnicodeSyntax #-}

module Main (main) where

import HoloFlex                (host)
import Holotype                (initialiser)

-- | A FRP implementation meets a FRP network.
main ∷ IO ()
main = host initialiser

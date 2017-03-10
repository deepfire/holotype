{-# LANGUAGE UnicodeSyntax #-}

module Main (main) where

import Reflex.GLFW             (simpleHost)
import Holotype                (holotype)

-- | A FRP implementation meets a FRP network.
main ∷ IO ()
main = simpleHost "holotype" holotype

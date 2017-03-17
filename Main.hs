{-# LANGUAGE UnicodeSyntax #-}

module Main (main) where

import Reflex.GLFW             (basicGL33Host)
import Holotype                (holotype)

-- | A FRP implementation meets a FRP network.
main ∷ IO ()
main = basicGL33Host "a simple holotype" holotype

{-# LANGUAGE UnicodeSyntax #-}

module Main (main) where

import Reflex.GLFW             (host, defaultGLWindow)
import Holotype                (initialiser)

-- | A FRP implementation meets a FRP network.
main ∷ IO ()
main = host defaultGLWindow initialiser

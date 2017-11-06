{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-type-defaults #-}

module FlexTest where

import           Control.Lens
import           Data.List
import           Prelude.Unicode

import qualified Hedgehog              as H
import qualified Hedgehog.Gen          as Gen
import qualified Hedgehog.Range        as Range
import           Test.Tasty
import           Test.Tasty.Hspec
import           Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Flatland
import Flex


-- * test_grow
--
unit_grow1 ∷ IO ()
unit_grow1 =
  let r = mkItem 60 240
          [ mkItem 60 30 [] & it'grow .~ 0
          , mkItem 60  0 [] & it'grow .~ 1
          , mkItem 60  0 [] & it'grow .~ 2
          ] & flex_layout
  in do
    frame'of (r^.child 0) @?= frame (po 0   0) (di 60  30)
    frame'of (r^.child 1) @?= frame (po 0  30) (di 60  70)
    frame'of (r^.child 2) @?= frame (po 0 100) (di 60 140)

unit_grow2 ∷ IO ()
unit_grow2 =
  let r = mkItem 100 100
          [ mkItem 100 20 [] & it'grow .~ 1
          , mkItem 100 20 [] & it'grow .~ 0
          , mkItem 100 20 []
          ] & flex_layout
  in do
    frame'of (r^.child 0) @?= frame (po 0   0) (di 100  60)
    frame'of (r^.child 1) @?= frame (po 0  60) (di 100  20)
    frame'of (r^.child 2) @?= frame (po 0  80) (di 100  20)

unit_grow3 ∷ IO ()
unit_grow3 =
  let r = mkItem 100 100
          [ mkItem 100 50 [] & it'grow .~ 2
          , mkItem 100 50 [] & it'grow .~ 3
          ] & flex_layout
  in do
    frame'of (r^.child 0) @?= frame (po 0   0) (di 100  50)
    frame'of (r^.child 1) @?= frame (po 0  50) (di 100  50)

-- unit_grow4 ∷ IO ()
-- unit_grow4 = (⊥)

-- unit_grow5 ∷ IO ()
-- unit_grow5 = (⊥)

-- unit_grow6 ∷ IO ()
-- unit_grow6 = (⊥)


-- * test_wrap
--
unit_wrap1 ∷ IO ()
unit_wrap1 =
  let r = mkItem 100 300
          [ mkItem 100 150 []
          , mkItem 100 150 []
          , mkItem 100 150 []
          , mkItem 100 150 []
          ] & it'wrap .~ NoWrap
            & flex_layout
  in do
    frame'of (r^.child 0) @?= frame (po 0   0) (di 100  75)
    frame'of (r^.child 1) @?= frame (po 0  75) (di 100  75)
    frame'of (r^.child 2) @?= frame (po 0 150) (di 100  75)
    frame'of (r^.child 3) @?= frame (po 0 225) (di 100  75)

-- unit_wrap2 ∷ IO ()
-- unit_wrap2 = (⊥)

-- unit_wrap3 ∷ IO ()
-- unit_wrap3 = (⊥)

-- unit_wrap4 ∷ IO ()
-- unit_wrap4 = (⊥)

-- unit_wrap5 ∷ IO ()
-- unit_wrap5 = (⊥)

-- unit_wrap6 ∷ IO ()
-- unit_wrap6 = (⊥)

-- unit_wrap7 ∷ IO ()
-- unit_wrap7 = (⊥)

-- unit_wrap8 ∷ IO ()
-- unit_wrap8 = (⊥)

-- unit_wrap9 ∷ IO ()
-- unit_wrap9 = (⊥)

-- unit_wrap10 ∷ IO ()
-- unit_wrap10 = (⊥)

-- unit_wrap11 ∷ IO ()
-- unit_wrap11 = (⊥)

-- unit_wrap12 ∷ IO ()
-- unit_wrap12 = (⊥)

-- unit_wrap13 ∷ IO ()
-- unit_wrap13 = (⊥)

-- unit_wrap14 ∷ IO ()
-- unit_wrap14 = (⊥)

-- unit_wrap15 ∷ IO ()
-- unit_wrap15 = (⊥)

-- unit_wrap16 ∷ IO ()
-- unit_wrap16 = (⊥)

-- unit_wrap17 ∷ IO ()
-- unit_wrap17 = (⊥)


-- * test_basis
--


-- * test_order
--


-- * test_margin
--


-- * test_shrink
--


-- * test_padding
--


-- * test_children
--


-- * test_position
--


-- * test_direction
--


-- * test_align_self
--


-- * test_align_items
--


-- * test_align_content
--


-- * test_default_values
--


-- * test_justify_content
--

{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-type-defaults #-}

module FlexTest where

import           Control.Lens
import           Data.List
import           Linear                            hiding (basis, trace)
import           Prelude.Unicode

import qualified Hedgehog              as H
import qualified Hedgehog.Gen          as Gen
import qualified Hedgehog.Range        as Range
import           Test.Tasty
import           Test.Tasty.ExpectedFailure
import           Test.Tasty.Hspec
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck             hiding (shrink)
import           Text.Printf                              (printf)


import Flatland
import Flex


-- *
--
test_grow1 ∷ TestTree
test_grow1 =
  let r = mkItem 60 240
          [ mkItem 60 30 [] () & style.grow .~ 0
          , mkItem 60  0 [] () & style.grow .~ 1
          , mkItem 60  0 [] () & style.grow .~ 2
          ] () & layout
  in testGroup "grow1: three children grow proportionally to property"
     [ testCase "child 0" $ r^.child 0.area @?= Area (po   0   0) (di 60  30)
     , testCase "child 1" $ r^.child 1.area @?= Area (po   0  30) (di 60  70)
     , testCase "child 2" $ r^.child 2.area @?= Area (po   0 100) (di 60 140)
     ]

test_grow2 ∷ TestTree
test_grow2 =
  let r = mkItem 100 100
          [ mkItem 100 20 [] () & style.grow .~ 1
          , mkItem 100 20 [] () & style.grow .~ 0
          , mkItem 100 20 [] ()
          ] () & layout
  in testGroup "grow2: only grow if property set to 1"
     [ testCase "child 0" $ r^.child 0.area @?= Area (po   0   0) (di 100  60)
     , testCase "child 1" $ r^.child 1.area @?= Area (po   0  60) (di 100  20)
     , testCase "child 2" $ r^.child 2.area @?= Area (po   0  80) (di 100  20)
     ]

test_grow3 ∷ TestTree
test_grow3 =
  let r = mkItem 100 100
          [ mkItem 100 50 [] () & style.grow .~ 2
          , mkItem 100 50 [] () & style.grow .~ 3
          ] () & layout
  in testGroup "grow3: growth has no effect if parent already full"
     [ testCase "child 0" $ r^.child 0.area @?= Area (po   0   0) (di 100  50)
     , testCase "child 1" $ r^.child 1.area @?= Area (po   0  50) (di 100  50)
     ]

test_grow4 ∷ TestTree
test_grow4 =
  let r = mkItem 100 100
          [ mkItem 100 25 [] ()
          , mkItem 100 25 [] ()
          ] () & style.grow .~ 2
               & layout
  in testGroup "grow4: parent growth property has no effect on children"
     [ testCase "child 0" $ r^.child 0.area @?= Area (po   0   0) (di 100  25)
     , testCase "child 1" $ r^.child 1.area @?= Area (po   0  25) (di 100  25)
     ]

test_grow5 ∷ TestTree
test_grow5 =
  let r = mkItem 100 100
          [ mkItem 100 25 [] () & style.grow .~ 1
          ] () & layout
  in testGroup "grow5: single child fills parent"
     [ testCase "child 0" $ r^.child 0.area @?= Area (po   0   0) (di 100  100)
     ]

test_grow6 ∷ TestTree
test_grow6 =
  let r = mkItem 100 100
          [ mkItem 100 45 [] () & style.grow .~ 1
          , mkItem 100 45 [] () & style.grow .~ 1
          ] () & layout
  in testGroup "grow6: two undersized children fill at equal rate"
     [ testCase "child 0" $ r^.child 0.area @?= Area (po   0   0) (di 100   50)
     , testCase "child 1" $ r^.child 1.area @?= Area (po   0  50) (di 100   50)
     ]
-- COMPLETE


-- *
--
test_wrap1 ∷ TestTree
test_wrap1 =
  let r = mkItem 100 300
          [ mkItem 100 150 [] ()
          , mkItem 100 150 [] ()
          , mkItem 100 150 [] ()
          , mkItem 100 150 [] ()
          ] () & style.wrap .~ NoWrap
               & layout
  in testGroup "wrap1: NoWrap doesn't enable wrapping"
     [ testCase "child 0" $ r^.child 0.area @?= Area (po   0   0) (di 100  75)
     , testCase "child 1" $ r^.child 1.area @?= Area (po   0  75) (di 100  75)
     , testCase "child 2" $ r^.child 2.area @?= Area (po   0 150) (di 100  75)
     , testCase "child 3" $ r^.child 3.area @?= Area (po   0 225) (di 100  75)
     ]

test_wrap2 ∷ TestTree
test_wrap2 =
  let r = mkItem 100 300
          [ mkItem 50 150 [] ()
          , mkItem 50 150 [] ()
          , mkItem 50 150 [] ()
          , mkItem 50 150 [] ()
          ] () & style.wrap          .~ Wrap
               & style.align'content .~ AlignStart
               & layout
  in testGroup "wrap2: four non-stretching children wrap across two rows"
     [ testCase "child 0" $ r^.child 0.area @?= Area (po   0   0) (di  50 150)
     , testCase "child 1" $ r^.child 1.area @?= Area (po   0 150) (di  50 150)
     , testCase "child 2" $ r^.child 2.area @?= Area (po  50   0) (di  50 150)
     , testCase "child 3" $ r^.child 3.area @?= Area (po  50 150) (di  50 150)
     ]

test_wrap3 ∷ TestTree
test_wrap3 =
  let r = mkItem 120 120
          [ mkItem 50 50 [] ()
          , mkItem 50 50 [] ()
          , mkItem 50 50 [] ()
          ] () & style.wrap          .~ Wrap
               & style.align'content .~ AlignStart
               & layout
  in testGroup "wrap3: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (po   0   0) (di  50  50)
     , testCase "child 1" $ r^.child 1.area @?= Area (po   0  50) (di  50  50)
     , testCase "child 2" $ r^.child 2.area @?= Area (po  50   0) (di  50  50)
     ]

test_wrap4 ∷ TestTree
test_wrap4 =
  let r = mkItem 120 120
          [ mkItem 25 50 [] ()
          , mkItem 25 50 [] ()
          , mkItem 25 50 [] ()
          , mkItem 25 50 [] ()
          , mkItem 25 50 [] ()
          , mkItem 25 50 [] ()
          ] () & style.wrap          .~ Wrap
               & style.align'content .~ AlignStart
               & layout
  in testGroup "wrap4: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (po   0   0) (di  25  50)
     , testCase "child 1" $ r^.child 1.area @?= Area (po   0  50) (di  25  50)
     , testCase "child 2" $ r^.child 2.area @?= Area (po  25   0) (di  25  50)
     , testCase "child 3" $ r^.child 3.area @?= Area (po  25  50) (di  25  50)
     , testCase "child 4" $ r^.child 4.area @?= Area (po  50   0) (di  25  50)
     , testCase "child 5" $ r^.child 5.area @?= Area (po  50  50) (di  25  50)
     ]

test_wrap5 ∷ TestTree
test_wrap5 =
  let r = mkItem 120 120
          [ mkItem 50 50 [] ()
          , mkItem 50 50 [] ()
          , mkItem 50 50 [] ()
          ] () & style.wrap            .~ Wrap
               & style.justify'content .~ AlignEnd
               & style.align'content   .~ AlignStart
               & layout
  in testGroup "wrap5: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (po   0  20) (di  50  50)
     , testCase "child 1" $ r^.child 1.area @?= Area (po   0  70) (di  50  50)
     , testCase "child 2" $ r^.child 2.area @?= Area (po  50  70) (di  50  50)
     ]

test_wrap6 ∷ TestTree
test_wrap6 =
  let r = mkItem 120 120
          [ mkItem 50 50 [] ()
          , mkItem 50 50 [] ()
          , mkItem 50 50 [] ()
          ] () & style.wrap            .~ Wrap
               & style.justify'content .~ AlignCenter
               & style.align'content   .~ AlignStart
               & layout
  in testGroup "wrap6: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (po   0  10) (di  50  50)
     , testCase "child 1" $ r^.child 1.area @?= Area (po   0  60) (di  50  50)
     , testCase "child 2" $ r^.child 2.area @?= Area (po  50  35) (di  50  50)
     ]

test_wrap7 ∷ TestTree
test_wrap7 =
  let r = mkItem 120 120
          [ mkItem 50 50 [] ()
          , mkItem 50 50 [] ()
          , mkItem 50 50 [] ()
          ] () & style.wrap            .~ Wrap
               & style.justify'content .~ AlignSpaceAround
               & style.align'content   .~ AlignStart
               & layout
  in testGroup "wrap7: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (po   0   5) (di  50  50)
     , testCase "child 1" $ r^.child 1.area @?= Area (po   0  65) (di  50  50)
     , testCase "child 2" $ r^.child 2.area @?= Area (po  50  35) (di  50  50)
     ]

test_wrap8 ∷ TestTree
test_wrap8 =
  let r = mkItem 120 120
          [ mkItem 50 50 [] ()
          , mkItem 50 50 [] ()
          , mkItem 50 50 [] ()
          ] () & style.wrap            .~ Wrap
               & style.justify'content .~ AlignSpaceBetween
               & style.align'content   .~ AlignStart
               & layout
  in testGroup "wrap8: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (po   0   0) (di  50  50)
     , testCase "child 1" $ r^.child 1.area @?= Area (po   0  70) (di  50  50)
     , testCase "child 2" $ r^.child 2.area @?= Area (po  50   0) (di  50  50)
     ]

test_wrap9 ∷ TestTree
test_wrap9 =
  let r = mkItem 120 120
          [ mkItem 50 50 [] ()
          , mkItem 50 50 [] () & style.grow .~ 1
          , mkItem 50 50 [] () & style.grow .~ 1
          , mkItem 50 50 [] ()
          ] () & style.wrap            .~ Wrap
               & style.align'content   .~ AlignStart
               & layout
  in testGroup "wrap9: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (po   0   0) (di  50  50)
     , testCase "child 1" $ r^.child 1.area @?= Area (po   0  50) (di  50  70)
     , testCase "child 2" $ r^.child 2.area @?= Area (po  50   0) (di  50  70)
     , testCase "child 3" $ r^.child 3.area @?= Area (po  50  70) (di  50  50)
     ]

test_wrap10 ∷ TestTree
test_wrap10 =
  let r = mkItem 120 120
          [ mkItem 50 40 [] ()
          , mkItem 70 30 [] ()
          , mkItem 60 40 [] ()
          , mkItem 40 50 [] ()
          , mkItem 50 60 [] ()
          ] () & style.wrap            .~ Wrap
               & style.align'items     .~ AlignStart
               & layout
  in testGroup "wrap10: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (po   0   0) (di  50  40)
     , testCase "child 1" $ r^.child 1.area @?= Area (po   0  40) (di  70  30)
     , testCase "child 2" $ r^.child 2.area @?= Area (po   0  70) (di  60  40)
     , testCase "child 3" $ r^.child 3.area @?= Area (po  70   0) (di  40  50)
     , testCase "child 4" $ r^.child 4.area @?= Area (po  70  50) (di  50  60)
     ]

-- XXX: ⊥
test_wrap11 ∷ TestTree
test_wrap11 =
  let r = mkItem 120 120
          [ mkItem 50 40 [] ()
          , mkItem 70 30 [] ()
          , mkItem 60 40 [] ()
          , mkItem 40 50 [] ()
          , mkItem 50 60 [] ()
          ] () & style.wrap            .~ Wrap
               & style.align'items     .~ AlignCenter
               & layout
  in testGroup "wrap11: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (po  10   0) (di  50  40)
     , testCase "child 1" $ r^.child 1.area @?= Area (po   0  40) (di  70  30)
     , testCase "child 2" $ r^.child 2.area @?= Area (po   5  70) (di  60  40)
     , testCase "child 3" $ r^.child 3.area @?= Area (po  75   0) (di  40  50)
     , testCase "child 4" $ r^.child 4.area @?= Area (po  70  50) (di  50  60)
     ]

test_wrap12 ∷ TestTree
test_wrap12 =
  let r = mkItem 120 120
          [ mkItem 50 40 [] ()
          , mkItem 70 30 [] ()
          , mkItem 60 40 [] ()
          , mkItem 40 50 [] ()
          , mkItem 50 60 [] ()
          ] () & style.wrap            .~ Wrap
               & style.align'items     .~ AlignEnd
               & layout
  in testGroup "wrap12: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (po  20   0) (di  50  40)
     , testCase "child 1" $ r^.child 1.area @?= Area (po   0  40) (di  70  30)
     , testCase "child 2" $ r^.child 2.area @?= Area (po  10  70) (di  60  40)
     , testCase "child 3" $ r^.child 3.area @?= Area (po  80   0) (di  40  50)
     , testCase "child 4" $ r^.child 4.area @?= Area (po  70  50) (di  50  60)
     ]

test_wrap13 ∷ TestTree
test_wrap13 =
  let r = mkItem 120 120
          [ mkItem 50 40 [] () & style.align'self .~ AlignEnd
          , mkItem 70 30 [] ()
          , mkItem 60 40 [] () & style.align'self .~ AlignCenter
          , mkItem 40 50 [] () & style.align'self .~ AlignStart
          , mkItem 50 60 [] ()
          , mkItem 10 10 [] () & style.align'self .~ AlignEnd
          ] () & style.wrap .~ Wrap
               & layout
  in testGroup "wrap13: potpourri"
     [ testCase "child 0" $ r^.child 0.area @?= Area (po  20   0) (di  50  40)
     , testCase "child 1" $ r^.child 1.area @?= Area (po   0  40) (di  70  30)
     , testCase "child 2" $ r^.child 2.area @?= Area (po   5  70) (di  60  40)
     , testCase "child 3" $ r^.child 3.area @?= Area (po  70   0) (di  40  50)
     , testCase "child 4" $ r^.child 4.area @?= Area (po  70  50) (di  50  60)
     , testCase "child 5" $ r^.child 5.area @?= Area (po 110 110) (di  10  10)
     ]

test_wrap14 ∷ TestTree
test_wrap14 =
  let r = mkItem 120 120
          [ mkItem 50 50 [] ()
          , mkItem 50 50 [] ()
          , mkItem 50 50 [] ()
          ] () & style.wrap            .~ ReverseWrap
               & style.align'content   .~ AlignStart
               & layout
  in testGroup "wrap14: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (po  70   0) (di  50  50)
     , testCase "child 1" $ r^.child 1.area @?= Area (po  70  50) (di  50  50)
     , testCase "child 2" $ r^.child 2.area @?= Area (po  20   0) (di  50  50)
     ]

test_wrap15 ∷ TestTree
test_wrap15 =
  let r = mkItem 120 120
          [ mkItem 25 50 [] ()
          , mkItem 25 50 [] ()
          , mkItem 25 50 [] ()
          , mkItem 25 50 [] ()
          , mkItem 25 50 [] ()
          , mkItem 25 50 [] ()
          ] () & style.wrap          .~ ReverseWrap
               & style.align'content .~ AlignStart
               & layout
  in testGroup "wrap15: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (po  95   0) (di  25  50)
     , testCase "child 1" $ r^.child 1.area @?= Area (po  95  50) (di  25  50)
     , testCase "child 2" $ r^.child 2.area @?= Area (po  70   0) (di  25  50)
     , testCase "child 3" $ r^.child 3.area @?= Area (po  70  50) (di  25  50)
     , testCase "child 4" $ r^.child 4.area @?= Area (po  45   0) (di  25  50)
     , testCase "child 5" $ r^.child 5.area @?= Area (po  45  50) (di  25  50)
     ]

test_wrap16 ∷ TestTree
test_wrap16 =
  let r = mkItem 120 120
          [ mkItem 20 50 [] ()
          , mkItem 20 50 [] ()
          , mkItem 20 50 [] ()
          ] () & style.direction       .~ DirColumn
               & style.wrap            .~ Wrap
               & style.align'content   .~ AlignStretch
               & layout
  in testGroup "wrap16: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (po   0   0) (di  20  50)
     , testCase "child 1" $ r^.child 1.area @?= Area (po   0  50) (di  20  50)
     , testCase "child 2" $ r^.child 2.area @?= Area (po  60   0) (di  20  50)
     ]

test_wrap17 ∷ TestTree
test_wrap17 =
  let r = mkItem 120 120
          [ mkItem 20 50 [] ()
          , mkItem 20 50 [] ()
          , mkItem 20 50 [] ()
          , mkItem 20 50 [] ()
          , mkItem 20 50 [] ()
          ] () & style.direction       .~ DirColumn
               & style.wrap            .~ Wrap
               & style.align'content   .~ AlignStretch
               & layout
  in testGroup "wrap17: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (po   0   0) (di  20  50)
     , testCase "child 1" $ r^.child 1.area @?= Area (po   0  50) (di  20  50)
     , testCase "child 2" $ r^.child 2.area @?= Area (po  40   0) (di  20  50)
     , testCase "child 3" $ r^.child 3.area @?= Area (po  40  50) (di  20  50)
     , testCase "child 4" $ r^.child 4.area @?= Area (po  80   0) (di  20  50)
     ]
-- COMPLETE


-- *
--
test_basis1 ∷ TestTree
test_basis1 =
  let r = mkItem 100 100
          [ mkItem' (Just 100) Nothing [] () & style.basis .~ 60
          , mkItem        100  40      [] ()
          ] () & layout
  in testGroup "basis1: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (po   0   0) (di 100  60)
     , testCase "child 1" $ r^.child 1.area @?= Area (po   0  60) (di 100  40)
     ]

test_basis2 ∷ TestTree
test_basis2 =
  let r = mkItem 100 100
          [ mkItem 100 40 [] () & style.basis .~ 60
          , mkItem 100 40 [] ()
          ] () & layout
  in testGroup "basis2: the basis attribute has priority over width/height"
     [ testCase "child 0" $ r^.child 0.area @?= Area (po   0   0) (di 100  60)
     , testCase "child 1" $ r^.child 1.area @?= Area (po   0  60) (di 100  40)
     ]

test_basis3 ∷ TestTree
test_basis3 =
  let r = mkItem 100 100
          [ mkItem' (Just 100) Nothing [] () & style.basis .~ (-60)
          , mkItem        100  40      [] ()
          ] () & layout
  in testGroup "basis3: the basis attribute is ignored if negative"
     [ testCase "child 0" $ r^.child 0.area @?= Area (po   0   0) (di 100   0)
     , testCase "child 1" $ r^.child 1.area @?= Area (po   0   0) (di 100  40)
     ]

test_basis4 ∷ TestTree
test_basis4 =
  let r = mkItem 100 100
          [ mkItem 100 40 [] () & style.basis .~ (-60)
          , mkItem 100 40 [] ()
          ] () & layout
  in testGroup "basis4: the basis attribute is ignored if negative"
     [ testCase "child 0" $ r^.child 0.area @?= Area (po   0   0) (di 100  40)
     , testCase "child 1" $ r^.child 1.area @?= Area (po   0  40) (di 100  40)
     ]

test_basis5 ∷ TestTree
test_basis5 =
  let r = mkItem 100 100
          [ mkItem 100 40 [] () & style.basis .~ 0
          , mkItem 100 40 [] ()
          ] () & layout
  in testGroup "basis5: the basis attribute is ignored if 0"
     [ testCase "child 0" $ r^.child 0.area @?= Area (po   0   0) (di 100  40)
     , testCase "child 1" $ r^.child 1.area @?= Area (po   0  40) (di 100  40)
     ]
-- COMPLETE


-- *
--
test_order1 ∷ TestTree
test_order1 =
  let r = mkItem 200 200
          [ mkItem 50 50 [] () & style.order .~ Just 1
          , mkItem 50 50 [] () & style.order .~ Just 3
          , mkItem 50 50 [] () & style.order .~ Just 2
          ] () & layout
  in testGroup "order1: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (po   0   0) (di  50  50)
     , expectFail $
       testCase "child 1" $ r^.child 1.area @?= Area (po   0 100) (di  50  50)
     , expectFail $
       testCase "child 2" $ r^.child 2.area @?= Area (po   0  50) (di  50  50)
     ]

test_order2 ∷ TestTree
test_order2 =
  let r = mkItem 200 200
          [ mkItem 50 50 [] () & style.order .~ Just 2
          , mkItem 50 50 [] () & style.order .~ Just 3
          , mkItem 50 50 [] () & style.order .~ Just 1
          ] () & style.direction .~ DirColumnReverse
               & layout
  in testGroup "order2: "
     [ expectFail $
       testCase "child 0" $ r^.child 0.area @?= Area (po   0 100) (di  50  50)
     , expectFail $
       testCase "child 1" $ r^.child 1.area @?= Area (po   0  50) (di  50  50)
     , expectFail $
       testCase "child 2" $ r^.child 2.area @?= Area (po   0 150) (di  50  50)
     ]

test_order3 ∷ TestTree
test_order3 =
  let r = mkItem 200 200
          [ mkItem 50 50 [] ()
          , mkItem 50 50 [] () & style.order .~ Just (-1)
          , mkItem 50 50 [] ()
          ] () & style.direction .~ DirColumnReverse
               & layout
  in testGroup "order3: "
     [ expectFail $
       testCase "child 0" $ r^.child 0.area @?= Area (po   0  50) (di  50  50)
     , expectFail $
       testCase "child 1" $ r^.child 1.area @?= Area (po   0   0) (di  50  50)
     , expectFail $
       testCase "child 2" $ r^.child 2.area @?= Area (po   0 100) (di  50  50)
     ]
--
-- test_order4 skipped:
--  > this test ensures that the insertion order of the children is preserved when they get re-ordered during layout
--


-- *
--
test_margin1 ∷ TestTree
test_margin1 =
  let r = mkItem 100 100
          [ mkItem  25 25 [] ()
          , mkItem  25 25 [] () & style.margin .~ LRTB 15 15 10 10
          , mkItem  25 25 [] ()
          ] () & style.align'items     .~ AlignStart
               & style.justify'content .~ AlignStart
               & layout
  in testGroup "margin1: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (po   0   0) (di  25  25)
     , testCase "child 1" $ r^.child 1.area @?= Area (po  15  35) (di  25  25)
     , testCase "child 2" $ r^.child 2.area @?= Area (po   0  70) (di  25  25)
     ]

test_margin2 ∷ TestTree
test_margin2 =
  let r = mkItem 100 100
          [ mkItem  25 25 [] ()
          , mkItem  25 25 [] () & style.margin .~ LRTB 15 15 10 10
          , mkItem  25 25 [] ()
          ] () & style.align'items     .~ AlignEnd
               & style.justify'content .~ AlignStart
               & layout
  in testGroup "margin2: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (po  75   0) (di  25  25)
     , testCase "child 1" $ r^.child 1.area @?= Area (po  60  35) (di  25  25)
     , testCase "child 2" $ r^.child 2.area @?= Area (po  75  70) (di  25  25)
     ]

test_margin3 ∷ TestTree
test_margin3 =
  let r = mkItem 100 100
          [ mkItem  25 25 [] ()
          , mkItem  25 25 [] () & style.margin .~ LRTB 15 15 10 10
          , mkItem  25 25 [] ()
          ] () & style.align'items     .~ AlignStart
               & style.justify'content .~ AlignEnd
               & layout
  in testGroup "margin3: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (po   0   5) (di  25  25)
     , testCase "child 1" $ r^.child 1.area @?= Area (po  15  40) (di  25  25)
     , testCase "child 2" $ r^.child 2.area @?= Area (po   0  75) (di  25  25)
     ]

test_margin4 ∷ TestTree
test_margin4 =
  let r = mkItem 100 100
          [ mkItem  25 25 [] ()
          , mkItem  25 25 [] () & style.margin .~ LRTB 15 15 10 10
          , mkItem  25 25 [] ()
          ] () & style.align'items     .~ AlignEnd
               & style.justify'content .~ AlignEnd
               & layout
  in testGroup "margin4: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (po  75   5) (di  25  25)
     , testCase "child 1" $ r^.child 1.area @?= Area (po  60  40) (di  25  25)
     , testCase "child 2" $ r^.child 2.area @?= Area (po  75  75) (di  25  25)
     ]

test_margin5 ∷ TestTree
test_margin5 =
  let r = mkItem 100 100
          [ mkItem  10 10 [] ()
          , mkItem  10 10 [] () & style.margin .~ LRTB 15 10 0 0
          , mkItem  10 10 [] ()
          ] () & style.align'items     .~ AlignCenter
               & style.justify'content .~ AlignStart
               & layout
  in testGroup "margin5: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (po  45   0) (di  10  10)
     , testCase "child 1" $ r^.child 1.area @?= Area (po  50  10) (di  10  10)
     , testCase "child 2" $ r^.child 2.area @?= Area (po  45  20) (di  10  10)
     ]

test_margin6 ∷ TestTree
test_margin6 =
  let r = mkItem 100 100
          [ mkItem  10 10 [] ()
          , mkItem   0 10 [] () & style.margin     .~ LRTB 15 10 0 0
                                & style.align'self .~ AlignStretch
          , mkItem  10 10 [] ()
          ] () & layout
  in testGroup "margin6 "
     [ testCase "child 0" $ r^.child 0.area @?= Area (po   0   0) (di  10  10)
     , testCase "child 1" $ r^.child 1.area @?= Area (po  15  10) (di  75  10)
     , testCase "child 2" $ r^.child 2.area @?= Area (po   0  20) (di  10  10)
     ]

test_margin7 ∷ TestTree
test_margin7 =
  let r = mkItem 100 100
          [ mkItem  10 10 [] ()
          , mkItem  10 10 [] () & style.margin     .~ LRTB 15 10 0 0
                                & style.align'self .~ AlignStretch
          , mkItem  10 10 [] ()
          ] () & layout
  in testGroup "margin7 "
     [ testCase "child 0" $ r^.child 0.area @?= Area (po   0   0) (di  10  10)
     , testCase "child 1" $ r^.child 1.area @?= Area (po  15  10) (di  10  10)
     , testCase "child 2" $ r^.child 2.area @?= Area (po   0  20) (di  10  10)
     ]

test_margin8 ∷ TestTree
test_margin8 =
  let r = mkItem 100 100
          [ mkItem' Nothing (Just 10) [] () & style.margin .~ LRTB 10  0 0 0
          , mkItem' Nothing (Just 10) [] () & style.margin .~ LRTB  0 10 0 0
          , mkItem' Nothing (Just 10) [] () & style.margin .~ LRTB 10 20 0 0
          ] () & style.direction .~ DirColumn
               & layout
  in testGroup "margin8 "
     [ testCase "child 0" $ r^.child 0.area @?= Area (po  10   0) (di  90  10)
     , testCase "child 1" $ r^.child 1.area @?= Area (po   0  10) (di  90  10)
     , testCase "child 2" $ r^.child 2.area @?= Area (po  10  20) (di  70  10)
     ]

test_margin9 ∷ TestTree
test_margin9 =
  let r = mkItem 100 100
          [ mkItem' (Just 10) Nothing [] () & style.margin .~ LRTB 0 0 10  0
          , mkItem' (Just 10) Nothing [] () & style.margin .~ LRTB 0 0  0 10
          , mkItem' (Just 10) Nothing [] () & style.margin .~ LRTB 0 0 10 20
          ] () & style.direction .~ DirRow
               & layout
  in testGroup "margin9 "
     [ testCase "child 0" $ r^.child 0.area @?= Area (po   0  10) (di  10  90)
     , testCase "child 1" $ r^.child 1.area @?= Area (po  10   0) (di  10  90)
     , testCase "child 2" $ r^.child 2.area @?= Area (po  20  10) (di  10  70)
     ]
-- COMPLETE


-- *
--
test_shrink1 ∷ TestTree
test_shrink1 =
  let r = mkItem 100 100
          [ mkItem 100 100 [] () & style.shrink .~ 2
          , mkItem 100 100 [] () & style.shrink .~ 3
          ] () & layout
  in testGroup "shrink1"
     [ testCase "child 0" $ r^.child 0.area @?= Area (po   0   0) (di 100  60)
     , testCase "child 1" $ r^.child 1.area @?= Area (po   0  60) (di 100  40)
     ]

test_shrink2 ∷ TestTree
test_shrink2 =
  let r = mkItem 100 100
          [ mkItem 100 100 [] ()
          , mkItem 100 100 [] () & style.shrink .~ 4
          ] () & layout
  in testGroup "shrink2"
     [ testCase "child 0" $ r^.child 0.area @?= Area (po   0   0) (di 100  80)
     , testCase "child 1" $ r^.child 1.area @?= Area (po   0  80) (di 100  20)
     ]

test_shrink3 ∷ TestTree
test_shrink3 =
  let r = mkItem 100 100
          [ mkItem 100 40 [] () & style.shrink .~ 2
          , mkItem 100 40 [] () & style.shrink .~ 3
          ] () & layout
  in testGroup "shrink3: the shrink attributes are not taken into account when there is enough flexible space available"
     [ testCase "child 0" $ r^.child 0.area @?= Area (po   0   0) (di 100  40)
     , testCase "child 1" $ r^.child 1.area @?= Area (po   0  40) (di 100  40)
     ]

test_shrink4 ∷ TestTree
test_shrink4 =
  let r = mkItem 100 100
          [ mkItem 100 25 [] ()
          , mkItem 100 25 [] ()
          ] () & style.shrink .~ 2
               & layout
  in testGroup "shrink4: the shrink attribute is not inherited from children"
     [ testCase "child 0" $ r^.child 0.area @?= Area (po   0   0) (di 100  25)
     , testCase "child 1" $ r^.child 1.area @?= Area (po   0  25) (di 100  25)
     ]

test_shrink5 ∷ TestTree
test_shrink5 =
  let r = mkItem 100 100
          [ mkItem 100 550 [] () & style.shrink .~ 1
          ] () & layout
  in testGroup "shrink5: all the container space is used when there is only one item with a positive value for the shrink attribute"
     [ testCase "child 0" $ r^.child 0.area @?= Area (po   0   0) (di 100 100)
     ]

test_shrink6 ∷ TestTree
test_shrink6 =
  let r = mkItem 100 100
          [ mkItem 100 75 [] () & style.shrink .~ 1
          , mkItem 100 75 [] () & style.shrink .~ 1
          ] () & layout
  in testGroup "shrink6: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (po   0   0) (di 100  50)
     , testCase "child 1" $ r^.child 1.area @?= Area (po   0  50) (di 100  50)
     ]
-- COMPLETE


-- * padding
--
test_padding1 ∷ TestTree
test_padding1 =
  let r = mkItem 100 100
          [ mkItem  25 25 [] ()
          ] () & style.direction       .~ DirColumn
               & style.justify'content .~ AlignStart
               & style.align'items     .~ AlignStart
               & style.padding         .~ LRTB 10 15 15 10
               & layout
  in testGroup "padding1: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (po  10  15) (di  25  25)
     ]

test_padding2 ∷ TestTree
test_padding2 =
  let r = mkItem 100 100
          [ mkItem  25 25 [] ()
          ] () & style.direction       .~ DirColumn
               & style.justify'content .~ AlignEnd
               & style.align'items     .~ AlignStart
               & style.padding         .~ LRTB 10 15 15 10
               & layout
  in testGroup "padding2: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (po  10  65) (di  25  25)
     ]

test_padding3 ∷ TestTree
test_padding3 =
  let r = mkItem 100 100
          [ mkItem  25 25 [] ()
          ] () & style.direction       .~ DirColumn
               & style.justify'content .~ AlignEnd
               & style.align'items     .~ AlignEnd
               & style.padding         .~ LRTB 10 15 15 10
               & layout
  in testGroup "padding3: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (po  60  65) (di  25  25)
     ]

test_padding4 ∷ TestTree
test_padding4 =
  let r = mkItem 100 100
          [ mkItem  25 25 [] ()
          ] () & style.direction       .~ DirColumn
               & style.justify'content .~ AlignStart
               & style.align'items     .~ AlignEnd
               & style.padding         .~ LRTB 10 15 15 10
               & layout
  in testGroup "padding4: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (po  60  15) (di  25  25)
     ]

test_padding5 ∷ TestTree
test_padding5 =
  let r = mkItem 100 100
          [ mkItem  0 25 [] () & style.align'self .~ AlignStretch
          ] () & style.direction       .~ DirColumn
               & style.justify'content .~ AlignStart
               & style.align'items     .~ AlignStart
               & style.padding         .~ LRTB 10 15 15 10
               & layout
  in testGroup "padding5: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (po  10  15) (di  75  25)
     ]
-- COMPLETE


-- *
--
-- test_children1 -- ignoring
-- test_children2 -- ignoring
-- test_children3 -- ignoring
--
test_children4 ∷ TestTree
test_children4 =
  let r = mkItem 100 100
          [ mkItem  90  90
            [ mkItem  80  80
              [ mkItem  70  70
                [ mkItem  60  60
                  [ mkItem  50  50
                    [] () & style.align'items     .~ AlignCenter
                          & style.justify'content .~ AlignCenter
                  ] () & style.align'items     .~ AlignCenter
                       & style.justify'content .~ AlignCenter
                ] () & style.align'items     .~ AlignCenter
                     & style.justify'content .~ AlignCenter
              ] () & style.align'items     .~ AlignCenter
                   & style.justify'content .~ AlignCenter
            ] () & style.align'items     .~ AlignCenter
                 & style.justify'content .~ AlignCenter
          ] () & style.align'items     .~ AlignCenter
               & style.justify'content .~ AlignCenter
               & layout
  in testGroup "children4: "
     [ testCase "child 0"         $ r^.child 0.area                                 @?= Area (po   5   5) (di  90  90)
     , testCase "child 0.0"       $ r^.child 0.child 0.area                         @?= Area (po   5   5) (di  80  80)
     , testCase "child 0.0.0"     $ r^.child 0.child 0.child 0.area                 @?= Area (po   5   5) (di  70  70)
     , testCase "child 0.0.0.0"   $ r^.child 0.child 0.child 0.child 0.area         @?= Area (po   5   5) (di  60  60)
     , testCase "child 0.0.0.0.0" $ r^.child 0.child 0.child 0.child 0.child 0.area @?= Area (po   5   5) (di  50  50)
     ]
-- COMPLETE


-- * position
--
test_position1 ∷ TestTree
test_position1 =
  let r = mkItem 100 100
          [ mkItem  10 10 [] () & style.positioning .~ Absolute
          ] () & layout
  in testGroup "position1: items with an absolute position default to the left/top corner"
     [ testCase "child 0" $ r^.child 0.area @?= Area (po   0   0) (di  10  10)
     ]

test_position2 ∷ TestTree
test_position2 =
  let r = mkItem 100 100
          [ mkItem  10 10 [] () & style.positioning .~ Absolute
                                & absolute              .~ LRTB (Just 10) Nothing  (Just 10) Nothing
          , mkItem  10 10 [] () & style.positioning .~ Absolute
                                & absolute              .~ LRTB  Nothing (Just 10) (Just 10) Nothing
          , mkItem  10 10 [] () & style.positioning .~ Absolute
                                & absolute              .~ LRTB  Nothing (Just 10)  Nothing (Just 10)
          , mkItem  10 10 [] () & style.positioning .~ Absolute
                                & absolute              .~ LRTB (Just 10) Nothing   Nothing (Just 10)
          ] () & style.align'items     .~ AlignCenter
               & style.justify'content .~ AlignStart
               & layout
  in testGroup "position2 "
     [ testCase "child 0" $ r^.child 0.area @?= Area (po  10  10) (di  10  10)
     , testCase "child 1" $ r^.child 1.area @?= Area (po  80  10) (di  10  10)
     , testCase "child 2" $ r^.child 2.area @?= Area (po  80  80) (di  10  10)
     , testCase "child 3" $ r^.child 3.area @?= Area (po  10  80) (di  10  10)
     ]

test_position3 ∷ TestTree
test_position3 =
  let r = mkItem 100 100
          [ mkItem  10 10 [] () & style.positioning .~ Absolute
                                & absolute              .~ LRTB (Just 10) (Just 10) Nothing   Nothing
          , mkItem  10 10 [] () & style.positioning .~ Absolute
                                & absolute              .~ LRTB  Nothing   Nothing (Just 10) (Just 10)
          ] () & layout
  in testGroup "position3: if both left/right or top/bottom are given, left/top get the priority if the item has the appropriate size dimension set"
     [ testCase "child 0" $ r^.child 0.area @?= Area (po  10   0) (di  10  10)
     , testCase "child 1" $ r^.child 1.area @?= Area (po   0  10) (di  10  10)
     ]

test_position4 ∷ TestTree
test_position4 =
  let r = mkItem 100 100
          [ mkItem' Nothing (Just 20) [] () & style.positioning .~ Absolute
                                            & absolute              .~ LRTB (Just 10) (Just 10) Nothing   Nothing
          , mkItem' (Just 20) Nothing [] () & style.positioning .~ Absolute
                                            & absolute              .~ LRTB  Nothing   Nothing (Just 10) (Just 10)
          ] () & layout
  in testGroup "position4: if both left/right or top/bottom are given, the item is properly resized if the appropriate size dimension hasn't been set"
     [ testCase "child 0" $ r^.child 0.area @?= Area (po  10   0) (di  80  20)
     , testCase "child 1" $ r^.child 1.area @?= Area (po   0  10) (di  20  80)
     ]

test_position5 ∷ TestTree
test_position5 =
  let r = mkItem 100 100
          [ mkItem  10 10 [] () & style.positioning .~ Absolute
                                & style.basis       .~ 20
                                & absolute              .~ LRTB (Just 10) Nothing Nothing (Just 10)
          ] () & layout
  in testGroup "position5: the `basis' property is ignored for items with an absolute position"
     [ testCase "child 0" $ r^.child 0.area @?= Area (po  10  80) (di  10  10)
     ]

test_position6 ∷ TestTree
test_position6 =
  let r = mkItem 200 200
          [ mkItem  50 50 [] ()
          , mkItem  50 50 [] () & style.positioning .~ Absolute
                                & absolute              .~ LRTB Nothing (Just 0) Nothing (Just 0)
          , mkItem  50 50 [] ()
          ] () & style.direction .~ DirRow
               & layout
  in testGroup "position6: items with an absolute position are separated from the other items during the layout"
     [ testCase "child 0" $ r^.child 0.area @?= Area (po   0   0) (di  50  50)
     , testCase "child 1" $ r^.child 1.area @?= Area (po 150 150) (di  50  50)
     , testCase "child 2" $ r^.child 2.area @?= Area (po  50   0) (di  50  50)
     ]

test_position7 ∷ TestTree
test_position7 =
  let r = mkItem 120 120
          [ mkItem  50 50 [] ()
          , mkItem  50 50 [] ()
          , mkItem  50 50 [] () & style.positioning .~ Absolute
                                & absolute              .~ LRTB Nothing (Just 0) (Just 0) Nothing
          , mkItem  50 50 [] ()
          ] () & style.wrap            .~ Wrap
               & style.justify'content .~ AlignSpaceAround
               & style.align'content   .~ AlignStart
               & layout
  in testGroup "position7: items with an absolute position are separated from the other items during the layout and are not taken into account when calculating spacing"
     [ testCase "child 0" $ r^.child 0.area @?= Area (po   0   5) (di  50  50)
     , testCase "child 1" $ r^.child 1.area @?= Area (po   0  65) (di  50  50)
     , testCase "child 2" $ r^.child 2.area @?= Area (po  70   0) (di  50  50)
     , testCase "child 3" $ r^.child 3.area @?= Area (po  50  35) (di  50  50)
     ]

test_position8 ∷ TestTree
test_position8 =
  let r = mkItem 100 100
          [ mkItem' Nothing Nothing
            [ mkItem  60 60
              [ mkItem  40 40 [] () & style.positioning .~ Absolute
                                    & absolute              .~ LRTB (Just 10)  Nothing   Nothing (Just 10)
              ] () & style.positioning .~ Absolute
                   & absolute              .~ LRTB  Nothing  (Just 10) (Just 10) Nothing
            ] () & style.positioning .~ Absolute
                 & absolute              .~ LRTB (Just 10) (Just 10) (Just 10) (Just 10)
          ] () & style.direction .~ DirRow
               & layout
  in testGroup "position8: items with an absolute position can be nested"
     [ testCase "child 0" $ r^.child 0.area                 @?= Area (po  10  10) (di  80  80)
     , testCase "child 1" $ r^.child 0.child 0.area         @?= Area (po  10  10) (di  60  60)
     , testCase "child 2" $ r^.child 0.child 0.child 0.area @?= Area (po  10  10) (di  40  40)
     ]
-- COMPLETE


-- * direction
--
test_direction1 ∷ TestTree
test_direction1 =
  let r = mkItem 200 200
          [ mkItem 50 50 [] ()
          , mkItem 50 50 [] ()
          , mkItem 50 50 [] ()
          ] () & style.direction .~ DirRow
               & layout
  in testGroup "direction1: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (po   0   0) (di  50  50)
     , testCase "child 1" $ r^.child 1.area @?= Area (po  50   0) (di  50  50)
     , testCase "child 2" $ r^.child 2.area @?= Area (po 100   0) (di  50  50)
     ]

test_direction2 ∷ TestTree
test_direction2 =
  let r = mkItem 200 200
          [ mkItem 50 50 [] ()
          , mkItem 50 50 [] ()
          , mkItem 50 50 [] ()
          ] () & style.direction .~ DirColumn
               & layout
  in testGroup "direction2: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (po   0   0) (di  50  50)
     , testCase "child 1" $ r^.child 1.area @?= Area (po   0  50) (di  50  50)
     , testCase "child 2" $ r^.child 2.area @?= Area (po   0 100) (di  50  50)
     ]

test_direction3 ∷ TestTree
test_direction3 =
  let r = mkItem 200 200
          [ mkItem 50 50 [] ()
          , mkItem 50 50 [] ()
          , mkItem 50 50 [] ()
          ] () & style.direction .~ DirRowReverse
               & layout
  in testGroup "direction3: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (po 150   0) (di  50  50)
     , testCase "child 1" $ r^.child 1.area @?= Area (po 100   0) (di  50  50)
     , testCase "child 2" $ r^.child 2.area @?= Area (po  50   0) (di  50  50)
     ]

test_direction4 ∷ TestTree
test_direction4 =
  let r = mkItem 200 200
          [ mkItem 50 50 [] ()
          , mkItem 50 50 [] ()
          , mkItem 50 50 [] ()
          ] () & style.direction .~ DirColumnReverse
               & layout
  in testGroup "direction4: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (po   0 150) (di  50  50)
     , testCase "child 1" $ r^.child 1.area @?= Area (po   0 100) (di  50  50)
     , testCase "child 2" $ r^.child 2.area @?= Area (po   0  50) (di  50  50)
     ]
-- COMPLETE


-- *
--
test_align_self1 ∷ TestTree
test_align_self1 =
  let r = mkItem 100 100
          [ mkItem  50  25 [] () & style.align'self .~ AlignStart
          , mkItem  50  25 [] () & style.align'self .~ AlignStart
          , mkItem  50  25 [] () & style.align'self .~ AlignStart
          ] () & layout
  in testGroup "align_self1"
     [ testCase "child 0" $ r^.child 0.area @?= Area (po   0   0) (di  50  25)
     , testCase "child 1" $ r^.child 1.area @?= Area (po   0  25) (di  50  25)
     , testCase "child 2" $ r^.child 2.area @?= Area (po   0  50) (di  50  25)
     ]

test_align_self2 ∷ TestTree
test_align_self2 =
  let r = mkItem 100 100
          [ mkItem  50  25 [] () & style.align'self .~ AlignEnd
          , mkItem  50  25 [] () & style.align'self .~ AlignEnd
          , mkItem  50  25 [] () & style.align'self .~ AlignEnd
          ] () & layout
  in testGroup "align_self2"
     [ testCase "child 0" $ r^.child 0.area @?= Area (po  50   0) (di  50  25)
     , testCase "child 1" $ r^.child 1.area @?= Area (po  50  25) (di  50  25)
     , testCase "child 2" $ r^.child 2.area @?= Area (po  50  50) (di  50  25)
     ]

test_align_self3 ∷ TestTree
test_align_self3 =
  let r = mkItem 100 100
          [ mkItem  50  25 [] () & style.align'self .~ AlignCenter
          , mkItem  50  25 [] () & style.align'self .~ AlignCenter
          , mkItem  50  25 [] () & style.align'self .~ AlignCenter
          ] () & layout
  in testGroup "align_self3"
     [ testCase "child 0" $ r^.child 0.area @?= Area (po  25   0) (di  50  25)
     , testCase "child 1" $ r^.child 1.area @?= Area (po  25  25) (di  50  25)
     , testCase "child 2" $ r^.child 2.area @?= Area (po  25  50) (di  50  25)
     ]

test_align_self4 ∷ TestTree
test_align_self4 =
  let r = mkItem 100 100
          [ mkItem' Nothing (Just 25) [] () & style.align'self .~ AlignStretch
          , mkItem        0       25  [] () & style.align'self .~ AlignStretch
          , mkItem' Nothing (Just 25) [] () & style.align'self .~ AlignStretch
          ] () & layout
  in testGroup "align_self4: stretch works if the align dimension is not set or is 0"
     [ testCase "child 0" $ r^.child 0.area @?= Area (po   0   0) (di 100  25)
     , testCase "child 1" $ r^.child 1.area @?= Area (po   0  25) (di 100  25)
     , testCase "child 2" $ r^.child 2.area @?= Area (po   0  50) (di 100  25)
     ]

test_align_self5 ∷ TestTree
test_align_self5 =
  let r = mkItem 100 100
          [ mkItem  50  25 [] () & style.align'self .~ AlignStretch
          , mkItem  50  50 [] () & style.align'self .~ AlignStretch
          , mkItem  50  25 [] () & style.align'self .~ AlignStretch
          ] () & layout
  in testGroup "align_self5: stretch does not work if the align dimension is set"
     [ testCase "child 0" $ r^.child 0.area @?= Area (po   0   0) (di  50  25)
     , testCase "child 1" $ r^.child 1.area @?= Area (po   0  25) (di  50  50)
     , testCase "child 2" $ r^.child 2.area @?= Area (po   0  75) (di  50  25)
     ]

test_align_self6 ∷ TestTree
test_align_self6 =
  let r = mkItem 100 100
          [ mkItem  50  25 [] () & style.align'self .~ AlignStart
          , mkItem  50  25 [] () & style.align'self .~ AlignCenter
          , mkItem   0  25 [] () & style.align'self .~ AlignStretch
          , mkItem  50  25 [] () & style.align'self .~ AlignEnd
          ] () & layout
  in testGroup "align_self6: potpourri"
     [ testCase "child 0" $ r^.child 0.area @?= Area (po   0   0) (di  50  25)
     , testCase "child 1" $ r^.child 1.area @?= Area (po  25  25) (di  50  25)
     , testCase "child 2" $ r^.child 2.area @?= Area (po   0  50) (di 100  25)
     , testCase "child 2" $ r^.child 3.area @?= Area (po  50  75) (di  50  25)
     ]
-- COMPLETE


-- *
--
test_align_items1 ∷ TestTree
test_align_items1 =
  let r = mkItem 100 100
          [ mkItem  50  25      [] ()
          , mkItem  50  25      [] ()
          , mkItem  50  25      [] ()
          ] () & style.align'items .~ AlignStart
               & layout
  in testGroup "align_items1: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (po   0   0) (di  50  25)
     , testCase "child 1" $ r^.child 1.area @?= Area (po   0  25) (di  50  25)
     , testCase "child 2" $ r^.child 2.area @?= Area (po   0  50) (di  50  25)
     ]

test_align_items2 ∷ TestTree
test_align_items2 =
  let r = mkItem 100 100
          [ mkItem  50  25      [] ()
          , mkItem  50  25      [] ()
          , mkItem  50  25      [] ()
          ] () & style.align'items .~ AlignEnd
               & layout
  in testGroup "align_items2: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (po  50   0) (di  50  25)
     , testCase "child 1" $ r^.child 1.area @?= Area (po  50  25) (di  50  25)
     , testCase "child 2" $ r^.child 2.area @?= Area (po  50  50) (di  50  25)
     ]

test_align_items3 ∷ TestTree
test_align_items3 =
  let r = mkItem 100 100
          [ mkItem  50  25      [] ()
          , mkItem  50  25      [] ()
          , mkItem  50  25      [] ()
          ] () & style.align'items .~ AlignCenter
               & layout
  in testGroup "align_items3: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (po  25   0) (di  50  25)
     , testCase "child 1" $ r^.child 1.area @?= Area (po  25  25) (di  50  25)
     , testCase "child 2" $ r^.child 2.area @?= Area (po  25  50) (di  50  25)
     ]

test_align_items4 ∷ TestTree
test_align_items4 =
  let r = mkItem 100 100
          [ mkItem       50        25  [] ()
          , mkItem        0        25  [] ()
          , mkItem' Nothing  (Just 25) [] ()
          ] () & style.align'items .~ AlignStretch
               & layout
  in testGroup "align_items4: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (po   0   0) (di  50  25)
     , testCase "child 1" $ r^.child 1.area @?= Area (po   0  25) (di 100  25)
     , testCase "child 2" $ r^.child 2.area @?= Area (po   0  50) (di 100  25)
     ]

test_align_items5 ∷ TestTree
test_align_items5 =
  let r = mkItem 100 100
          [ mkItem 50 25 [] ()
          , mkItem 50 25 [] () & style.align'self .~ AlignStart
          , mkItem 50 25 [] () & style.align'self .~ AlignAuto
          , mkItem 50 25 [] () & style.align'self .~ AlignEnd
          ] () & style.align'items .~ AlignCenter
               & layout
  in testGroup "align_items5: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (po  25   0) (di  50  25)
     , testCase "child 1" $ r^.child 1.area @?= Area (po   0  25) (di  50  25)
     , testCase "child 2" $ r^.child 2.area @?= Area (po  25  50) (di  50  25)
     , testCase "child 3" $ r^.child 3.area @?= Area (po  50  75) (di  50  25)
     ]
-- COMPLETE


-- *
--
test_align_content1 ∷ TestTree
test_align_content1 =
  let r = mkItem 200 120
          [ mkItem  50  50      [] ()
          , mkItem  60  50      [] ()
          , mkItem  40  50      [] ()
          ] () & style.wrap          .~ Wrap
               & style.align'content .~ AlignStart
               & layout
  in testGroup "align_content1: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (po   0   0) (di  50  50)
     , testCase "child 1" $ r^.child 1.area @?= Area (po   0  50) (di  60  50)
     , testCase "child 2" $ r^.child 2.area @?= Area (po  60   0) (di  40  50)
     ]

test_align_content2 ∷ TestTree
test_align_content2 =
  let r = mkItem 200 120
          [ mkItem  50  50      [] ()
          , mkItem  60  50      [] ()
          , mkItem  40  50      [] ()
          ] () & style.wrap          .~ Wrap
               & style.align'content .~ AlignCenter
               & layout
  in testGroup "align_content2: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (po  50   0) (di  50  50)
     , testCase "child 1" $ r^.child 1.area @?= Area (po  50  50) (di  60  50)
     , testCase "child 2" $ r^.child 2.area @?= Area (po 110   0) (di  40  50)
     ]

test_align_content3 ∷ TestTree
test_align_content3 =
  let r = mkItem 200 120
          [ mkItem  50  50      [] ()
          , mkItem  60  50      [] ()
          , mkItem  40  50      [] ()
          ] () & style.wrap          .~ Wrap
               & style.align'content .~ AlignEnd
               & layout
  in testGroup "align_content3: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (po 100   0) (di  50  50)
     , testCase "child 1" $ r^.child 1.area @?= Area (po 100  50) (di  60  50)
     , testCase "child 2" $ r^.child 2.area @?= Area (po 160   0) (di  40  50)
     ]

test_align_content4 ∷ TestTree
test_align_content4 =
  let r = mkItem 200 120
          [ mkItem  50  50      [] ()
          , mkItem  60  50      [] ()
          , mkItem  40  50      [] ()
          ] () & style.wrap          .~ Wrap
               & style.align'content .~ AlignSpaceBetween
               & layout
  in testGroup "align_content4: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (po   0   0) (di  50  50)
     , testCase "child 1" $ r^.child 1.area @?= Area (po   0  50) (di  60  50)
     , testCase "child 2" $ r^.child 2.area @?= Area (po 160   0) (di  40  50)
     ]

test_align_content5 ∷ TestTree
test_align_content5 =
  let r = mkItem 200 120
          [ mkItem  50  50      [] ()
          , mkItem  60  50      [] ()
          , mkItem  40  50      [] ()
          ] () & style.wrap          .~ Wrap
               & style.align'content .~ AlignSpaceAround
               & layout
  in testGroup "align_content5: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (po  25   0) (di  50  50)
     , testCase "child 1" $ r^.child 1.area @?= Area (po  25  50) (di  60  50)
     , testCase "child 2" $ r^.child 2.area @?= Area (po 135   0) (di  40  50)
     ]

test_align_content6 ∷ TestTree
test_align_content6 =
  let r = mkItem 250 120
          [ mkItem  50  50      [] ()
          , mkItem  60  50      [] ()
          , mkItem  40  50      [] ()
          ] () & style.wrap          .~ Wrap
               & style.align'content .~ AlignSpaceEvenly
               & layout
  in testGroup "align_content6: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (po  50   0) (di  50  50)
     , testCase "child 1" $ r^.child 1.area @?= Area (po  50  50) (di  60  50)
     , testCase "child 2" $ r^.child 2.area @?= Area (po 160   0) (di  40  50)
     ]
-- COMPLETE


-- *
--
-- test_default1 -- too simple to test
--
test_default_values2 ∷ TestTree
test_default_values2 =
  let r = mkItem 200 200
          [ mkItem' (Just 100) Nothing   [] ()
          , mkItem' Nothing   (Just 100) [] ()
          , mkItem' Nothing    Nothing   [] ()
          ] () & style.direction .~ DirColumn
               & layout
  in testGroup "default_values2: if the width/height property isn't set on a child, it's frame size defaults to 0 for the main axis and the parent's size for the minor axis"
     [ testCase "child 0" $ r^.child 0.area.area'b @?= di 100   0
     , testCase "child 1" $ r^.child 1.area.area'b @?= di 200 100
     , testCase "child 2" $ r^.child 2.area.area'b @?= di 200   0
     ]

test_default_values3 ∷ TestTree
test_default_values3 =
  let r = mkItem 200 200
          [ mkItem' (Just 100) Nothing   [] ()
          , mkItem' Nothing   (Just 100) [] ()
          , mkItem' Nothing    Nothing   [] ()
          ] () & style.direction .~ DirRow
               & layout
  in testGroup "default_values3: "
     [ testCase "child 0" $ r^.child 0.area.area'b @?= di 100 200
     , testCase "child 1" $ r^.child 1.area.area'b @?= di   0 100
     , testCase "child 2" $ r^.child 2.area.area'b @?= di   0 200
     ]
-- COMPLETE


-- *
--
test_justify_content1 ∷ TestTree
test_justify_content1 =
  let r = mkItem 100 300
          [ mkItem 50 100 [] ()
          , mkItem 50 100 [] ()
          ] () & style.justify'content .~ AlignCenter
               & layout
  in testGroup "justify_content1: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (po   0  50) (di  50 100)
     , testCase "child 1" $ r^.child 1.area @?= Area (po   0 150) (di  50 100)
     ]

test_justify_content2 ∷ TestTree
test_justify_content2 =
  let r = mkItem 100 300
          [ mkItem 50 100 [] ()
          , mkItem 50 100 [] ()
          ] () & style.justify'content .~ AlignStart
               & layout
  in testGroup "justify_content2: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (po   0   0) (di  50 100)
     , testCase "child 1" $ r^.child 1.area @?= Area (po   0 100) (di  50 100)
     ]

test_justify_content3 ∷ TestTree
test_justify_content3 =
  let r = mkItem 100 300
          [ mkItem 50 100 [] ()
          , mkItem 50 100 [] ()
          ] () & style.justify'content .~ AlignEnd
               & layout
  in testGroup "justify_content3: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (po   0 100) (di  50 100)
     , testCase "child 1" $ r^.child 1.area @?= Area (po   0 200) (di  50 100)
     ]

test_justify_content4 ∷ TestTree
test_justify_content4 =
  let r = mkItem 100 300
          [ mkItem 50 100 [] ()
          , mkItem 50 100 [] ()
          ] () & style.justify'content .~ AlignSpaceBetween
               & layout
  in testGroup "justify_content4: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (po   0   0) (di  50 100)
     , testCase "child 1" $ r^.child 1.area @?= Area (po   0 200) (di  50 100)
     ]

test_justify_content5 ∷ TestTree
test_justify_content5 =
  let r = mkItem 100 300
          [ mkItem 50 50 [] ()
          , mkItem 50 50 [] ()
          , mkItem 50 50 [] ()
          ] () & style.justify'content .~ AlignSpaceBetween
               & layout
  in testGroup "justify_content5: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (po   0   0) (di  50 50)
     , testCase "child 1" $ r^.child 1.area @?= Area (po   0 125) (di  50 50)
     , testCase "child 2" $ r^.child 2.area @?= Area (po   0 250) (di  50 50)
     ]

test_justify_content6 ∷ TestTree
test_justify_content6 =
  let r = mkItem 100 300
          [ mkItem 50 100 [] ()
          , mkItem 50 100 [] ()
          ] () & style.justify'content .~ AlignSpaceAround
               & layout
  in testGroup "justify_content6: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (po   0  25) (di  50 100)
     , testCase "child 1" $ r^.child 1.area @?= Area (po   0 175) (di  50 100)
     ]

test_justify_content7 ∷ TestTree
test_justify_content7 =
  let r = mkItem 100 300
          [ mkItem 50 50 [] ()
          , mkItem 50 50 [] ()
          , mkItem 50 50 [] ()
          ] () & style.justify'content .~ AlignSpaceAround
               & layout
  in testGroup "justify_content7: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (po   0  25) (di  50 50)
     , testCase "child 1" $ r^.child 1.area @?= Area (po   0 125) (di  50 50)
     , testCase "child 2" $ r^.child 2.area @?= Area (po   0 225) (di  50 50)
     ]

test_justify_content8 ∷ TestTree
test_justify_content8 =
  let r = mkItem 100 300
          [ mkItem 50 105 [] ()
          , mkItem 50 105 [] ()
          ] () & style.justify'content .~ AlignSpaceEvenly
               & layout
  in testGroup "justify_content8: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (po   0  30) (di  50 105)
     , testCase "child 1" $ r^.child 1.area @?= Area (po   0 165) (di  50 105)
     ]

test_justify_content9 ∷ TestTree
test_justify_content9 =
  let r = mkItem 100 300
          [ mkItem 50 40 [] ()
          , mkItem 50 40 [] ()
          , mkItem 50 40 [] ()
          ] () & style.justify'content .~ AlignSpaceEvenly
               & layout
  in testGroup "justify_content9: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (po   0  45) (di  50 40)
     , testCase "child 1" $ r^.child 1.area @?= Area (po   0 130) (di  50 40)
     , testCase "child 2" $ r^.child 2.area @?= Area (po   0 215) (di  50 40)
     ]

test_justify_content10 ∷ TestTree
test_justify_content10 =
  let r = mkItem 100 300
          [ mkItem 50 100 [] ()
          , mkItem 50 100 [] ()
          ] () & style.direction       .~ DirColumnReverse
               & style.justify'content .~ AlignCenter
               & layout
  in testGroup "justify_content10: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (po   0 150) (di  50 100)
     , testCase "child 1" $ r^.child 1.area @?= Area (po   0  50) (di  50 100)
     ]

test_justify_content11 ∷ TestTree
test_justify_content11 =
  let r = mkItem 100 300
          [ mkItem 50 100 [] ()
          , mkItem 50 100 [] ()
          ] () & style.direction       .~ DirColumnReverse
               & style.justify'content .~ AlignStart
               & layout
  in testGroup "justify_content11: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (po   0 200) (di  50 100)
     , testCase "child 1" $ r^.child 1.area @?= Area (po   0 100) (di  50 100)
     ]

test_justify_content12 ∷ TestTree
test_justify_content12 =
  let r = mkItem 100 300
          [ mkItem 50 100 [] ()
          , mkItem 50 100 [] ()
          ] () & style.direction       .~ DirColumnReverse
               & style.justify'content .~ AlignEnd
               & layout
  in testGroup "justify_content12: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (po   0 100) (di  50 100)
     , testCase "child 1" $ r^.child 1.area @?= Area (po   0   0) (di  50 100)
     ]

test_justify_content13 ∷ TestTree
test_justify_content13 =
  let r = mkItem 100 300
          [ mkItem 50 100 [] ()
          , mkItem 50 100 [] ()
          ] () & style.direction       .~ DirColumnReverse
               & style.justify'content .~ AlignSpaceBetween
               & layout
  in testGroup "justify_content13: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (po   0 200) (di  50 100)
     , testCase "child 1" $ r^.child 1.area @?= Area (po   0   0) (di  50 100)
     ]

test_justify_content14 ∷ TestTree
test_justify_content14 =
  let r = mkItem 100 300
          [ mkItem 50 100 [] ()
          , mkItem 50 100 [] ()
          ] () & style.direction       .~ DirColumnReverse
               & style.justify'content .~ AlignSpaceAround
               & layout
  in testGroup "justify_content14: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (po   0 175) (di  50 100)
     , testCase "child 1" $ r^.child 1.area @?= Area (po   0  25) (di  50 100)
     ]

test_justify_content15 ∷ TestTree
test_justify_content15 =
  let r = mkItem 100 300
          [ mkItem 50 105 [] ()
          , mkItem 50 105 [] ()
          ] () & style.direction       .~ DirColumnReverse
               & style.justify'content .~ AlignSpaceEvenly
               & layout
  in testGroup "justify_content15: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (po   0 165) (di  50 105)
     , testCase "child 1" $ r^.child 1.area @?= Area (po   0  30) (di  50 105)
     ]

test_justify_content16 ∷ TestTree
test_justify_content16 =
  let modes = [AlignStart, AlignCenter, AlignEnd, AlignSpaceBetween, AlignSpaceAround, AlignSpaceEvenly]
      r mode
        = mkItem 100 100
          [ mkItem 50 50 [] ()
          , mkItem 50 50 [] ()
          ] () & style.justify'content .~ mode
               & layout
  in testGroup "justify_content16: the `justify_content' property is ignored when the children fill up all the space" $
     flip concatMap modes
       (\mode→
          [ testCase (printf "child 0 / %s" $ show mode) $ (r mode)^.child 0.area @?= Area (po   0   0) (di  50  50)
          , testCase (printf "child 1 / %s" $ show mode) $ (r mode)^.child 1.area @?= Area (po   0  50) (di  50  50)
          ])

test_justify_content17 ∷ TestTree
test_justify_content17 =
  let modes = [AlignStart, AlignCenter, AlignEnd, AlignSpaceBetween, AlignSpaceAround, AlignSpaceEvenly]
      r mode
        = mkItem 100 100
          [ mkItem 50 100 [] ()
          , mkItem 50 100 [] ()
          , mkItem 50 100 [] ()
          , mkItem 50 100 [] ()
          ] () & style.justify'content .~ mode
               & layout
  in testGroup "justify_content17: the `justify_content' property is ignored when the children fill up all the space" $
     flip concatMap modes
       (\mode→
          [ testCase (printf "child 0 / %s" $ show mode) $ (r mode)^.child 0.area @?= Area (po   0   0) (di  50  25)
          , testCase (printf "child 1 / %s" $ show mode) $ (r mode)^.child 1.area @?= Area (po   0  25) (di  50  25)
          , testCase (printf "child 2 / %s" $ show mode) $ (r mode)^.child 2.area @?= Area (po   0  50) (di  50  25)
          , testCase (printf "child 3 / %s" $ show mode) $ (r mode)^.child 3.area @?= Area (po   0  75) (di  50  25)
          ])

test_justify_content18 ∷ TestTree
test_justify_content18 =
  let modes = [AlignStart, AlignCenter, AlignEnd, AlignSpaceBetween, AlignSpaceAround, AlignSpaceEvenly]
      r mode
        = mkItem 100 100
          [ mkItem 50 20 [] ()
          , mkItem 50 20 [] () & style.grow .~ 1
          , mkItem 50 20 [] ()
          ] () & style.justify'content .~ mode
               & layout
  in testGroup "justify_content18: the `justify_content' property is ignored when there are flexible children" $
     flip concatMap modes
       (\mode→
          [ testCase (printf "child 0 / %s" $ show mode) $ (r mode)^.child 0.area @?= Area (po   0   0) (di  50  20)
          , testCase (printf "child 1 / %s" $ show mode) $ (r mode)^.child 1.area @?= Area (po   0  20) (di  50  60)
          , testCase (printf "child 2 / %s" $ show mode) $ (r mode)^.child 2.area @?= Area (po   0  80) (di  50  20)
          ])
-- COMPLETE

{-# LANGUAGE TemplateHaskell, UnicodeSyntax #-}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-type-defaults #-}

module FlexTest where

import           Control.Lens
import           Control.Lens.TH
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


-- * Dummy Flex
--
data FreeFlex =
  FF
  { _ffGeo      ∷ Geo
  , _ffSize     ∷ Di (Maybe Double)
  , _ffChildren ∷ [FreeFlex]
  , _ffArea     ∷ Area'LU Double
  }
makeLenses ''FreeFlex

instance Flex (FreeFlex) where
  geo      = ffGeo
  size     = ffSize
  children = ffChildren
  area     = ffArea

leaf ∷ Double → Double → FreeFlex
leaf w h     = node' (Just w) (Just h) []

leaf' ∷ Maybe Double → Maybe Double → FreeFlex
leaf' w h    = node' w h []

node ∷ Double → Double → [FreeFlex] → FreeFlex
node w h cs  = node' (Just w) (Just h) cs

node' ∷ Maybe Double → Maybe Double → [FreeFlex] → FreeFlex
node' w h cs = FF mempty (Di $ V2 w h) cs mempty


-- *
--
test_scene ∷ TestTree
test_scene =
  let r = node 400 200
          [ leaf 50  50
          , node' Nothing Nothing
            [ leaf 50  100
            , leaf 100  50
            ] & geo.direction       .~ DirRow
              & geo.align'content   .~ AlignStart
              & geo.grow            .~ 1
          , leaf 200  50
          ] & geo.direction       .~ DirColumn
            & geo.align'content   .~ AlignStart
            & geo.grow            .~ 1
            & layout
  in testGroup "scene"
     [ testCase "child 0"   $ r^.child 0.area         @?= Area (mkLU   0   0) (mkSize  50  50)
     , testCase "child 1"   $ r^.child 1.area         @?= Area (mkLU   0  50) (mkSize 400 100)
     , testCase "child 1.0" $ r^.child 1.child 0.area @?= Area (mkLU   0   0) (mkSize  50 100)
     , testCase "child 1.1" $ r^.child 1.child 1.area @?= Area (mkLU  50   0) (mkSize 100  50)
     , testCase "child 2"   $ r^.child 2.area         @?= Area (mkLU   0 150) (mkSize 200  50)
     ]


-- *
--
test_grow1 ∷ TestTree
test_grow1 =
  let r = node 60 240
          [ leaf 60 30 & geo.grow .~ 0
          , leaf 60  0 & geo.grow .~ 1
          , leaf 60  0 & geo.grow .~ 2
          ] & layout
  in testGroup "grow1: three children grow proportionally to property"
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU   0   0) (mkSize 60  30)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU   0  30) (mkSize 60  70)
     , testCase "child 2" $ r^.child 2.area @?= Area (mkLU   0 100) (mkSize 60 140)
     ]

test_grow2 ∷ TestTree
test_grow2 =
  let r = node 100 100
          [ leaf 100 20 & geo.grow .~ 1
          , leaf 100 20 & geo.grow .~ 0
          , leaf 100 20
          ] & layout
  in testGroup "grow2: only grow if property set to 1"
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU   0   0) (mkSize 100  60)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU   0  60) (mkSize 100  20)
     , testCase "child 2" $ r^.child 2.area @?= Area (mkLU   0  80) (mkSize 100  20)
     ]

test_grow3 ∷ TestTree
test_grow3 =
  let r = node 100 100
          [ leaf 100 50 & geo.grow .~ 2
          , leaf 100 50 & geo.grow .~ 3
          ] & layout
  in testGroup "grow3: growth has no effect if parent already full"
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU   0   0) (mkSize 100  50)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU   0  50) (mkSize 100  50)
     ]

test_grow4 ∷ TestTree
test_grow4 =
  let r = node 100 100
          [ leaf 100 25
          , leaf 100 25
          ] & geo.grow .~ 2
            & layout
  in testGroup "grow4: parent growth property has no effect on children"
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU   0   0) (mkSize 100  25)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU   0  25) (mkSize 100  25)
     ]

test_grow5 ∷ TestTree
test_grow5 =
  let r = node 100 100
          [ leaf 100 25 & geo.grow .~ 1
          ] & layout
  in testGroup "grow5: single child fills parent"
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU   0   0) (mkSize 100  100)
     ]

test_grow6 ∷ TestTree
test_grow6 =
  let r = node 100 100
          [ leaf 100 45 & geo.grow .~ 1
          , leaf 100 45 & geo.grow .~ 1
          ] & layout
  in testGroup "grow6: two undersized children fill at equal rate"
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU   0   0) (mkSize 100   50)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU   0  50) (mkSize 100   50)
     ]

test_grow7 ∷ TestTree
test_grow7 =
  let r = node 500 600
          [ leaf 250  0 & geo.grow .~ 1
          , leaf 250 50 & geo.grow .~ 1
          , leaf 250  0
          , leaf 250  0 & geo.grow .~ 1
          , leaf 250  0
          ] & layout
  in testGroup "grow7: sizes of flexible items should be ignored when growing"
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU   0   0) (mkSize 250 200)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU   0 200) (mkSize 250 200)
     , testCase "child 2" $ r^.child 2.area @?= Area (mkLU   0 400) (mkSize 250   0)
     , testCase "child 3" $ r^.child 3.area @?= Area (mkLU   0 400) (mkSize 250 200)
     , testCase "child 4" $ r^.child 4.area @?= Area (mkLU   0 600) (mkSize 250   0)
     ]

-- COMPLETE


-- *
--
test_wrap1 ∷ TestTree
test_wrap1 =
  let r = node 100 300
          [ leaf 100 150
          , leaf 100 150
          , leaf 100 150
          , leaf 100 150
          ] & geo.wrap .~ NoWrap
            & layout
  in testGroup "wrap1: NoWrap doesn't enable wrapping"
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU   0   0) (mkSize 100  75)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU   0  75) (mkSize 100  75)
     , testCase "child 2" $ r^.child 2.area @?= Area (mkLU   0 150) (mkSize 100  75)
     , testCase "child 3" $ r^.child 3.area @?= Area (mkLU   0 225) (mkSize 100  75)
     ]

test_wrap2 ∷ TestTree
test_wrap2 =
  let r = node 100 300
          [ leaf 50 150
          , leaf 50 150
          , leaf 50 150
          , leaf 50 150
          ] & geo.wrap          .~ Wrap
            & geo.align'content .~ AlignStart
            & layout
  in testGroup "wrap2: four non-stretching children wrap across two rows"
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU   0   0) (mkSize  50 150)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU   0 150) (mkSize  50 150)
     , testCase "child 2" $ r^.child 2.area @?= Area (mkLU  50   0) (mkSize  50 150)
     , testCase "child 3" $ r^.child 3.area @?= Area (mkLU  50 150) (mkSize  50 150)
     ]

test_wrap3 ∷ TestTree
test_wrap3 =
  let r = node 120 120
          [ leaf 50 50
          , leaf 50 50
          , leaf 50 50
          ] & geo.wrap          .~ Wrap
            & geo.align'content .~ AlignStart
            & layout
  in testGroup "wrap3: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU   0   0) (mkSize  50  50)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU   0  50) (mkSize  50  50)
     , testCase "child 2" $ r^.child 2.area @?= Area (mkLU  50   0) (mkSize  50  50)
     ]

test_wrap4 ∷ TestTree
test_wrap4 =
  let r = node 120 120
          [ leaf 25 50
          , leaf 25 50
          , leaf 25 50
          , leaf 25 50
          , leaf 25 50
          , leaf 25 50
          ] & geo.wrap          .~ Wrap
            & geo.align'content .~ AlignStart
            & layout
  in testGroup "wrap4: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU   0   0) (mkSize  25  50)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU   0  50) (mkSize  25  50)
     , testCase "child 2" $ r^.child 2.area @?= Area (mkLU  25   0) (mkSize  25  50)
     , testCase "child 3" $ r^.child 3.area @?= Area (mkLU  25  50) (mkSize  25  50)
     , testCase "child 4" $ r^.child 4.area @?= Area (mkLU  50   0) (mkSize  25  50)
     , testCase "child 5" $ r^.child 5.area @?= Area (mkLU  50  50) (mkSize  25  50)
     ]

test_wrap5 ∷ TestTree
test_wrap5 =
  let r = node 120 120
          [ leaf 50 50
          , leaf 50 50
          , leaf 50 50
          ] & geo.wrap            .~ Wrap
            & geo.justify'content .~ AlignEnd
            & geo.align'content   .~ AlignStart
            & layout
  in testGroup "wrap5: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU   0  20) (mkSize  50  50)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU   0  70) (mkSize  50  50)
     , testCase "child 2" $ r^.child 2.area @?= Area (mkLU  50  70) (mkSize  50  50)
     ]

test_wrap6 ∷ TestTree
test_wrap6 =
  let r = node 120 120
          [ leaf 50 50
          , leaf 50 50
          , leaf 50 50
          ] & geo.wrap            .~ Wrap
            & geo.justify'content .~ AlignCenter
            & geo.align'content   .~ AlignStart
            & layout
  in testGroup "wrap6: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU   0  10) (mkSize  50  50)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU   0  60) (mkSize  50  50)
     , testCase "child 2" $ r^.child 2.area @?= Area (mkLU  50  35) (mkSize  50  50)
     ]

test_wrap7 ∷ TestTree
test_wrap7 =
  let r = node 120 120
          [ leaf 50 50
          , leaf 50 50
          , leaf 50 50
          ] & geo.wrap            .~ Wrap
            & geo.justify'content .~ AlignSpaceAround
            & geo.align'content   .~ AlignStart
            & layout
  in testGroup "wrap7: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU   0   5) (mkSize  50  50)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU   0  65) (mkSize  50  50)
     , testCase "child 2" $ r^.child 2.area @?= Area (mkLU  50  35) (mkSize  50  50)
     ]

test_wrap8 ∷ TestTree
test_wrap8 =
  let r = node 120 120
          [ leaf 50 50
          , leaf 50 50
          , leaf 50 50
          ] & geo.wrap            .~ Wrap
            & geo.justify'content .~ AlignSpaceBetween
            & geo.align'content   .~ AlignStart
            & layout
  in testGroup "wrap8: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU   0   0) (mkSize  50  50)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU   0  70) (mkSize  50  50)
     , testCase "child 2" $ r^.child 2.area @?= Area (mkLU  50   0) (mkSize  50  50)
     ]

test_wrap9 ∷ TestTree
test_wrap9 =
  let r = node 120 120
          [ leaf 50 50
          , leaf 50 50 & geo.grow .~ 1
          , leaf 50 50 & geo.grow .~ 1
          , leaf 50 50
          ] & geo.wrap            .~ Wrap
            & geo.align'content   .~ AlignStart
            & layout
  in testGroup "wrap9: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU   0   0) (mkSize  50  50)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU   0  50) (mkSize  50  70)
     , testCase "child 2" $ r^.child 2.area @?= Area (mkLU  50   0) (mkSize  50  70)
     , testCase "child 3" $ r^.child 3.area @?= Area (mkLU  50  70) (mkSize  50  50)
     ]

test_wrap10 ∷ TestTree
test_wrap10 =
  let r = node 120 120
          [ leaf 50 40
          , leaf 70 30
          , leaf 60 40
          , leaf 40 50
          , leaf 50 60
          ] & geo.wrap            .~ Wrap
            & geo.align'items     .~ AlignStart
            & layout
  in testGroup "wrap10: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU   0   0) (mkSize  50  40)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU   0  40) (mkSize  70  30)
     , testCase "child 2" $ r^.child 2.area @?= Area (mkLU   0  70) (mkSize  60  40)
     , testCase "child 3" $ r^.child 3.area @?= Area (mkLU  70   0) (mkSize  40  50)
     , testCase "child 4" $ r^.child 4.area @?= Area (mkLU  70  50) (mkSize  50  60)
     ]

-- XXX: ⊥
test_wrap11 ∷ TestTree
test_wrap11 =
  let r = node 120 120
          [ leaf 50 40
          , leaf 70 30
          , leaf 60 40
          , leaf 40 50
          , leaf 50 60
          ] & geo.wrap            .~ Wrap
            & geo.align'items     .~ AlignCenter
            & layout
  in testGroup "wrap11: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU  10   0) (mkSize  50  40)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU   0  40) (mkSize  70  30)
     , testCase "child 2" $ r^.child 2.area @?= Area (mkLU   5  70) (mkSize  60  40)
     , testCase "child 3" $ r^.child 3.area @?= Area (mkLU  75   0) (mkSize  40  50)
     , testCase "child 4" $ r^.child 4.area @?= Area (mkLU  70  50) (mkSize  50  60)
     ]

test_wrap12 ∷ TestTree
test_wrap12 =
  let r = node 120 120
          [ leaf 50 40
          , leaf 70 30
          , leaf 60 40
          , leaf 40 50
          , leaf 50 60
          ] & geo.wrap            .~ Wrap
            & geo.align'items     .~ AlignEnd
            & layout
  in testGroup "wrap12: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU  20   0) (mkSize  50  40)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU   0  40) (mkSize  70  30)
     , testCase "child 2" $ r^.child 2.area @?= Area (mkLU  10  70) (mkSize  60  40)
     , testCase "child 3" $ r^.child 3.area @?= Area (mkLU  80   0) (mkSize  40  50)
     , testCase "child 4" $ r^.child 4.area @?= Area (mkLU  70  50) (mkSize  50  60)
     ]

test_wrap13 ∷ TestTree
test_wrap13 =
  let r = node 120 120
          [ leaf 50 40 & geo.align'self .~ AlignEnd
          , leaf 70 30
          , leaf 60 40 & geo.align'self .~ AlignCenter
          , leaf 40 50 & geo.align'self .~ AlignStart
          , leaf 50 60
          , leaf 10 10 & geo.align'self .~ AlignEnd
          ] & geo.wrap .~ Wrap
            & layout
  in testGroup "wrap13: potpourri"
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU  20   0) (mkSize  50  40)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU   0  40) (mkSize  70  30)
     , testCase "child 2" $ r^.child 2.area @?= Area (mkLU   5  70) (mkSize  60  40)
     , testCase "child 3" $ r^.child 3.area @?= Area (mkLU  70   0) (mkSize  40  50)
     , testCase "child 4" $ r^.child 4.area @?= Area (mkLU  70  50) (mkSize  50  60)
     , testCase "child 5" $ r^.child 5.area @?= Area (mkLU 110 110) (mkSize  10  10)
     ]

test_wrap14 ∷ TestTree
test_wrap14 =
  let r = node 120 120
          [ leaf 50 50
          , leaf 50 50
          , leaf 50 50
          ] & geo.wrap            .~ ReverseWrap
            & geo.align'content   .~ AlignStart
            & layout
  in testGroup "wrap14: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU  70   0) (mkSize  50  50)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU  70  50) (mkSize  50  50)
     , testCase "child 2" $ r^.child 2.area @?= Area (mkLU  20   0) (mkSize  50  50)
     ]

test_wrap15 ∷ TestTree
test_wrap15 =
  let r = node 120 120
          [ leaf 25 50
          , leaf 25 50
          , leaf 25 50
          , leaf 25 50
          , leaf 25 50
          , leaf 25 50
          ] & geo.wrap          .~ ReverseWrap
            & geo.align'content .~ AlignStart
            & layout
  in testGroup "wrap15: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU  95   0) (mkSize  25  50)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU  95  50) (mkSize  25  50)
     , testCase "child 2" $ r^.child 2.area @?= Area (mkLU  70   0) (mkSize  25  50)
     , testCase "child 3" $ r^.child 3.area @?= Area (mkLU  70  50) (mkSize  25  50)
     , testCase "child 4" $ r^.child 4.area @?= Area (mkLU  45   0) (mkSize  25  50)
     , testCase "child 5" $ r^.child 5.area @?= Area (mkLU  45  50) (mkSize  25  50)
     ]

test_wrap16 ∷ TestTree
test_wrap16 =
  let r = node 120 120
          [ leaf 20 50
          , leaf 20 50
          , leaf 20 50
          ] & geo.direction       .~ DirColumn
            & geo.wrap            .~ Wrap
            & geo.align'content   .~ AlignStretch
            & layout
  in testGroup "wrap16: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU   0   0) (mkSize  20  50)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU   0  50) (mkSize  20  50)
     , testCase "child 2" $ r^.child 2.area @?= Area (mkLU  60   0) (mkSize  20  50)
     ]

test_wrap17 ∷ TestTree
test_wrap17 =
  let r = node 120 120
          [ leaf 20 50
          , leaf 20 50
          , leaf 20 50
          , leaf 20 50
          , leaf 20 50
          ] & geo.direction       .~ DirColumn
            & geo.wrap            .~ Wrap
            & geo.align'content   .~ AlignStretch
            & layout
  in testGroup "wrap17: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU   0   0) (mkSize  20  50)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU   0  50) (mkSize  20  50)
     , testCase "child 2" $ r^.child 2.area @?= Area (mkLU  40   0) (mkSize  20  50)
     , testCase "child 3" $ r^.child 3.area @?= Area (mkLU  40  50) (mkSize  20  50)
     , testCase "child 4" $ r^.child 4.area @?= Area (mkLU  80   0) (mkSize  20  50)
     ]
-- COMPLETE


-- *
--
test_basis1 ∷ TestTree
test_basis1 =
  let r = node 100 100
          [ leaf' (Just 100) Nothing & geo.basis .~ 60
          , leaf        100  40
          ] & layout
  in testGroup "basis1: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU   0   0) (mkSize 100  60)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU   0  60) (mkSize 100  40)
     ]

test_basis2 ∷ TestTree
test_basis2 =
  let r = node 100 100
          [ leaf 100 40 & geo.basis .~ 60
          , leaf 100 40
          ] & layout
  in testGroup "basis2: the basis attribute has priority over width/height"
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU   0   0) (mkSize 100  60)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU   0  60) (mkSize 100  40)
     ]

test_basis3 ∷ TestTree
test_basis3 =
  let r = node 100 100
          [ leaf' (Just 100) Nothing & geo.basis .~ (-60)
          , leaf        100  40
          ] & layout
  in testGroup "basis3: the basis attribute is ignored if negative"
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU   0   0) (mkSize 100   0)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU   0   0) (mkSize 100  40)
     ]

test_basis4 ∷ TestTree
test_basis4 =
  let r = node 100 100
          [ leaf 100 40 & geo.basis .~ (-60)
          , leaf 100 40
          ] & layout
  in testGroup "basis4: the basis attribute is ignored if negative"
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU   0   0) (mkSize 100  40)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU   0  40) (mkSize 100  40)
     ]

test_basis5 ∷ TestTree
test_basis5 =
  let r = node 100 100
          [ leaf 100 40 & geo.basis .~ 0
          , leaf 100 40
          ] & layout
  in testGroup "basis5: the basis attribute is ignored if 0"
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU   0   0) (mkSize 100  40)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU   0  40) (mkSize 100  40)
     ]
-- COMPLETE


-- *
--
test_order1 ∷ TestTree
test_order1 =
  let r = node 200 200
          [ leaf 50 50 & geo.order .~ Just 1
          , leaf 50 50 & geo.order .~ Just 3
          , leaf 50 50 & geo.order .~ Just 2
          ] & layout
  in testGroup "order1: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU   0   0) (mkSize  50  50)
     , expectFail $
       testCase "child 1" $ r^.child 1.area @?= Area (mkLU   0 100) (mkSize  50  50)
     , expectFail $
       testCase "child 2" $ r^.child 2.area @?= Area (mkLU   0  50) (mkSize  50  50)
     ]

test_order2 ∷ TestTree
test_order2 =
  let r = node 200 200
          [ leaf 50 50 & geo.order .~ Just 2
          , leaf 50 50 & geo.order .~ Just 3
          , leaf 50 50 & geo.order .~ Just 1
          ] & geo.direction .~ DirColumnReverse
            & layout
  in testGroup "order2: "
     [ expectFail $
       testCase "child 0" $ r^.child 0.area @?= Area (mkLU   0 100) (mkSize  50  50)
     , expectFail $
       testCase "child 1" $ r^.child 1.area @?= Area (mkLU   0  50) (mkSize  50  50)
     , expectFail $
       testCase "child 2" $ r^.child 2.area @?= Area (mkLU   0 150) (mkSize  50  50)
     ]

test_order3 ∷ TestTree
test_order3 =
  let r = node 200 200
          [ leaf 50 50
          , leaf 50 50 & geo.order .~ Just (-1)
          , leaf 50 50
          ] & geo.direction .~ DirColumnReverse
            & layout
  in testGroup "order3: "
     [ expectFail $
       testCase "child 0" $ r^.child 0.area @?= Area (mkLU   0  50) (mkSize  50  50)
     , expectFail $
       testCase "child 1" $ r^.child 1.area @?= Area (mkLU   0   0) (mkSize  50  50)
     , expectFail $
       testCase "child 2" $ r^.child 2.area @?= Area (mkLU   0 100) (mkSize  50  50)
     ]
--
-- test_order4 skipped:
--  > this test ensures that the insertion order of the children is preserved when they get re-ordered during layout
--


-- *
--
test_margin1 ∷ TestTree
test_margin1 =
  let r = node 100 100
          [ leaf  25 25
          , leaf  25 25 & geo.margin .~ LRTB 15 15 10 10
          , leaf  25 25
          ] & geo.align'items     .~ AlignStart
            & geo.justify'content .~ AlignStart
            & layout
  in testGroup "margin1: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU   0   0) (mkSize  25  25)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU  15  35) (mkSize  25  25)
     , testCase "child 2" $ r^.child 2.area @?= Area (mkLU   0  70) (mkSize  25  25)
     ]

test_margin2 ∷ TestTree
test_margin2 =
  let r = node 100 100
          [ leaf  25 25
          , leaf  25 25 & geo.margin .~ LRTB 15 15 10 10
          , leaf  25 25
          ] & geo.align'items     .~ AlignEnd
            & geo.justify'content .~ AlignStart
            & layout
  in testGroup "margin2: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU  75   0) (mkSize  25  25)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU  60  35) (mkSize  25  25)
     , testCase "child 2" $ r^.child 2.area @?= Area (mkLU  75  70) (mkSize  25  25)
     ]

test_margin3 ∷ TestTree
test_margin3 =
  let r = node 100 100
          [ leaf  25 25
          , leaf  25 25 & geo.margin .~ LRTB 15 15 10 10
          , leaf  25 25
          ] & geo.align'items     .~ AlignStart
            & geo.justify'content .~ AlignEnd
            & layout
  in testGroup "margin3: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU   0   5) (mkSize  25  25)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU  15  40) (mkSize  25  25)
     , testCase "child 2" $ r^.child 2.area @?= Area (mkLU   0  75) (mkSize  25  25)
     ]

test_margin4 ∷ TestTree
test_margin4 =
  let r = node 100 100
          [ leaf  25 25
          , leaf  25 25 & geo.margin .~ LRTB 15 15 10 10
          , leaf  25 25
          ] & geo.align'items     .~ AlignEnd
            & geo.justify'content .~ AlignEnd
            & layout
  in testGroup "margin4: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU  75   5) (mkSize  25  25)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU  60  40) (mkSize  25  25)
     , testCase "child 2" $ r^.child 2.area @?= Area (mkLU  75  75) (mkSize  25  25)
     ]

test_margin5 ∷ TestTree
test_margin5 =
  let r = node 100 100
          [ leaf  10 10
          , leaf  10 10 & geo.margin .~ LRTB 15 10 0 0
          , leaf  10 10
          ] & geo.align'items     .~ AlignCenter
            & geo.justify'content .~ AlignStart
            & layout
  in testGroup "margin5: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU  45   0) (mkSize  10  10)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU  50  10) (mkSize  10  10)
     , testCase "child 2" $ r^.child 2.area @?= Area (mkLU  45  20) (mkSize  10  10)
     ]

test_margin6 ∷ TestTree
test_margin6 =
  let r = node 100 100
          [ leaf  10 10
          , leaf   0 10 & geo.margin     .~ LRTB 15 10 0 0
                                 & geo.align'self .~ AlignStretch
          , leaf  10 10
          ] & layout
  in testGroup "margin6 "
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU   0   0) (mkSize  10  10)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU  15  10) (mkSize  75  10)
     , testCase "child 2" $ r^.child 2.area @?= Area (mkLU   0  20) (mkSize  10  10)
     ]

test_margin7 ∷ TestTree
test_margin7 =
  let r = node 100 100
          [ leaf  10 10
          , leaf  10 10 & geo.margin     .~ LRTB 15 10 0 0
                                 & geo.align'self .~ AlignStretch
          , leaf  10 10
          ] & layout
  in testGroup "margin7 "
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU   0   0) (mkSize  10  10)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU  15  10) (mkSize  10  10)
     , testCase "child 2" $ r^.child 2.area @?= Area (mkLU   0  20) (mkSize  10  10)
     ]

test_margin8 ∷ TestTree
test_margin8 =
  let r = node 100 100
          [ leaf' Nothing (Just 10) & geo.margin .~ LRTB 10  0 0 0
          , leaf' Nothing (Just 10) & geo.margin .~ LRTB  0 10 0 0
          , leaf' Nothing (Just 10) & geo.margin .~ LRTB 10 20 0 0
          ] & geo.direction .~ DirColumn
            & layout
  in testGroup "margin8 "
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU  10   0) (mkSize  90  10)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU   0  10) (mkSize  90  10)
     , testCase "child 2" $ r^.child 2.area @?= Area (mkLU  10  20) (mkSize  70  10)
     ]

test_margin9 ∷ TestTree
test_margin9 =
  let r = node 100 100
          [ leaf' (Just 10) Nothing & geo.margin .~ LRTB 0 0 10  0
          , leaf' (Just 10) Nothing & geo.margin .~ LRTB 0 0  0 10
          , leaf' (Just 10) Nothing & geo.margin .~ LRTB 0 0 10 20
          ] & geo.direction .~ DirRow
            & layout
  in testGroup "margin9 "
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU   0  10) (mkSize  10  90)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU  10   0) (mkSize  10  90)
     , testCase "child 2" $ r^.child 2.area @?= Area (mkLU  20  10) (mkSize  10  70)
     ]
-- COMPLETE


-- *
--
test_shrink1 ∷ TestTree
test_shrink1 =
  let r = node 100 100
          [ leaf 100 100 & geo.shrink .~ 2
          , leaf 100 100 & geo.shrink .~ 3
          ] & layout
  in testGroup "shrink1"
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU   0   0) (mkSize 100  60)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU   0  60) (mkSize 100  40)
     ]

test_shrink2 ∷ TestTree
test_shrink2 =
  let r = node 100 100
          [ leaf 100 100
          , leaf 100 100 & geo.shrink .~ 4
          ] & layout
  in testGroup "shrink2"
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU   0   0) (mkSize 100  80)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU   0  80) (mkSize 100  20)
     ]

test_shrink3 ∷ TestTree
test_shrink3 =
  let r = node 100 100
          [ leaf 100 40 & geo.shrink .~ 2
          , leaf 100 40 & geo.shrink .~ 3
          ] & layout
  in testGroup "shrink3: the shrink attributes are not taken into account when there is enough flexible space available"
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU   0   0) (mkSize 100  40)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU   0  40) (mkSize 100  40)
     ]

test_shrink4 ∷ TestTree
test_shrink4 =
  let r = node 100 100
          [ leaf 100 25
          , leaf 100 25
          ] & geo.shrink .~ 2
            & layout
  in testGroup "shrink4: the shrink attribute is not inherited from children"
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU   0   0) (mkSize 100  25)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU   0  25) (mkSize 100  25)
     ]

test_shrink5 ∷ TestTree
test_shrink5 =
  let r = node 100 100
          [ leaf 100 550 & geo.shrink .~ 1
          ] & layout
  in testGroup "shrink5: all the container space is used when there is only one item with a positive value for the shrink attribute"
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU   0   0) (mkSize 100 100)
     ]

test_shrink6 ∷ TestTree
test_shrink6 =
  let r = node 100 100
          [ leaf 100 75 & geo.shrink .~ 1
          , leaf 100 75 & geo.shrink .~ 1
          ] & layout
  in testGroup "shrink6: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU   0   0) (mkSize 100  50)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU   0  50) (mkSize 100  50)
     ]
-- COMPLETE


-- * padding
--
test_padding1 ∷ TestTree
test_padding1 =
  let r = node 100 100
          [ leaf  25 25
          ] & geo.direction       .~ DirColumn
            & geo.justify'content .~ AlignStart
            & geo.align'items     .~ AlignStart
            & geo.padding         .~ LRTB 10 15 15 10
            & layout
  in testGroup "padding1: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU  10  15) (mkSize  25  25)
     ]

test_padding2 ∷ TestTree
test_padding2 =
  let r = node 100 100
          [ leaf  25 25
          ] & geo.direction       .~ DirColumn
            & geo.justify'content .~ AlignEnd
            & geo.align'items     .~ AlignStart
            & geo.padding         .~ LRTB 10 15 15 10
            & layout
  in testGroup "padding2: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU  10  65) (mkSize  25  25)
     ]

test_padding3 ∷ TestTree
test_padding3 =
  let r = node 100 100
          [ leaf  25 25
          ] & geo.direction       .~ DirColumn
            & geo.justify'content .~ AlignEnd
            & geo.align'items     .~ AlignEnd
            & geo.padding         .~ LRTB 10 15 15 10
            & layout
  in testGroup "padding3: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU  60  65) (mkSize  25  25)
     ]

test_padding4 ∷ TestTree
test_padding4 =
  let r = node 100 100
          [ leaf  25 25
          ] & geo.direction       .~ DirColumn
            & geo.justify'content .~ AlignStart
            & geo.align'items     .~ AlignEnd
            & geo.padding         .~ LRTB 10 15 15 10
            & layout
  in testGroup "padding4: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU  60  15) (mkSize  25  25)
     ]

test_padding5 ∷ TestTree
test_padding5 =
  let r = node 100 100
          [ leaf  0 25 & geo.align'self .~ AlignStretch
          ] & geo.direction       .~ DirColumn
            & geo.justify'content .~ AlignStart
            & geo.align'items     .~ AlignStart
            & geo.padding         .~ LRTB 10 15 15 10
            & layout
  in testGroup "padding5: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU  10  15) (mkSize  75  25)
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
  let r = node 100 100
          [ node  90  90
            [ node  80  80
              [ node  70  70
                [ node  60  60
                  [ leaf  50  50
                       & geo.align'items     .~ AlignCenter
                       & geo.justify'content .~ AlignCenter
                  ] & geo.align'items     .~ AlignCenter
                    & geo.justify'content .~ AlignCenter
                ] & geo.align'items     .~ AlignCenter
                  & geo.justify'content .~ AlignCenter
              ] & geo.align'items     .~ AlignCenter
                & geo.justify'content .~ AlignCenter
            ] & geo.align'items     .~ AlignCenter
              & geo.justify'content .~ AlignCenter
          ] & geo.align'items     .~ AlignCenter
            & geo.justify'content .~ AlignCenter
            & layout
  in testGroup "children4: "
     [ testCase "child 0"         $ r^.child 0.area                                 @?= Area (mkLU   5   5) (mkSize  90  90)
     , testCase "child 0.0"       $ r^.child 0.child 0.area                         @?= Area (mkLU   5   5) (mkSize  80  80)
     , testCase "child 0.0.0"     $ r^.child 0.child 0.child 0.area                 @?= Area (mkLU   5   5) (mkSize  70  70)
     , testCase "child 0.0.0.0"   $ r^.child 0.child 0.child 0.child 0.area         @?= Area (mkLU   5   5) (mkSize  60  60)
     , testCase "child 0.0.0.0.0" $ r^.child 0.child 0.child 0.child 0.child 0.area @?= Area (mkLU   5   5) (mkSize  50  50)
     ]
-- COMPLETE


-- * position
--
test_position1 ∷ TestTree
test_position1 =
  let r = node 100 100
          [ leaf  10 10 & geo.positioning .~ Absolute
          ] & layout
  in testGroup "position1: items with an absolute position default to the left/top corner"
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU   0   0) (mkSize  10  10)
     ]

test_position2 ∷ TestTree
test_position2 =
  let r = node 100 100
          [ leaf  10 10 & geo.positioning .~ Absolute
                                 & geo.absolute    .~ LRTB (Just 10) Nothing  (Just 10) Nothing
          , leaf  10 10 & geo.positioning .~ Absolute
                                 & geo.absolute    .~ LRTB  Nothing (Just 10) (Just 10) Nothing
          , leaf  10 10 & geo.positioning .~ Absolute
                                 & geo.absolute    .~ LRTB  Nothing (Just 10)  Nothing (Just 10)
          , leaf  10 10 & geo.positioning .~ Absolute
                                 & geo.absolute    .~ LRTB (Just 10) Nothing   Nothing (Just 10)
          ] & geo.align'items     .~ AlignCenter
            & geo.justify'content .~ AlignStart
            & layout
  in testGroup "position2 "
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU  10  10) (mkSize  10  10)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU  80  10) (mkSize  10  10)
     , testCase "child 2" $ r^.child 2.area @?= Area (mkLU  80  80) (mkSize  10  10)
     , testCase "child 3" $ r^.child 3.area @?= Area (mkLU  10  80) (mkSize  10  10)
     ]

test_position3 ∷ TestTree
test_position3 =
  let r = node 100 100
          [ leaf  10 10 & geo.positioning .~ Absolute
                                 & geo.absolute    .~ LRTB (Just 10) (Just 10) Nothing   Nothing
          , leaf  10 10 & geo.positioning .~ Absolute
                                 & geo.absolute    .~ LRTB  Nothing   Nothing (Just 10) (Just 10)
          ] & layout
  in testGroup "position3: if both left/right or top/bottom are given, left/top get the priority if the item has the appropriate size dimension set"
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU  10   0) (mkSize  10  10)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU   0  10) (mkSize  10  10)
     ]

test_position4 ∷ TestTree
test_position4 =
  let r = node 100 100
          [ leaf' Nothing (Just 20) & geo.positioning .~ Absolute
                                             & geo.absolute    .~ LRTB (Just 10) (Just 10) Nothing   Nothing
          , leaf' (Just 20) Nothing & geo.positioning .~ Absolute
                                             & geo.absolute    .~ LRTB  Nothing   Nothing (Just 10) (Just 10)
          ] & layout
  in testGroup "position4: if both left/right or top/bottom are given, the item is properly resized if the appropriate size dimension hasn't been set"
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU  10   0) (mkSize  80  20)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU   0  10) (mkSize  20  80)
     ]

test_position5 ∷ TestTree
test_position5 =
  let r = node 100 100
          [ leaf  10 10 & geo.positioning .~ Absolute
                                 & geo.basis       .~ 20
                                 & geo.absolute    .~ LRTB (Just 10) Nothing Nothing (Just 10)
          ] & layout
  in testGroup "position5: the `basis' property is ignored for items with an absolute position"
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU  10  80) (mkSize  10  10)
     ]

test_position6 ∷ TestTree
test_position6 =
  let r = node 200 200
          [ leaf  50 50
          , leaf  50 50 & geo.positioning .~ Absolute
                                 & geo.absolute    .~ LRTB Nothing (Just 0) Nothing (Just 0)
          , leaf  50 50
          ] & geo.direction .~ DirRow
            & layout
  in testGroup "position6: items with an absolute position are separated from the other items during the layout"
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU   0   0) (mkSize  50  50)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU 150 150) (mkSize  50  50)
     , testCase "child 2" $ r^.child 2.area @?= Area (mkLU  50   0) (mkSize  50  50)
     ]

test_position7 ∷ TestTree
test_position7 =
  let r = node 120 120
          [ leaf  50 50
          , leaf  50 50
          , leaf  50 50 & geo.positioning .~ Absolute
                                 & geo.absolute    .~ LRTB Nothing (Just 0) (Just 0) Nothing
          , leaf  50 50
          ] & geo.wrap            .~ Wrap
            & geo.justify'content .~ AlignSpaceAround
            & geo.align'content   .~ AlignStart
            & layout
  in testGroup "position7: items with an absolute position are separated from the other items during the layout and are not taken into account when calculating spacing"
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU   0   5) (mkSize  50  50)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU   0  65) (mkSize  50  50)
     , testCase "child 2" $ r^.child 2.area @?= Area (mkLU  70   0) (mkSize  50  50)
     , testCase "child 3" $ r^.child 3.area @?= Area (mkLU  50  35) (mkSize  50  50)
     ]

test_position8 ∷ TestTree
test_position8 =
  let r = node 100 100
          [ node' Nothing Nothing
            [ node  60 60
              [ leaf  40 40 & geo.positioning .~ Absolute
                                     & geo.absolute    .~ LRTB (Just 10)  Nothing   Nothing (Just 10)
              ] & geo.positioning .~ Absolute
                & geo.absolute    .~ LRTB  Nothing  (Just 10) (Just 10) Nothing
            ] & geo.positioning .~ Absolute
              & geo.absolute    .~ LRTB (Just 10) (Just 10) (Just 10) (Just 10)
          ] & geo.direction .~ DirRow
            & layout
  in testGroup "position8: items with an absolute position can be nested"
     [ testCase "child 0" $ r^.child 0.area                 @?= Area (mkLU  10  10) (mkSize  80  80)
     , testCase "child 1" $ r^.child 0.child 0.area         @?= Area (mkLU  10  10) (mkSize  60  60)
     , testCase "child 2" $ r^.child 0.child 0.child 0.area @?= Area (mkLU  10  10) (mkSize  40  40)
     ]
-- COMPLETE


-- * direction
--
test_direction1 ∷ TestTree
test_direction1 =
  let r = node 200 200
          [ leaf 50 50
          , leaf 50 50
          , leaf 50 50
          ] & geo.direction .~ DirRow
            & layout
  in testGroup "direction1: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU   0   0) (mkSize  50  50)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU  50   0) (mkSize  50  50)
     , testCase "child 2" $ r^.child 2.area @?= Area (mkLU 100   0) (mkSize  50  50)
     ]

test_direction2 ∷ TestTree
test_direction2 =
  let r = node 200 200
          [ leaf 50 50
          , leaf 50 50
          , leaf 50 50
          ] & geo.direction .~ DirColumn
            & layout
  in testGroup "direction2: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU   0   0) (mkSize  50  50)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU   0  50) (mkSize  50  50)
     , testCase "child 2" $ r^.child 2.area @?= Area (mkLU   0 100) (mkSize  50  50)
     ]

test_direction3 ∷ TestTree
test_direction3 =
  let r = node 200 200
          [ leaf 50 50
          , leaf 50 50
          , leaf 50 50
          ] & geo.direction .~ DirRowReverse
            & layout
  in testGroup "direction3: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU 150   0) (mkSize  50  50)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU 100   0) (mkSize  50  50)
     , testCase "child 2" $ r^.child 2.area @?= Area (mkLU  50   0) (mkSize  50  50)
     ]

test_direction4 ∷ TestTree
test_direction4 =
  let r = node 200 200
          [ leaf 50 50
          , leaf 50 50
          , leaf 50 50
          ] & geo.direction .~ DirColumnReverse
            & layout
  in testGroup "direction4: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU   0 150) (mkSize  50  50)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU   0 100) (mkSize  50  50)
     , testCase "child 2" $ r^.child 2.area @?= Area (mkLU   0  50) (mkSize  50  50)
     ]
-- COMPLETE


-- *
--
test_align_self1 ∷ TestTree
test_align_self1 =
  let r = node 100 100
          [ leaf  50  25 & geo.align'self .~ AlignStart
          , leaf  50  25 & geo.align'self .~ AlignStart
          , leaf  50  25 & geo.align'self .~ AlignStart
          ] & layout
  in testGroup "align_self1"
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU   0   0) (mkSize  50  25)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU   0  25) (mkSize  50  25)
     , testCase "child 2" $ r^.child 2.area @?= Area (mkLU   0  50) (mkSize  50  25)
     ]

test_align_self2 ∷ TestTree
test_align_self2 =
  let r = node 100 100
          [ leaf  50  25 & geo.align'self .~ AlignEnd
          , leaf  50  25 & geo.align'self .~ AlignEnd
          , leaf  50  25 & geo.align'self .~ AlignEnd
          ] & layout
  in testGroup "align_self2"
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU  50   0) (mkSize  50  25)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU  50  25) (mkSize  50  25)
     , testCase "child 2" $ r^.child 2.area @?= Area (mkLU  50  50) (mkSize  50  25)
     ]

test_align_self3 ∷ TestTree
test_align_self3 =
  let r = node 100 100
          [ leaf  50  25 & geo.align'self .~ AlignCenter
          , leaf  50  25 & geo.align'self .~ AlignCenter
          , leaf  50  25 & geo.align'self .~ AlignCenter
          ] & layout
  in testGroup "align_self3"
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU  25   0) (mkSize  50  25)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU  25  25) (mkSize  50  25)
     , testCase "child 2" $ r^.child 2.area @?= Area (mkLU  25  50) (mkSize  50  25)
     ]

test_align_self4 ∷ TestTree
test_align_self4 =
  let r = node 100 100
          [ leaf' Nothing (Just 25) & geo.align'self .~ AlignStretch
          , leaf        0       25  & geo.align'self .~ AlignStretch
          , leaf' Nothing (Just 25) & geo.align'self .~ AlignStretch
          ] & layout
  in testGroup "align_self4: stretch works if the align dimension is not set or is 0"
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU   0   0) (mkSize 100  25)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU   0  25) (mkSize 100  25)
     , testCase "child 2" $ r^.child 2.area @?= Area (mkLU   0  50) (mkSize 100  25)
     ]

test_align_self5 ∷ TestTree
test_align_self5 =
  let r = node 100 100
          [ leaf  50  25 & geo.align'self .~ AlignStretch
          , leaf  50  50 & geo.align'self .~ AlignStretch
          , leaf  50  25 & geo.align'self .~ AlignStretch
          ] & layout
  in testGroup "align_self5: stretch does not work if the align dimension is set"
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU   0   0) (mkSize  50  25)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU   0  25) (mkSize  50  50)
     , testCase "child 2" $ r^.child 2.area @?= Area (mkLU   0  75) (mkSize  50  25)
     ]

test_align_self6 ∷ TestTree
test_align_self6 =
  let r = node 100 100
          [ leaf  50  25 & geo.align'self .~ AlignStart
          , leaf  50  25 & geo.align'self .~ AlignCenter
          , leaf   0  25 & geo.align'self .~ AlignStretch
          , leaf  50  25 & geo.align'self .~ AlignEnd
          ] & layout
  in testGroup "align_self6: potpourri"
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU   0   0) (mkSize  50  25)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU  25  25) (mkSize  50  25)
     , testCase "child 2" $ r^.child 2.area @?= Area (mkLU   0  50) (mkSize 100  25)
     , testCase "child 2" $ r^.child 3.area @?= Area (mkLU  50  75) (mkSize  50  25)
     ]
-- COMPLETE


-- *
--
test_align_items1 ∷ TestTree
test_align_items1 =
  let r = node 100 100
          [ leaf  50  25
          , leaf  50  25
          , leaf  50  25
          ] & geo.align'items .~ AlignStart
            & layout
  in testGroup "align_items1: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU   0   0) (mkSize  50  25)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU   0  25) (mkSize  50  25)
     , testCase "child 2" $ r^.child 2.area @?= Area (mkLU   0  50) (mkSize  50  25)
     ]

test_align_items2 ∷ TestTree
test_align_items2 =
  let r = node 100 100
          [ leaf  50  25
          , leaf  50  25
          , leaf  50  25
          ] & geo.align'items .~ AlignEnd
            & layout
  in testGroup "align_items2: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU  50   0) (mkSize  50  25)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU  50  25) (mkSize  50  25)
     , testCase "child 2" $ r^.child 2.area @?= Area (mkLU  50  50) (mkSize  50  25)
     ]

test_align_items3 ∷ TestTree
test_align_items3 =
  let r = node 100 100
          [ leaf  50  25
          , leaf  50  25
          , leaf  50  25
          ] & geo.align'items .~ AlignCenter
            & layout
  in testGroup "align_items3: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU  25   0) (mkSize  50  25)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU  25  25) (mkSize  50  25)
     , testCase "child 2" $ r^.child 2.area @?= Area (mkLU  25  50) (mkSize  50  25)
     ]

test_align_items4 ∷ TestTree
test_align_items4 =
  let r = node 100 100
          [ leaf       50        25
          , leaf        0        25
          , leaf' Nothing  (Just 25)
          ] & geo.align'items .~ AlignStretch
            & layout
  in testGroup "align_items4: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU   0   0) (mkSize  50  25)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU   0  25) (mkSize 100  25)
     , testCase "child 2" $ r^.child 2.area @?= Area (mkLU   0  50) (mkSize 100  25)
     ]

test_align_items5 ∷ TestTree
test_align_items5 =
  let r = node 100 100
          [ leaf 50 25
          , leaf 50 25 & geo.align'self .~ AlignStart
          , leaf 50 25 & geo.align'self .~ AlignAuto
          , leaf 50 25 & geo.align'self .~ AlignEnd
          ] & geo.align'items .~ AlignCenter
            & layout
  in testGroup "align_items5: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU  25   0) (mkSize  50  25)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU   0  25) (mkSize  50  25)
     , testCase "child 2" $ r^.child 2.area @?= Area (mkLU  25  50) (mkSize  50  25)
     , testCase "child 3" $ r^.child 3.area @?= Area (mkLU  50  75) (mkSize  50  25)
     ]
-- COMPLETE


-- *
--
test_align_content1 ∷ TestTree
test_align_content1 =
  let r = node 200 120
          [ leaf  50  50
          , leaf  60  50
          , leaf  40  50
          ] & geo.wrap          .~ Wrap
            & geo.align'content .~ AlignStart
            & layout
  in testGroup "align_content1: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU   0   0) (mkSize  50  50)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU   0  50) (mkSize  60  50)
     , testCase "child 2" $ r^.child 2.area @?= Area (mkLU  60   0) (mkSize  40  50)
     ]

test_align_content2 ∷ TestTree
test_align_content2 =
  let r = node 200 120
          [ leaf  50  50
          , leaf  60  50
          , leaf  40  50
          ] & geo.wrap          .~ Wrap
            & geo.align'content .~ AlignCenter
            & layout
  in testGroup "align_content2: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU  50   0) (mkSize  50  50)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU  50  50) (mkSize  60  50)
     , testCase "child 2" $ r^.child 2.area @?= Area (mkLU 110   0) (mkSize  40  50)
     ]

test_align_content3 ∷ TestTree
test_align_content3 =
  let r = node 200 120
          [ leaf  50  50
          , leaf  60  50
          , leaf  40  50
          ] & geo.wrap          .~ Wrap
            & geo.align'content .~ AlignEnd
            & layout
  in testGroup "align_content3: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU 100   0) (mkSize  50  50)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU 100  50) (mkSize  60  50)
     , testCase "child 2" $ r^.child 2.area @?= Area (mkLU 160   0) (mkSize  40  50)
     ]

test_align_content4 ∷ TestTree
test_align_content4 =
  let r = node 200 120
          [ leaf  50  50
          , leaf  60  50
          , leaf  40  50
          ] & geo.wrap          .~ Wrap
            & geo.align'content .~ AlignSpaceBetween
            & layout
  in testGroup "align_content4: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU   0   0) (mkSize  50  50)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU   0  50) (mkSize  60  50)
     , testCase "child 2" $ r^.child 2.area @?= Area (mkLU 160   0) (mkSize  40  50)
     ]

test_align_content5 ∷ TestTree
test_align_content5 =
  let r = node 200 120
          [ leaf  50  50
          , leaf  60  50
          , leaf  40  50
          ] & geo.wrap          .~ Wrap
            & geo.align'content .~ AlignSpaceAround
            & layout
  in testGroup "align_content5: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU  25   0) (mkSize  50  50)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU  25  50) (mkSize  60  50)
     , testCase "child 2" $ r^.child 2.area @?= Area (mkLU 135   0) (mkSize  40  50)
     ]

test_align_content6 ∷ TestTree
test_align_content6 =
  let r = node 250 120
          [ leaf  50  50
          , leaf  60  50
          , leaf  40  50
          ] & geo.wrap          .~ Wrap
            & geo.align'content .~ AlignSpaceEvenly
            & layout
  in testGroup "align_content6: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU  50   0) (mkSize  50  50)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU  50  50) (mkSize  60  50)
     , testCase "child 2" $ r^.child 2.area @?= Area (mkLU 160   0) (mkSize  40  50)
     ]
-- COMPLETE


-- *
--
-- test_default1 -- too simple to test
--
test_default_values2 ∷ TestTree
test_default_values2 =
  let r = node 200 200
          [ leaf' (Just 100) Nothing
          , leaf' Nothing   (Just 100)
          , leaf' Nothing    Nothing
          ] & geo.direction .~ DirColumn
            & layout
  in testGroup "default_values2: if the width/height property isn't set on a child, it's frame size defaults to 0 for the main axis and the parent's size for the minor axis"
     [ testCase "child 0" $ r^.child 0.area.area'b @?= mkSize 100   0
     , testCase "child 1" $ r^.child 1.area.area'b @?= mkSize 200 100
     , testCase "child 2" $ r^.child 2.area.area'b @?= mkSize 200   0
     ]

test_default_values3 ∷ TestTree
test_default_values3 =
  let r = node 200 200
          [ leaf' (Just 100) Nothing
          , leaf' Nothing   (Just 100)
          , leaf' Nothing    Nothing
          ] & geo.direction .~ DirRow
            & layout
  in testGroup "default_values3: "
     [ testCase "child 0" $ r^.child 0.area.area'b @?= mkSize 100 200
     , testCase "child 1" $ r^.child 1.area.area'b @?= mkSize   0 100
     , testCase "child 2" $ r^.child 2.area.area'b @?= mkSize   0 200
     ]
-- COMPLETE


-- *
--
test_justify_content1 ∷ TestTree
test_justify_content1 =
  let r = node 100 300
          [ leaf 50 100
          , leaf 50 100
          ] & geo.justify'content .~ AlignCenter
            & layout
  in testGroup "justify_content1: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU   0  50) (mkSize  50 100)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU   0 150) (mkSize  50 100)
     ]

test_justify_content2 ∷ TestTree
test_justify_content2 =
  let r = node 100 300
          [ leaf 50 100
          , leaf 50 100
          ] & geo.justify'content .~ AlignStart
            & layout
  in testGroup "justify_content2: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU   0   0) (mkSize  50 100)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU   0 100) (mkSize  50 100)
     ]

test_justify_content3 ∷ TestTree
test_justify_content3 =
  let r = node 100 300
          [ leaf 50 100
          , leaf 50 100
          ] & geo.justify'content .~ AlignEnd
            & layout
  in testGroup "justify_content3: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU   0 100) (mkSize  50 100)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU   0 200) (mkSize  50 100)
     ]

test_justify_content4 ∷ TestTree
test_justify_content4 =
  let r = node 100 300
          [ leaf 50 100
          , leaf 50 100
          ] & geo.justify'content .~ AlignSpaceBetween
            & layout
  in testGroup "justify_content4: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU   0   0) (mkSize  50 100)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU   0 200) (mkSize  50 100)
     ]

test_justify_content5 ∷ TestTree
test_justify_content5 =
  let r = node 100 300
          [ leaf 50 50
          , leaf 50 50
          , leaf 50 50
          ] & geo.justify'content .~ AlignSpaceBetween
            & layout
  in testGroup "justify_content5: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU   0   0) (mkSize  50 50)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU   0 125) (mkSize  50 50)
     , testCase "child 2" $ r^.child 2.area @?= Area (mkLU   0 250) (mkSize  50 50)
     ]

test_justify_content6 ∷ TestTree
test_justify_content6 =
  let r = node 100 300
          [ leaf 50 100
          , leaf 50 100
          ] & geo.justify'content .~ AlignSpaceAround
            & layout
  in testGroup "justify_content6: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU   0  25) (mkSize  50 100)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU   0 175) (mkSize  50 100)
     ]

test_justify_content7 ∷ TestTree
test_justify_content7 =
  let r = node 100 300
          [ leaf 50 50
          , leaf 50 50
          , leaf 50 50
          ] & geo.justify'content .~ AlignSpaceAround
            & layout
  in testGroup "justify_content7: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU   0  25) (mkSize  50 50)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU   0 125) (mkSize  50 50)
     , testCase "child 2" $ r^.child 2.area @?= Area (mkLU   0 225) (mkSize  50 50)
     ]

test_justify_content8 ∷ TestTree
test_justify_content8 =
  let r = node 100 300
          [ leaf 50 105
          , leaf 50 105
          ] & geo.justify'content .~ AlignSpaceEvenly
            & layout
  in testGroup "justify_content8: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU   0  30) (mkSize  50 105)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU   0 165) (mkSize  50 105)
     ]

test_justify_content9 ∷ TestTree
test_justify_content9 =
  let r = node 100 300
          [ leaf 50 40
          , leaf 50 40
          , leaf 50 40
          ] & geo.justify'content .~ AlignSpaceEvenly
            & layout
  in testGroup "justify_content9: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU   0  45) (mkSize  50 40)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU   0 130) (mkSize  50 40)
     , testCase "child 2" $ r^.child 2.area @?= Area (mkLU   0 215) (mkSize  50 40)
     ]

test_justify_content10 ∷ TestTree
test_justify_content10 =
  let r = node 100 300
          [ leaf 50 100
          , leaf 50 100
          ] & geo.direction       .~ DirColumnReverse
            & geo.justify'content .~ AlignCenter
            & layout
  in testGroup "justify_content10: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU   0 150) (mkSize  50 100)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU   0  50) (mkSize  50 100)
     ]

test_justify_content11 ∷ TestTree
test_justify_content11 =
  let r = node 100 300
          [ leaf 50 100
          , leaf 50 100
          ] & geo.direction       .~ DirColumnReverse
            & geo.justify'content .~ AlignStart
            & layout
  in testGroup "justify_content11: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU   0 200) (mkSize  50 100)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU   0 100) (mkSize  50 100)
     ]

test_justify_content12 ∷ TestTree
test_justify_content12 =
  let r = node 100 300
          [ leaf 50 100
          , leaf 50 100
          ] & geo.direction       .~ DirColumnReverse
            & geo.justify'content .~ AlignEnd
            & layout
  in testGroup "justify_content12: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU   0 100) (mkSize  50 100)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU   0   0) (mkSize  50 100)
     ]

test_justify_content13 ∷ TestTree
test_justify_content13 =
  let r = node 100 300
          [ leaf 50 100
          , leaf 50 100
          ] & geo.direction       .~ DirColumnReverse
            & geo.justify'content .~ AlignSpaceBetween
            & layout
  in testGroup "justify_content13: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU   0 200) (mkSize  50 100)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU   0   0) (mkSize  50 100)
     ]

test_justify_content14 ∷ TestTree
test_justify_content14 =
  let r = node 100 300
          [ leaf 50 100
          , leaf 50 100
          ] & geo.direction       .~ DirColumnReverse
            & geo.justify'content .~ AlignSpaceAround
            & layout
  in testGroup "justify_content14: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU   0 175) (mkSize  50 100)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU   0  25) (mkSize  50 100)
     ]

test_justify_content15 ∷ TestTree
test_justify_content15 =
  let r = node 100 300
          [ leaf 50 105
          , leaf 50 105
          ] & geo.direction       .~ DirColumnReverse
            & geo.justify'content .~ AlignSpaceEvenly
            & layout
  in testGroup "justify_content15: "
     [ testCase "child 0" $ r^.child 0.area @?= Area (mkLU   0 165) (mkSize  50 105)
     , testCase "child 1" $ r^.child 1.area @?= Area (mkLU   0  30) (mkSize  50 105)
     ]

test_justify_content16 ∷ TestTree
test_justify_content16 =
  let modes = [AlignStart, AlignCenter, AlignEnd, AlignSpaceBetween, AlignSpaceAround, AlignSpaceEvenly]
      r mode
        = node 100 100
          [ leaf 50 50
          , leaf 50 50
          ] & geo.justify'content .~ mode
            & layout
  in testGroup "justify_content16: the `justify_content' property is ignored when the children fill up all the space" $
     flip concatMap modes
       (\mode→
          [ testCase (printf "child 0 / %s" $ show mode) $ (r mode)^.child 0.area @?= Area (mkLU   0   0) (mkSize  50  50)
          , testCase (printf "child 1 / %s" $ show mode) $ (r mode)^.child 1.area @?= Area (mkLU   0  50) (mkSize  50  50)
          ])

test_justify_content17 ∷ TestTree
test_justify_content17 =
  let modes = [AlignStart, AlignCenter, AlignEnd, AlignSpaceBetween, AlignSpaceAround, AlignSpaceEvenly]
      r mode
        = node 100 100
          [ leaf 50 100
          , leaf 50 100
          , leaf 50 100
          , leaf 50 100
          ] & geo.justify'content .~ mode
            & layout
  in testGroup "justify_content17: the `justify_content' property is ignored when the children fill up all the space" $
     flip concatMap modes
       (\mode→
          [ testCase (printf "child 0 / %s" $ show mode) $ (r mode)^.child 0.area @?= Area (mkLU   0   0) (mkSize  50  25)
          , testCase (printf "child 1 / %s" $ show mode) $ (r mode)^.child 1.area @?= Area (mkLU   0  25) (mkSize  50  25)
          , testCase (printf "child 2 / %s" $ show mode) $ (r mode)^.child 2.area @?= Area (mkLU   0  50) (mkSize  50  25)
          , testCase (printf "child 3 / %s" $ show mode) $ (r mode)^.child 3.area @?= Area (mkLU   0  75) (mkSize  50  25)
          ])

test_justify_content18 ∷ TestTree
test_justify_content18 =
  let modes = [AlignStart, AlignCenter, AlignEnd, AlignSpaceBetween, AlignSpaceAround, AlignSpaceEvenly]
      r mode
        = node 100 100
          [ leaf 50 20
          , leaf 50 20 & geo.grow .~ 1
          , leaf 50 20
          ] & geo.justify'content .~ mode
            & layout
  in testGroup "justify_content18: the `justify_content' property is ignored when there are flexible children" $
     flip concatMap modes
       (\mode→
          [ testCase (printf "child 0 / %s" $ show mode) $ (r mode)^.child 0.area @?= Area (mkLU   0   0) (mkSize  50  20)
          , testCase (printf "child 1 / %s" $ show mode) $ (r mode)^.child 1.area @?= Area (mkLU   0  20) (mkSize  50  60)
          , testCase (printf "child 2 / %s" $ show mode) $ (r mode)^.child 2.area @?= Area (mkLU   0  80) (mkSize  50  20)
          ])
-- COMPLETE

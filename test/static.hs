{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import qualified Data.TypeMap.List as TL
import qualified Data.TypeMap.Vector as TV

type D = '[ '("a", Int), '("b", Bool), '("c", String)]

v :: TV.TypeVector D
v = TL.toVector l

l :: TL.TypeList D
l = (TL.cons 0 (TL.cons True (TL.cons "" TL.empty)))

main = defaultMain
  [ testCase "List" $ do
      0 @=? TL.index @"a" l
      True @=? TL.index @"b" l
      "" @=? TL.index @"c" l
  , testCase "Vector" $ do
      0 @=? TV.index @"a" v
      True @=? TV.index @"b" v
      "" @=? TV.index @"c" v
  ]

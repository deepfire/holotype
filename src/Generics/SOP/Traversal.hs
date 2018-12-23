{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns #-}
-- | Generalized traversals
--
-- Intended to be imported qualified
--
-- > import Generics.SOP.Traversal as GTraversal
--
module Generics.SOP.Traversal (
    -- * Generalized traversals
    GTraversal
  , traversal
  , get
  , modify
  , set
    -- * Conversion
  , fromLens
  , fromIso
  , toLens
    -- * Generic computation of lenses for record type
  , gtraversals
    -- * Labels for the representation types
  , np
  , rep
  , sop
  , head
  , tail
  , i
  ) where

import Prelude hiding (id, (.), curry, uncurry, const, head, tail)
import Control.Arrow
import Control.Category
import Data.Label.Mono (Lens)
import Data.Label.Point (Iso(..))
import qualified Data.Label.Mono as Lens

import Generics.SOP
import qualified GHC.Generics as GHC

{-------------------------------------------------------------------------------
  Generalized lens using two categories
-------------------------------------------------------------------------------}

-- | GTraversal generalizes a monomorphic lens by allowing for different categories
-- for the getter and modifier
data GTraversal r w z b = GTraversal (r z b) (w (w b b, z) z)

instance (Category r, ArrowApply w) => Category (GTraversal r w) where
  id = GTraversal id app
  (GTraversal f m) . (GTraversal g n) = GTraversal (f . g) (uncurry (curry n . curry m))

traversal :: r a b -> w (w b b, a) a -> GTraversal r w a b
traversal = GTraversal

get :: GTraversal r w a b -> r a b
get (GTraversal f _) = f

modify :: GTraversal r w a b -> w (w b b, a) a
modify (GTraversal _ g) = g

set :: Arrow w => GTraversal r w a b -> w (b, a) a
set l = modify l . first (arr const)

{-------------------------------------------------------------------------------
  Conversion
-------------------------------------------------------------------------------}

fromLens :: (Arrow r, ArrowApply w) => Lens (->) a b -> GTraversal r w a b
fromLens l =
  GTraversal (arr (Lens.get l))
        (uncurry $ \h -> arr (Lens.set l) . (h . arr (Lens.get l) &&& id))

fromIso :: (Arrow r, ArrowApply w) => Iso (->) a b -> GTraversal r w a b
fromIso (Iso f g) = GTraversal (arr f) (uncurry $ \h -> arr g . h . arr f)

toLens :: GTraversal cat cat a b -> Lens cat a b
toLens (GTraversal f g) = Lens.lens f g

{-------------------------------------------------------------------------------
  Generic computation of all traversals for a record type
-------------------------------------------------------------------------------}

gtraversals :: forall r w y xss . ( Generic y, Code y ~ xss
                                  , Arrow r, ArrowApply w
                                  , SListI xss
                                  , All SListI xss)
            => NP (NP (GTraversal r w y)) xss
gtraversals =
  hliftA
  (\(CTraversals p :: CTraversals r w xss xs)->
   (hliftA
    (\l -> l . p . sop . rep)
    np))
  nptraversals

-- | Traversal pack selector for some constructor
data CTraversals r w xss xs
  = (SListI xss, All SListI xss, SListI xs)
  => CTraversals (GTraversal r w (NS (NP I) xss) (NP I xs))

nptraversals :: forall r w xss. (Arrow r, ArrowApply w, SListI xss, All SListI xss) => NP (CTraversals r w xss) xss
nptraversals = case sList :: SList xss of
  SNil  -> Nil
  SCons ->
    (CTraversals
      ((fromIso $ Iso (\(Z x) -> x) Z)
        :: GTraversal r w (NS (NP I) (xs1 ': xss')) (NP I xs1))
      :: (SListI xss', All SListI xss', SListI xs) =>       CTraversals r w (xs ': xss') xs)
    :*
    (hliftA
     ((\(CTraversals x) -> CTraversals (x . tail'))
      :: (SListI xss', All SListI xss', SListI xs)
      => CTraversals r w        xss'  x
      -> CTraversals r w (xs ': xss') x)
     (nptraversals :: (SListI xss', All SListI xss') => NP (CTraversals r w        xss')  xss')
     :: (SListI xss', All SListI xss', SListI xs) =>    NP (CTraversals r w (xs ': xss')) xss')

tail' :: (Arrow r, ArrowApply w, SListI xss, SListI xs) => GTraversal r w (NS (NP I) (xs ': xss)) (NS (NP I) xss)
tail' = fromLens $ Lens.lens (\(S xs) -> xs) (\(f, S xs) -> (S (f xs)))

tail  :: (Arrow r, ArrowApply w) =>                        GTraversal r w (NP f       (x ': xs))      (NP f  xs)
tail  = fromLens $ Lens.lens (\(_ :* xs) -> xs) (\(f, x :* xs) -> (x :* f xs))

np :: forall r w xs. (Arrow r, ArrowApply w, SListI xs) => NP (GTraversal r w (NP I xs)) xs
np = case sList :: SList xs of
      SNil  -> Nil
      SCons -> (i . head            ::                   GTraversal r w (NP I (x ': xs1)) x)
               :*
               (hliftA (. tail) (np :: SListI xs1 => NP (GTraversal r w (NP I       xs1))  xs1)
                                    :: SListI xs1 => NP (GTraversal r w (NP I (x ': xs1))) xs1)

sop :: (Arrow r, ArrowApply w) => GTraversal r w (SOP I xss) (NS (NP I) xss)
sop = fromIso $ Iso (\(SOP x) -> x)  SOP

{-------------------------------------------------------------------------------
  Generalized lenses for representation types
-------------------------------------------------------------------------------}

i :: (Arrow r, ArrowApply w) => GTraversal r w (I a) a
i = fromIso $ Iso unI I

head :: (Arrow r, ArrowApply w) => GTraversal r w (NP f (x ': xs)) (f x)
head = fromLens $ Lens.lens
  (\(x :* _) -> x)
  (\(f, x :* xs) -> (f x :* xs))

rep :: (Arrow r, ArrowApply w, Generic a) => GTraversal r w a (Rep a)
rep = fromIso $ Iso from to

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

const :: Arrow arr => c -> arr b c
const a = arr (\_ -> a)

curry :: Arrow cat => cat (a, b) c -> (a -> cat b c)
curry m a = m . (const a &&& id)

uncurry :: ArrowApply cat => (a -> cat b c) -> cat (a, b) c
uncurry a = app . arr (first a)

------------------------------------------------------------------------------

data Ex = Num Int | Add { left :: Ex, right :: Ex } | Yay { x :: Ex } deriving (Show, GHC.Generic)
instance Generic Ex

expps :: Code Ex ~ xss => NP (NP (GTraversal (->) (->) Ex)) xss
expps = gtraversals

(,,,) (numX,pnx) (addX,pax) (addY,pay) (yayX,pyx) =
  let (((pnx@(GTraversal numX numX') :* Nil)) :*
       ((pax@(GTraversal addX addX') :* pay@(GTraversal addY addY') :* Nil)) :*
       ((pyx@(GTraversal yayX yayX') :* Nil)) :* Nil) = expps
  in (,,,) (numX,pnx) (addX,pax) (addY,pay) (yayX,pyx)

num = Num 0
add = Add (Num 1) (Num 2)
yay = Yay (Num 3)

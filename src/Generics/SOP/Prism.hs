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
-- | Generalized prisms
--
-- Intended to be imported qualified
--
-- > import Generics.SOP.Prism as GPrism
--
module Generics.SOP.Prism (
    -- * Generalized prisms
    GPrism
  , prism
  , get
  , modify
  , set
    -- * Conversion
  , fromLens
  , fromIso
  , toLens
    -- * Generic computation of lenses for record type
  , gprisms
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
import Data.SOP.NP
import Data.Label.Mono (Lens)
import Data.Label.Point (Iso(..))
import qualified Data.Label.Mono as Lens

import Generics.SOP

{-------------------------------------------------------------------------------
  Generalized lens using two categories
-------------------------------------------------------------------------------}

-- | GPrism generalizes a monomorphic lens by allowing for different categories
-- for the getter and modifier
data GPrism r w z b = GPrism (r z b) (w (w b b, z) z)

instance (Category r, ArrowApply w) => Category (GPrism r w) where
  id = GPrism id app
  (GPrism f m) . (GPrism g n) = GPrism (f . g) (uncurry (curry n . curry m))

prism :: r a b -> w (w b b, a) a -> GPrism r w a b
prism = GPrism

get :: GPrism r w a b -> r a b
get (GPrism f _) = f

modify :: GPrism r w a b -> w (w b b, a) a
modify (GPrism _ g) = g

set :: Arrow w => GPrism r w a b -> w (b, a) a
set l = modify l . first (arr const)

{-------------------------------------------------------------------------------
  Conversion
-------------------------------------------------------------------------------}

fromLens :: (Arrow r, ArrowApply w) => Lens (->) a b -> GPrism r w a b
fromLens l =
  GPrism (arr (Lens.get l))
        (uncurry $ \h -> arr (Lens.set l) . (h . arr (Lens.get l) &&& id))

fromIso :: (Arrow r, ArrowApply w) => Iso (->) a b -> GPrism r w a b
fromIso (Iso f g) = GPrism (arr f) (uncurry $ \h -> arr g . h . arr f)

toLens :: GPrism cat cat a b -> Lens cat a b
toLens (GPrism f g) = Lens.lens f g

{-------------------------------------------------------------------------------
  Generic computation of all prisms for a record type
-------------------------------------------------------------------------------}

gprisms :: forall r w y xss . ( Generic y, Code y ~ xss
                              , Arrow r, ArrowApply w
                              , SListI xss
                              , All SListI xss)
        => NP (NP (GPrism r w y)) xss
gprisms =
  hliftA
  (\(CPrisms p :: CPrisms r w xss xs)->
   (hliftA
    (\l -> l . p . sop . rep)
    np))
  npprisms

-- | Prism pack selector for some constructor
data CPrisms r w xss xs
  = (SListI xss, All SListI xss, SListI xs)
  => CPrisms { unCPrisms :: GPrism r w (NS (NP I) xss) (NP I xs) }

npprisms :: forall r w xss. (Arrow r, ArrowApply w, SListI xss, All SListI xss) => NP (CPrisms r w xss) xss
npprisms = case sList :: SList xss of
             SNil  -> Nil
             SCons ->
               (CPrisms
                ((fromIso $ Iso (\(Z x) -> x) Z)
                 :: GPrism r w (NS (NP I) (xs1 : xss')) (NP I xs1))
               :: (SListI xss', All SListI xss', SListI xs) =>                CPrisms r w (xs : xss') xs)
               :*
               (hliftA
                ((\(CPrisms x) -> CPrisms (x . tail'))
                 :: (SListI xss', All SListI xss', SListI xs)
                 => CPrisms r w       xss'  x
                 -> CPrisms r w (xs : xss') x)
                (npprisms :: (SListI xss', All SListI xss')            => NP (CPrisms r w       xss')  xss')
                          :: (SListI xss', All SListI xss', SListI xs) => NP (CPrisms r w (xs : xss')) xss')

tail' :: (Arrow r, ArrowApply w, SListI xss, SListI xs) => GPrism r w (NS (NP I) (xs : xss)) (NS (NP I) xss)
tail' = fromLens $ Lens.lens (\(S xs) -> xs) (\(f, S xs) -> (S (f xs)))

tail  :: (Arrow r, ArrowApply w) =>                        GPrism r w (NP f         (x ': xs))      (NP f       xs)
tail  = fromLens $ Lens.lens (\(_ :* xs) -> xs) (\(f, x :* xs) -> (x :* f xs))

np :: forall r w xs. (Arrow r, ArrowApply w, SListI xs) => NP (GPrism r w (NP I xs)) xs
np = case sList :: SList xs of
      SNil  -> Nil
      SCons -> (i . head            ::                   GPrism r w (NP I (x : xs1)) x)
               :*
               (hliftA (. tail) (np :: SListI xs1 => NP (GPrism r w (NP I      xs1))  xs1)
                                    :: SListI xs1 => NP (GPrism r w (NP I (x : xs1))) xs1)

sop :: (Arrow r, ArrowApply w) => GPrism r w (SOP I xss) (NS (NP I) xss)
sop = fromIso $ Iso (\(SOP x)     -> x)  SOP

{-------------------------------------------------------------------------------
  Generalized lenses for representation types
-------------------------------------------------------------------------------}

i :: (Arrow r, ArrowApply w) => GPrism r w (I a) a
i = fromIso $ Iso unI I

head :: (Arrow r, ArrowApply w) => GPrism r w (NP f (x ': xs)) (f x)
head = fromLens $ Lens.lens
  (\(x :* _) -> x)
  (\(f, x :* xs) -> (f x :* xs))

rep :: (Arrow r, ArrowApply w, Generic a) => GPrism r w a (Rep a)
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

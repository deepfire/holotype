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
import Data.Label.Mono (Lens)
import Data.Label.Point (Iso(..))
import qualified Data.Label.Mono as Lens

import Generics.SOP

{-------------------------------------------------------------------------------
  Generalized lens using two categories
-------------------------------------------------------------------------------}

-- | GPrism generalizes a monomorphic lens by allowing for different categories
-- for the getter and modifier
data GPrism r w a b = GPrism (r a b) (w (w b b, a) a)

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
  Generic computation of all lenses for a record type
-------------------------------------------------------------------------------}

type family Last (x :: [k]) where
  Last (x: '[]) = x
  Last (x:  xs) = Last xs

-- sop' :: (Arrow r, ArrowApply w) => GPrism r w (SOP f xss) (NP f (Last xss))
-- sop' = fromIso $ Iso
--   (let r :: SListI yss => NS (NP f) yss -> NP f (Last yss)
--        r x = case x of
--          Z x  -> (undefined :: ())
--          S xs -> undefined
--        -- r (Z x)  = x :* Nil
--        -- r (S xs) = undefined
--    in \(SOP x)-> r x)
--   (let w :: NP (NP f) xss -> NS (NP f) xss
--        w (x :* Nil) = Z x
--        w (x :* xs)  = S (w xs)
--    in SOP . w)

glenses :: forall r w a xs. (Generic a, Code a ~ '[xs], Arrow r, ArrowApply w) => NP (GPrism r w a) xs
glenses = case sList :: SList (Code a) of
            SCons -> hliftA (\l ->
                               (l   :: GPrism r w                 (NP I xs) _) .
                               (sop :: GPrism r w   (SOP I '[xs]) (NP I xs)) .
                               (rep :: GPrism r w a (Rep      a)))
                     (np :: NP (GPrism r w (NP I xs)) xs)
-- #if __GLASGOW_HASKELL__ < 800
--             _     -> error "inaccessible"
-- #endif

sop' :: (Arrow r, ArrowApply w) => GPrism r w (SOP f xss) (NP (NP f) xss)
sop' = undefined

np' :: (Arrow r, ArrowApply w) => NP (NP (GPrism r w (NP (NP I) xss))) xss
np' = undefined

gprisms :: forall r w a xss. (Generic a, Code a ~ xss, Arrow r, ArrowApply w) => NP (NP (GPrism r w a)) xss
gprisms = go (sList :: SList (Code a))
  where
    go :: (SListI yss, All SListI yss) => SList yss -> NP (NP (GPrism r w a)) yss
    go x = case x of
      SNil -> Nil
      SCons -> -- (undefined :: NP (NP (GPrism r w a)) (x : xs))
        ((hliftA (\(l :: GPrism r w (NP (NP I) xss) _) ->
                   (l       :: GPrism r w               (NP (NP I) xss)  _) .
                   (sop'    :: GPrism r w   (SOP I xss) (NP (NP I) xss)) .
                   (rep     :: GPrism r w a (SOP I xss)))
                 np') :: NP (NP (GPrism r w a)) (x : xs))
        :*
        (undefined :: NP (NP (GPrism r w a)) xs)
        -- np'  :: NP (NP I) xss -> x
        -- rep  :: a -> SOP f xss
        -- sop' :: SOP f xss -> NP (NP f) xss
        -- l    :: NP (NP I) xss
        -- (hliftA ((\l -> ( l :: GPrism r w (NP (NP I) xss) _) . sop' . rep)
        --          :: _)
        --   np' :: _)
        --    --(np' :: NP (NP (GPrism r w (NP (NP I) xss))) xss))
        --  :* _ --go sList

{-------------------------------------------------------------------------------
  Generalized lenses for representation types
-------------------------------------------------------------------------------}

data CPrisms r w xs = CPrisms { unCPrisms :: NP (GPrism r w (NP I xs)) xs }

np :: forall r w xs. (Arrow r, ArrowApply w, SListI xs) => NP (GPrism r w (NP I xs)) xs
np = case sList :: SList xs of
      SNil  -> Nil
      SCons -> i . head :* hliftA (. tail) np

nx :: forall r w xss. (Arrow r, ArrowApply w, All SListI xss) => NP (CPrisms r w) xss
nx = case sList :: SList xss of
       SNil  -> Nil
       SCons -> CPrisms np :* nx

rep :: (Arrow r, ArrowApply w, Generic a) => GPrism r w a (Rep a)
rep = fromIso $ Iso from to

sop  :: (Arrow r, ArrowApply w) => GPrism r w (SOP f '[xs]) (NP f xs)
sop  = fromIso $ Iso
  (\(SOP (Z x)) -> x)
  (SOP . Z)

head :: (Arrow r, ArrowApply w) => GPrism r w (NP f (x ': xs)) (f x)
head = fromLens $ Lens.lens (\(x :* _) -> x) (\(f, x :* xs) -> (f x :* xs))

tail :: (Arrow r, ArrowApply w) => GPrism r w (NP f (x ': xs)) (NP f xs)
tail = fromLens $ Lens.lens (\(_ :* xs) -> xs) (\(f, x :* xs) -> (x :* f xs))

i :: (Arrow r, ArrowApply w) => GPrism r w (I a) a
i = fromIso $ Iso unI I

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

const :: Arrow arr => c -> arr b c
const a = arr (\_ -> a)

curry :: Arrow cat => cat (a, b) c -> (a -> cat b c)
curry m a = m . (const a &&& id)

uncurry :: ArrowApply cat => (a -> cat b c) -> cat (a, b) c
uncurry a = app . arr (first a)

data Ex = Num Int | Add { left :: Ex, right :: Ex } | Yay { x :: Ex }
instance Generic Ex where
  type Code Ex              = '[ '[Int], '[Ex, Ex], '[Ex]]
  from (Num n)              = SOP $ (Z (I n :* Nil)            :: NS (NP I) ('[Int] : xs))
  from (Add e f)            = SOP $ (S (Z (I e :* I f :* Nil)) :: NS (NP I) (x : '[Ex, Ex] : xs))
  from (Yay x)              = SOP $ (S (S (Z (I x :* Nil)))    :: NS (NP I) (x0 : x1 : '[Ex] : xs))
  to (SOP (Z (I n :* Nil))) = Num n
  to (SOP (S (Z (I e :* I f :* Nil)))) = Add e f

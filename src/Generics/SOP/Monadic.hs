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
--{-# OPTIONS_GHC -Wextra -fplugin ThinErr #-}
module Generics.SOP.Monadic
  ( Record(..), SumChoiceT, ReadFieldT
  , Result
  -- , HasReadField(..)
  , recover

  -- reexports
  , (:.:)(..)
  , DatatypeInfo(..), ConstructorInfo(..), FieldInfo(..)
  )
where

import qualified Data.List                        as L
import           Data.Maybe
import           Data.String
import           Data.Text                           (Text, pack, unpack, toLower, drop, take)
import           Data.Typeable
import           GHC.Types                           (Constraint, Type)
import           Prelude                      hiding (read, take, drop, length)
import           Prelude.Unicode
import           Text.Printf

import           GHC.Stack
import           Generics.SOP                        ((:.:)(..)
                                                     , NP(..), SOP(..), POP(..), I(..), K(..), Code, All, All2, Top
                                                     , HasDatatypeInfo(..), DatatypeInfo(..), ConstructorInfo(..), FieldInfo(..), SListI
                                                     , hcliftA2, hliftA, hsequence
                                                     )
import           Generics.SOP.NP                     (pure_NP)
import qualified Generics.SOP                     as SOP
import qualified Generics.SOP.NP                  as SOP
import qualified Generics.SOP.Traversal           as SOP


-- * Somewhat generic
enumerate ∷ SListI xs ⇒ NP ConstructorInfo xs → NP (((,) Int) :.: ConstructorInfo) xs
enumerate cs = SOP.hliftA2 (\c (K n)→ Comp (n, c)) cs (fromJust $ SOP.fromList $ L.take (SOP.lengthSList cs) [0..])


data family Result    t s ∷ Type

type SumChoiceT        = Int
type ADTChoice   m xss = m SumChoiceT

class ( Monad m, SOP.Generic a, HasDatatypeInfo a
      ) ⇒ Record t m a where
  type RecordCtx t a ∷ Type
  restoreChoice     ∷ (HasCallStack, Monad m)
                    ⇒ Proxy (t, a)             -- ^ ..the type of the record
                    → RecordCtx t a            -- ^ Givent the record context
                    → ADTChoice m xss          -- ^ action to determine the record's constructor index.


type ReadFieldT c t m u a xss xs
  = (All c xs, c a)
  ⇒ Proxy c
  → Proxy (t, u, a)
  → RecordCtx t u
  → DatatypeInfo xss
  → SumChoiceT
  → ConstructorInfo xs
  → FieldInfo a
  → (u → a)
  → (m :.: Result t) a

recover
  ∷ ∀ a (c ∷ Type → Constraint) (t ∷ Type) m xss.
    ( Record t m a, Code a ~ xss, HasDatatypeInfo a
    , All2 c xss
    , HasCallStack, Monad m, Applicative (Result t))
  ⇒ Proxy c
  → Proxy (t, a)
  → RecordCtx t a
  → (forall f xs. (c f, All c xs, All2 c xss) ⇒ ReadFieldT c t m a f xss xs)
  → (m :.: Result t) a
recover pC pTA ctxR f = let dti = datatypeInfo (Proxy @a) ∷ DatatypeInfo xss
  in Comp $
  case dti of
    ADT _moduleName typeName cInfos → do
      choice ← restoreChoice pTA ctxR
      let pop       ∷ POP (m :.: Result t) xss     = recover' pC pTA f ctxR $ (datatypeInfo (Proxy @a) ∷ DatatypeInfo xss)
          ct        ∷ SOP (m :.: Result t) xss     = (!! choice) $ SOP.apInjs_POP pop
          Comp msop ∷ (m :.: Result t) (SOP I xss) = hsequence ct
      case SOP.sList ∷ SOP.SList xss of
        SOP.SCons → (SOP.to <$>) <$> msop
    Newtype _moduleName typeName cInfo → do
      let nCInfos -- ~∷ NP ((,) Int :.: ConstructorInfo) '[ '[x]]
            = enumerate $ cInfo :* Nil
          sop     ∷                  SOP (m :.: Result t) xss  = SOP.SOP $ SOP.Z $
            recoverCtor pC pTA f ctxR dti
            (SOP.hd nCInfos)
            (SOP.hd ((SOP.gtraversals -- ~∷ NP (NP (SOP.GTraversal (→) (→) s)) '[ '[x]]
                        )))
          Comp mdsop ∷ (m :.: Result t) (SOP I               xss) = hsequence sop
      (SOP.to <$>) <$> mdsop

recover'
  ∷ ∀ a (c ∷ Type → Constraint) (t ∷ Type) m xss.
    ( Record t m a, Code a ~ xss
    , All2 c xss
    , HasCallStack, Monad m)
  ⇒ Proxy c
  → Proxy (t, a)
  → (forall f xs. c f ⇒ ReadFieldT c t m a f xss xs)
  → RecordCtx t a
  → DatatypeInfo xss
  → POP (m :.: Result t) xss
recover' pC pTA f ctxR dti@(ADT _ name cs) =
  POP $ SOP.hcliftA2 (Proxy @(All c))
                     --(Proxy @(All (HasReadField t m a)))
        (recoverCtor pC pTA f ctxR dti)
        (enumerate cs)
        (SOP.gtraversals ∷ NP (NP (SOP.GTraversal (→) (→) a)) xss)
recover' _ _ _ _ _ = error "Non-ADTs not supported."

-- * 1. Extract the constructor's product of field names
--   2. Feed that to the field-name→action interpreter
recoverCtor
  ∷ ∀ a (c ∷ Type → Constraint) (t ∷ Type) m xss xs.
    ( Record t m a, Code a ~ xss
    , All c xs
    , HasCallStack, Monad m)
  ⇒ Proxy c
  → Proxy (t, a)
  → (forall f. c f ⇒ ReadFieldT c t m a f xss xs)
  → RecordCtx t a
  → DatatypeInfo xss
  → (((,) SumChoiceT) :.: ConstructorInfo) xs
  → NP (SOP.GTraversal (→) (→) a) xs
  → NP (m :.: Result t) xs
recoverCtor pC pTA f ctxR dti (Comp (consNr, consi@(Record _ finfos))) travs = recoverFields pC pTA f ctxR dti consNr consi travs finfos
recoverCtor pC pTA f ctxR dti (Comp (consNr, consi@Constructor{}))     travs = recoverFields pC pTA f ctxR dti consNr consi travs (SOP.hpure (FieldInfo ""))
recoverCtor _ _ _ _ (ADT _ name _) _ _ =
  error $ printf "Infix ADTs not supported: type %s." name

-- * Key part:  NP (K Text) xs → NP m xs
--   convert a product of field names to a product of monadic actions yielding 'a'
recoverFields
  ∷ ∀ (c ∷ Type → Constraint) (t ∷ Type) m u xss xs.
    ( Record t m u, Code u ~ xss
    , All c xs
    , SListI xs
    , HasCallStack, Monad m)
  ⇒ Proxy c
  → Proxy (t, u)
  → (forall a. ReadFieldT c t m u a xss xs)
  → RecordCtx t u
  → DatatypeInfo xss
  → SumChoiceT
  → ConstructorInfo xs
  → NP (SOP.GTraversal (→) (→) u) xs
  → NP (FieldInfo) xs
  → NP (m :.: Result t) xs
recoverFields pC _pTU f ctxR dtinfo consNr cinfo traversals finfos =
  hcliftA2 pC
  (recoverField ctxR dtinfo consNr cinfo)
  finfos
  traversals
  where
    recoverField ∷ ∀ a. (c a)
                 ⇒ RecordCtx t u
                 → DatatypeInfo xss
                 → SumChoiceT
                 → ConstructorInfo xs
                 → FieldInfo a
                 → SOP.GTraversal (→) (→) u a
                 → (m :.: Result t) a
    recoverField ctxR dtinfo consNr cinfo finfo trav =
      f (Proxy @c) (Proxy @(t, u, a))
      ctxR dtinfo consNr cinfo finfo (SOP.get trav ∷ u → a)

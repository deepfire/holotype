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
  ( HasReadField(..)
  , Record(..), SumChoiceT
  , Result
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
                                                     , NP(..), SOP(..), POP(..), I(..), K(..), Code, All, All2
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

class (Monad m) ⇒ HasReadField t m u a where
  readField         ∷ (HasCallStack)
                    ⇒ Proxy (t, u, a)
                    → RecordCtx t u
                    → DatatypeInfo xss
                    → SumChoiceT
                    → ConstructorInfo xs
                    → FieldInfo a
                    → (u → a)
                    → (m :.: Result t) a

type SumChoiceT        = Int
type ADTChoice   m xss = m SumChoiceT

class ( Monad m, SOP.Generic a, HasDatatypeInfo a
      ) ⇒ Record t m a where
  type RecordCtx t a ∷ Type
  restoreChoice     ∷ (HasCallStack, Monad m)
                    ⇒ Proxy (t, a)             -- ^ ..the type of the record
                    → RecordCtx t a            -- ^ Givent the record context
                    → ADTChoice m xss          -- ^ action to determine the record's constructor index.


recover
  ∷ ∀ (t ∷ Type) m a xss xs.
    ( Record t m a, Code a ~ xss, All2 (HasReadField t m a) xss, HasDatatypeInfo a
    , HasCallStack, Monad m, Applicative (Result t))
  ⇒ Proxy (t, a)
  → RecordCtx t a
  → (m :.: Result t) a
recover _pTA ctxR = let dti = datatypeInfo (Proxy @a) ∷ DatatypeInfo xss
  in Comp $
  case dti of
    ADT _moduleName typeName cInfos → do
      choice ← restoreChoice (Proxy @(t, a)) ctxR
      let pop       ∷ POP (m :.: Result t) xss     = recover' (Proxy @(t, a)) ctxR $ (datatypeInfo (Proxy @a) ∷ DatatypeInfo xss)
          ct        ∷ SOP (m :.: Result t) xss     = (!! choice) $ SOP.apInjs_POP pop
          Comp msop ∷ (m :.: Result t) (SOP I xss) = hsequence ct
      case SOP.sList ∷ SOP.SList xss of
        SOP.SCons → (SOP.to <$>) <$> msop
    Newtype _moduleName typeName cInfo → do
      let nCInfos -- ~∷ NP ((,) Int :.: ConstructorInfo) '[ '[x]]
            = enumerate $ cInfo :* Nil
          sop     ∷                  SOP (m :.: Result t) xss  = SOP.SOP $ SOP.Z $
            recoverCtor (Proxy @(t, a)) ctxR dti
            (SOP.hd nCInfos)
            (SOP.hd ((SOP.gtraversals -- ~∷ NP (NP (SOP.GTraversal (→) (→) s)) '[ '[x]]
                        )))
          Comp mdsop ∷ (m :.: Result t) (SOP I               xss) = hsequence sop
      (SOP.to <$>) <$> mdsop

recover'
  ∷ ∀ (t ∷ Type) m a xss xs.
    ( Record t m a, Code a ~ xss, All2 (HasReadField t m a) xss
    , HasCallStack, Monad m)
  ⇒ Proxy (t, a)
  → RecordCtx t a
  → DatatypeInfo xss
  → POP (m :.: Result t) xss
recover' pTDS ctxR dti@(ADT _ name cs) =
  POP $ SOP.hcliftA2 (Proxy @(All (HasReadField t m a)))
        (recoverCtor (Proxy @(t, a)) ctxR dti)
        (enumerate cs)
        (SOP.gtraversals ∷ NP (NP (SOP.GTraversal (→) (→) a)) xss)
recover' _ _ _ = error "Non-ADTs not supported."

-- * 1. Extract the constructor's product of field names
--   2. Feed that to the field-name→action interpreter
recoverCtor
  ∷ ∀ (t ∷ Type) m a xss xs.
    ( Record t m a, Code a ~ xss, All (HasReadField t m a) xs
    , HasCallStack, Monad m)
  ⇒ Proxy (t, a)
  → RecordCtx t a
  → DatatypeInfo xss
  → (((,) SumChoiceT) :.: ConstructorInfo) xs
  → NP (SOP.GTraversal (→) (→) a) xs
  → NP (m :.: Result t) xs
recoverCtor pTA ctxR dti (Comp (consNr, consi@(Record _ finfos))) travs = recoverFields pTA ctxR dti consNr consi travs finfos
recoverCtor pTA ctxR dti (Comp (consNr, consi@Constructor{}))     travs = recoverFields pTA ctxR dti consNr consi travs (SOP.hpure (FieldInfo ""))
recoverCtor _ _ (ADT _ name _) _ _ =
  error $ printf "Infix ADTs not supported: type %s." name

-- * Key part:  NP (K Text) xs → NP m xs
--   convert a product of field names to a product of monadic actions yielding 'a'
recoverFields
  ∷ ∀ (t ∷ Type) m u xss xs.
    ( Record t m u, Code u ~ xss, All (HasReadField t m u) xs
    , SListI xs
    , HasCallStack, Monad m)
  ⇒ Proxy (t, u)
  → RecordCtx t u
  → DatatypeInfo xss
  → SumChoiceT
  → ConstructorInfo xs
  → NP (SOP.GTraversal (→) (→) u) xs
  → NP (FieldInfo) xs
  → NP (m :.: Result t) xs
recoverFields _pTU ctxR dtinfo consNr cinfo traversals finfos =
  hcliftA2 (Proxy @(HasReadField t m u))
  (recoverField ctxR dtinfo consNr cinfo)
  finfos
  traversals
  where
    recoverField ∷ ∀ a. (HasReadField t m u a)
                 ⇒ RecordCtx t u
                 → DatatypeInfo xss
                 → SumChoiceT
                 → ConstructorInfo xs
                 → FieldInfo a
                 → SOP.GTraversal (→) (→) u a
                 → (m :.: Result t) a
    recoverField ctxR dtinfo consNr cinfo finfo trav =
      readField (Proxy @(t, u, a))
      ctxR dtinfo consNr cinfo finfo (SOP.get trav ∷ u → a)

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
module MRecord
  ( Field(..), Record(..)
  , CtxRecord(..), ConsCtx(..), FieldCtx(..)
  , FieldName(..)
  , recover

  -- reёxports
  , (:.), O(..)
  )
where

import           Control.Compose
import           Control.Exception
import           Control.Lens                        ((<&>))
import           Data.Functor.Identity
import           Data.Function                       ((&))
import           Data.Bool
import qualified Data.List                        as L
import           Data.Map                            (Map)
import qualified Data.Map                         as Map
import           Data.Maybe
import           Data.Set                            (Set)
import qualified Data.Set                         as Set
import           Data.String
import           Data.Text                           (Text, pack, unpack, toLower, toUpper, drop, take, length, isSuffixOf)
import qualified Data.Text                        as T
import           Data.Typeable
import           GHC.Types                           (Type)
import qualified GHC.Types                        as Type
import           Prelude                      hiding (read, take, drop, length)
import           Prelude.Unicode
import           Text.Printf

import           Data.Proxy
import           GHC.Generics                        (Generic)
import qualified GHC.Generics                     as GHC
import           GHC.Stack
import           Generics.SOP                        (Rep, NS(..), NP(..), SOP(..), POP(..), I(..), K(..), Code, All, All2
                                                     ,HasDatatypeInfo(..), DatatypeInfo(..), FieldInfo(..), ConstructorInfo(..), SListI
                                                     ,from, to, hcollapse, hcliftA2, hliftA, unI, hsequence, hcpure, hpure
                                                     , fn
                                                     , hcliftA
                                                     , AllN, HAp, hap
                                                     , SList
                                                     )
import qualified Generics.SOP                     as SOP
import qualified Generics.SOP.NS                  as SOP
import qualified Generics.SOP.NP                  as SOP
import qualified Generics.SOP.GGP                 as SOP

import           Debug.Trace (trace)



data NConstructorInfo xs where
  NC ∷ ConstructorInfo xs → Int →  NConstructorInfo xs

enumerate ∷ SListI xs ⇒ NP ConstructorInfo xs → NP NConstructorInfo xs
enumerate cs = SOP.hliftA2 (\c (K n)→ NC c n) cs (fromJust $ SOP.fromList $ L.take (SOP.lengthSList cs) [0..])

mapFields ∷ ∀ cst a c xs. (SOP.Generic a, SOP.HasDatatypeInfo a, Code a ~ '[xs], All cst xs)
          ⇒ (∀ b . cst b ⇒ Text → b → c) → a → [c]
mapFields f x = case datatypeInfo (Proxy ∷ Proxy a) of
                  (ADT _ _ ((Record _ fi) :* Nil)) →
                    hcollapse $ hcliftA2 (Proxy ∷ Proxy cst)
                                (\(FieldInfo fi) (I val)→
                                   K $ f (pack fi) val)
                                (fi ∷ NP FieldInfo xs)
                                (SOP.unZ ∘ SOP.unSOP $ from x)
                  _ → error "Non-ADTs/non-Records/sums not supported."



-- * Given 'a', a record type, recover

type family ConsCtx  t                   u ∷ Type
type family FieldCtx t (m ∷ Type → Type) s ∷ Type

class ( Monad m)
  ⇒ Field t m u s where
  fieldCtx          ∷ Proxy t
                    → Proxy m
                    → Proxy (u, s)
                    → ConsCtx t u
                    → (u → s)
                    → FieldCtx t m s
  readField         ∷ ∀ c. (HasCallStack, c)
                    ⇒ Proxy t
                    → Proxy c
                    → Proxy m                 -- ^ Given the result type
                    → Proxy u                 -- ^ Given the result type
                    → FieldCtx t m s          -- ^ ..the recovery context
                    → FieldName               -- ^ ..the field name
                    → m s  -- ^ restore the point.
  -- default readField    ∷ (CtxRecord ctx a, Code a ~ xss, All2 (RestoreField ctx) xss, HasCallStack, Typeable a, Monad m)
  default readField ∷ (HasCallStack, c
                      , CtxRecord t m s
                      , SOP.HasDatatypeInfo s, SOP.Generic s, GHC.Generic s
                      , Code s ~ xss
                      , All2 (FieldConstraint t m s) xss
                      -- , ConsCtx t s ~ RecordCtx s -- XXX: not sure why this was needed previously
                      )
                    ⇒ Proxy t
                    → Proxy c
                    → Proxy m
                    → Proxy u
                    → FieldCtx t m s          -- ^ ..the recovery context
                    → FieldName
                    → m s
  readField pT pC pM pU fctx _ = do
    recover pT pC pM (Proxy @s) (recordCtx pT pM (Proxy @s))

-- * Field's type is constrained to be:
--   1. recoverable
--   2. having its recovery context tied to the containing record's recovery context
class    (Field t m s a) ⇒ FieldConstraint t m s a where
instance (Field t m s a) ⇒ FieldConstraint t m s a where

class ( Monad m
      , SOP.Generic s, SOP.HasDatatypeInfo s)
      ⇒ Record t m s where
  prefixChars     ∷ Proxy (t,m s,s) -- ^ Given the type of the record
                  → Int             -- ^ ..produce the number of prefix characters to ignore.
  nameMap         ∷ Proxy (t,m s,s) -- ^ Given the type of the record
                  → [(Text, Text)]  -- ^ produce the partial field renaming map.
  toFieldName     ∷ Proxy (t,m s,s) -- ^ Given the type of the record
                  → Text            -- ^ ..the record's field name
                  → FieldName       -- ^ produce the serialised field name.
  -- *
  nameMap         = const []
  toFieldName r x =
    FieldName $ maybeRemap $ dropDetitle (prefixChars r) x
    where maybeRemap x = maybe x id (lookup x $ nameMap (Proxy @(t,m s,s)))
          dropDetitle ∷ Int → Text → Text
          dropDetitle n (drop 2 → x) = toLower (take 1 x) <> drop 1 x



newtype FieldName = FieldName { fromFieldName ∷ Text } deriving (Eq, IsString, Ord, Show)

type ADTChoiceT        = Int
type ADTChoice   m xss = m ADTChoiceT

class ( Monad m
      , SOP.HasDatatypeInfo s, SOP.Generic s, Record t m s, Monad m) ⇒
      CtxRecord  t m s where
  type RecordCtx t   s ∷ Type
  recordCtx         ∷ Proxy t         -- ^ ..the type of the record
                    → Proxy m         -- ^ ..the type of the record
                    → Proxy s         -- ^ ..the type of the record
                    → RecordCtx t s
  consCtx           ∷ Proxy t         -- ^ ..the type of the record
                    → Proxy m         -- ^ ..the type of the record
                    → Proxy s         -- ^ ..the type of the record
                    → RecordCtx t s   -- ^ Given the record context
                    → Text            -- ^ ..the constructor's name
                    → ADTChoiceT      -- ^ ..its number
                    → ConsCtx t s     -- ^ produce the constructor-specific context.
  -- * Defaulted methods
  restoreChoice     ∷ (HasCallStack, Monad m)
                    ⇒ RecordCtx t s   -- ^ Givent the record context
                    → Proxy (t, s)    -- ^ ..the type of the record
                    → ADTChoice m xss -- ^ action to determine the record's constructor index.
  ctxSwitch         ∷ (HasCallStack, Monad m)
                    ⇒ RecordCtx t s   -- ^ Given the type of the record
                    → m s  -- ^ action to update the context, before each field's presence test/recovery.


class Interpret a where
  -- XXX: sadly unused
  fromText ∷ Text → a
  toText   ∷ a → Text


recover  ∷ ∀ (t ∷ Type) c m s xss.
           ( c, CtxRecord t m s
           , SOP.HasDatatypeInfo s
           , Code s ~ xss
           , All2 (FieldConstraint t m s) xss
           , HasCallStack, Monad m)
         ⇒ Proxy t
         → Proxy c
         → Proxy m
         → Proxy s
         → RecordCtx t s
         → m s
recover pT pC pM pS ctx = do
    choice ← restoreChoice ctx (Proxy @(t, s))
    let pop    ∷ POP m xss          = recover' pT pC pS ctx $ (datatypeInfo (Proxy @s) ∷ DatatypeInfo (Code s))
        ct     ∷ SOP m (Code s)     = (!! choice) $ SOP.apInjs_POP pop
        msop   ∷ m (SOP I (Code s)) = hsequence ct
    SOP.to <$> msop

-- * 1. Construct a POP, mapping the (NConstructorInfo → NP m) interpreter over its rows
--   3. Return the resultant POP of actions
recover'
  ∷ ∀ (t ∷ Type) c m s xss.
    ( c, CtxRecord t m s
    , Code s ~ xss, All SListI xss
    , All2 (FieldConstraint t m s) xss
    , HasCallStack, Monad m)
  ⇒ Proxy t → Proxy c → Proxy s → RecordCtx t s → DatatypeInfo xss → POP m xss

recover' pT pC pD ctxR (ADT _ name cs) =
  POP $ hcliftA (Proxy @(All (FieldConstraint t m s)))
  (recoverCtor pT pC pD ctxR (pack name)) (enumerate cs)

recover' _ _ _ _ _ = error "Non-ADTs not supported."

-- * 1. Extract the constructor's product of field names
--   2. Feed that to the field-name→action interpreter
recoverCtor
  ∷ ∀ (t ∷ Type) c m s xs.
    ( c, CtxRecord t m s
    , All (FieldConstraint t m s) xs
    , HasCallStack, Monad m)
  ⇒ Proxy t → Proxy c → Proxy s → RecordCtx t s → Text → NConstructorInfo xs → NP m xs

recoverCtor pT pC pS ctxR _ (NC (Record consName fis) consNr) =
  recoverFields pT pC pS ctxR (consCtx pT (Proxy @m) (Proxy @s) ctxR (pack consName) consNr) (pack consName) consNr $ hliftA (K ∘ pack ∘ SOP.fieldName) fis

recoverCtor _ _ _ _ name _ =
  error $ printf "Non-Record (plain Constructor, Infix) ADTs not supported: type %s." (unpack name)

-- * Key part:  NP (K Text) xs → NP m xs
--   convert a product of field names to a product of monadic actions yielding 'a'
recoverFields
  ∷ ∀ (t ∷ Type) c m s xs.
    ( c, CtxRecord t m s
    , All (FieldConstraint t m s) xs
    , SListI xs
    , HasCallStack, Monad m)
  ⇒ Proxy t → Proxy c → Proxy s → RecordCtx t s → ConsCtx t s → Text → Int → NP (K Text) xs → NP m xs

recoverFields pT pC pD ctxR ctxC consName consNr fss =
  hcliftA (Proxy @(FieldConstraint t m s)) recoverField (fss ∷ NP (K Text) xs)
  where
    recoverField ∷ ∀ fs. (Field t m s fs)
                 ⇒ K Text fs
                 → m fs
    recoverField (K fi) =
      readField pT pC (Proxy @m) (Proxy @s) (fieldCtx pT (Proxy @m) (Proxy @(s, fs)) ctxC (⊥)) (toFieldName (Proxy @(t,m s,s)) fi)


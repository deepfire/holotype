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
  , Derived(..)
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
import qualified Data.Label.Mono                  as FLM
import qualified Data.Label.Poly                  as FLP
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
import qualified Generics.SOP.Lens                as SOP

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

data family Derived  t                   s ∷ Type
type family ConsCtx    (d ∷ Type)          = (r ∷ Type)
type family FieldCtx t (m ∷ Type → Type) s ∷ Type

class ( Monad m
      , d ~ Derived t)
  ⇒ Field t m d u s where
  fieldCtx          ∷ Proxy (t, u, s, m s)
                    → ConsCtx (d u)
                    → (u → s)
                    → FieldCtx t m s
  readField         ∷ ∀ c. (HasCallStack, c)
                    ⇒ Proxy c
                    → Proxy (t, u)
                    → FieldCtx t m s          -- ^ ..the recovery context
                    → FieldName               -- ^ ..the field name
                    → (m :. d) s  -- ^ restore the point.
  -- default readField    ∷ (CtxRecord ctx a, Code a ~ xss, All2 (RestoreField ctx) xss, HasCallStack, Typeable a, Monad m)
  default readField ∷ (HasCallStack, c
                      , CtxRecord t m d s
                      , SOP.HasDatatypeInfo s, SOP.Generic s, GHC.Generic s
                      , Code s ~ xss, xss ~ '[xs]
                      , All2 (FieldConstraint t m d s) xss
                      , FieldCtx t m s ~ RecordCtx d s
                      , Applicative d
                      )
                    ⇒ Proxy c
                    → Proxy (t, u)
                    → FieldCtx t m s          -- ^ ..the recovery context
                    → FieldName
                    → (m :. d) s
  readField pC pTUMS fctx _ =
    recover pC (Proxy @(t, s)) fctx

-- * Field's type is constrained to be:
--   1. recoverable
--   2. having its recovery context tied to the containing record's recovery context
class     ( Field t m d s a) ⇒
  FieldConstraint t m d s a where
instance  ( Field t m d s a) ⇒
  FieldConstraint t m d s a where

class ( Monad m, d ~ Derived t
      , SOP.Generic s, SOP.HasDatatypeInfo s)
      ⇒ Record t m d s where
  prefixChars       ∷ Proxy (s, d s,m s) -- ^ Given the type of the record
                    → Int                -- ^ ..produce the number of prefix characters to ignore.
  nameMap           ∷ Proxy (s, d s,m s) -- ^ Given the type of the record
                    → [(Text, Text)]     -- ^ produce the partial field renaming map.
  toFieldName       ∷ Proxy (s, d s,m s) -- ^ Given the type of the record
                    → Text               -- ^ ..the record's field name
                    → FieldName          -- ^ produce the serialised field name.
  -- *
  nameMap           = const []
  toFieldName r x   =
    FieldName $ maybeRemap $ dropDetitle (prefixChars r) x
    where maybeRemap x = maybe x id (lookup x $ nameMap r)
          dropDetitle ∷ Int → Text → Text
          dropDetitle n (drop n → x) = toLower (take 1 x) <> drop 1 x



newtype FieldName = FieldName { fromFieldName ∷ Text } deriving (Eq, IsString, Ord, Show)

type ADTChoiceT        = Int
type ADTChoice   m xss = m ADTChoiceT

class ( Monad m, d ~ Derived t
      , SOP.HasDatatypeInfo s, SOP.Generic s, Record t m d s, Monad m) ⇒
      CtxRecord t m d s where
  type RecordCtx   d s ∷ Type
  consCtx           ∷ Proxy (t, d s, m s)
                    → Text                  -- ^ Given the constructor's name
                    → ADTChoiceT            -- ^ ..its number
                    → RecordCtx  d s        -- ^ ..the record context
                    → ConsCtx   (d s)       -- ^ produce the constructor-specific context.
  -- * Defaulted methods
  restoreChoice     ∷ (HasCallStack, Monad m)
                    ⇒ Proxy (t, s, d s)     -- ^ ..the type of the record
                    → RecordCtx d s         -- ^ Givent the record context
                    → ADTChoice m xss       -- ^ action to determine the record's constructor index.
  ctxSwitch         ∷ (HasCallStack, Monad m)
                    ⇒ RecordCtx d s         -- ^ Given the type of the record
                    → m (RecordCtx d s)     -- ^ action to update the context, before each field's presence test/recovery.



class Interpret a where
  -- XXX: sadly unused
  fromText ∷ Text → a
  toText   ∷ a → Text



recover  ∷ ∀ (t ∷ Type) c m d s xss xs.
           ( c, CtxRecord t m d s
           , SOP.HasDatatypeInfo s
           , Code s ~ xss, xss ~ '[xs]
           , All2 (FieldConstraint t m d s) xss
           , HasCallStack, Monad m, Applicative d)
         ⇒ Proxy c
         → Proxy (t, s)
         → RecordCtx d s
         → (m :. d) s
recover pC pT ctxR = O $ do
    -- choice ← restoreChoice (Proxy @(t, s, d s)) ctx
    -- let pop    ∷ POP (m :. d) xss          = recover' pC (Proxy @(t, d s)) ctxR $ (datatypeInfo (Proxy @s) ∷ DatatypeInfo (Code s))
    --     ct     ∷ SOP (m :. d) (Code s)     = (!! choice) $ SOP.apInjs_POP pop
    --     O msop ∷ (m :. d) (SOP I (Code s)) = hsequence ct
    -- case SOP.sList ∷ SList (Code s) of
    --   SOP.SCons → (SOP.to <$>) <$> msop
    let ADT _ name cinfos = datatypeInfo (Proxy @s) ∷ DatatypeInfo (Code s)
        nc     ∷ NConstructorInfo xs       = SOP.hd $ enumerate cinfos
        ct     ∷ NP (m :. d)      xs       = recoverCtor pC (Proxy @(t, d s)) ctxR (pack name) nc
        O msop ∷ (m :. d) (SOP I (Code s)) = hsequence (SOP.SOP $ SOP.Z ct)
    (SOP.to <$>) <$> msop

-- * 1. Construct a POP, mapping the (NConstructorInfo → NP m) interpreter over its rows
--   3. Return the resultant POP of actions
-- recover'
--   ∷ ∀ (t ∷ Type) c m (d ∷ Type → Type) s xss xs.
--     ( c, CtxRecord t m d s
--     , Code s ~ xss, xss ~ '[xs], All SListI xss
--     , All2 (FieldConstraint t m d s) xss
--     , HasCallStack, Monad m)
--   ⇒ Proxy c → Proxy (t, d s) → RecordCtx d s → DatatypeInfo xss
--   → POP (m :. d) xss
-- recover' pC pTDS ctxR (ADT _ name cs) =
--   POP $ hcliftA (Proxy @(All (FieldConstraint t m d s)))
--         (recoverCtor pC (Proxy @(t, d s)) ctxR (pack name)) (enumerate cs)
-- recover' _ _ _ _ = error "Non-ADTs not supported."

-- * 1. Extract the constructor's product of field names
--   2. Feed that to the field-name→action interpreter
recoverCtor
  ∷ ∀ (t ∷ Type) c m (d ∷ Type → Type) s xs.
    ( c, CtxRecord t m d s, Code s ~ '[xs]
    , All (FieldConstraint t m d s) xs
    , HasCallStack, Monad m)
  ⇒ Proxy c → Proxy (t, d s) → RecordCtx d s → Text → NConstructorInfo xs
  → NP (m :. d) xs
recoverCtor pC pTDS ctxR _ (NC (Record consName fis) consNr) =
  recoverFields pC pTDS ctxR (consCtx (Proxy @(t, d s, m s)) (pack consName) consNr ctxR) (pack consName) consNr $ hliftA (K ∘ pack ∘ SOP.fieldName) fis

recoverCtor _ _ _ name _ =
  error $ printf "Non-Record (plain Constructor, Infix) ADTs not supported: type %s." (unpack name)

-- * Key part:  NP (K Text) xs → NP m xs
--   convert a product of field names to a product of monadic actions yielding 'a'
recoverFields
  ∷ ∀ (t ∷ Type) c m (d ∷ Type → Type) s xs.
    ( c, CtxRecord t m d s, Code s ~ '[xs]
    , All (FieldConstraint t m d s) xs
    , SListI xs
    , HasCallStack, Monad m)
  ⇒ Proxy c
  → Proxy (t, d s)
  → RecordCtx d s
  → ConsCtx (d s)
  → Text → Int → NP (K Text) xs → NP (m :. d) xs
recoverFields pC pTDS ctxR ctxC consName consNr fss =
  hcliftA2 (Proxy @(FieldConstraint t m d s)) recoverField fss SOP.glenses
  where
    recoverField ∷ ∀ fs. (Field t m d s fs)
                 ⇒ K Text fs
                 → SOP.GLens (→) (→) s fs
                 → (m :. d) fs
    recoverField (K fi) glens =
      readField pC (Proxy @(t, s)) (fieldCtx (Proxy @(t, s, fs, m fs)) ctxC (SOP.get glens ∷ s → fs))
      (toFieldName (Proxy @(s, d s, m s)) fi)

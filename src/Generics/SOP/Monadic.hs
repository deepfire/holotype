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
{-# OPTIONS_GHC -Wextra #-}
module Generics.SOP.Monadic
  ( Field(..), Record(..)
  , Derived
  , ConsCtx, FieldCtx
  , FieldName(..)
  , recover

  -- reexports
  , (:.), O
  )
where

import           Control.Compose
import qualified Data.List                        as L
import           Data.Maybe
import           Data.String
import           Data.Text                           (Text, pack, unpack, toLower, drop, take)
import           Data.Typeable
import           GHC.Types                           (Type)
import           Prelude                      hiding (read, take, drop, length)
import           Prelude.Unicode
import           Text.Printf

import qualified GHC.Generics                     as GHC
import           GHC.Stack
import           Generics.SOP                        (NP(..), SOP(..), I(..), K(..), Code, All, All2
                                                     ,HasDatatypeInfo(..), DatatypeInfo(..), ConstructorInfo(..), SListI
                                                     , hcliftA2, hliftA, hsequence
                                                     )
import qualified Generics.SOP                     as SOP
import qualified Generics.SOP.Lens                as SOP


-- * Somewhat generic
enumerate ∷ SListI xs ⇒ NP ConstructorInfo xs → NP (((,) Int) :. ConstructorInfo) xs
enumerate cs = SOP.hliftA2 (\c (K n)→ O (n, c)) cs (fromJust $ SOP.fromList $ L.take (SOP.lengthSList cs) [0..])


data family Derived  t   s ∷ Type
type family ConsCtx    d   ∷ Type
type family FieldCtx t   s ∷ Type

class (Monad m, d ~ Derived t) ⇒ Field t m d u s where
  fieldCtx          ∷ Proxy (t, u, s, m s)
                    → ConsCtx (d u)
                    → (u → s)
                    → FieldCtx t s
  readField         ∷ ∀ c. (HasCallStack, c)
                    ⇒ Proxy c
                    → Proxy (t, u)
                    → FieldCtx t s
                    → FieldName
                    → (m :. d) s
  default readField ∷ ( HasCallStack, c
                      , Record t m d s
                      , SOP.HasDatatypeInfo s, SOP.Generic s, GHC.Generic s
                      , Code s ~ xss, xss ~ '[xs]
                      , All2 (Field t m d s) xss
                      , FieldCtx t s ~ RecordCtx d s
                      , Applicative d
                      )
                    ⇒ Proxy c
                    → Proxy (t, u)
                    → FieldCtx t s
                    → FieldName
                    → (m :. d) s
  readField pC _pTUMS fctx _ =
    recover pC (Proxy @(t, s)) fctx

newtype FieldName = FieldName { fromFieldName ∷ Text } deriving (Eq, IsString, Ord, Show)
type ADTChoiceT        = Int

class ( Monad m, d ~ Derived t
      , SOP.Generic s, SOP.HasDatatypeInfo s)
      ⇒ Record t m d s where
  type RecordCtx   d s ∷ Type
  consCtx           ∷ Proxy (t, d s, m s)   -- ^ Given a bunch of proxies
                    → Text                  -- ^ …the constructor's name
                    → ADTChoiceT            -- ^ …its number
                    → RecordCtx  d s        -- ^ …the record recovery context
                    → ConsCtx   (d s)       -- ^ …produce the constructor recovery context.
  prefixChars       ∷ Proxy (s, d s, m s)   -- ^ Given a bunch of proxies
                    → Int                   -- ^ …tell how many prefix characters to drop from field names.
  nameMap           ∷ Proxy (s, d s, m s)   -- ^ Given the type of the record
                    → [(Text, Text)]        -- ^ …produce the partial field renaming map.
  toFieldName       ∷ Proxy (s, d s, m s)   -- ^ Given the type of the record
                    → Text                  -- ^ …the record's field name
                    → FieldName             -- ^ …produce the serialised field name.
  -- *
  nameMap           = const []
  toFieldName r x   =
    FieldName $ maybeRemap $ dropDetitle (prefixChars r) x
    where maybeRemap x = maybe x id (lookup x $ nameMap r)
          dropDetitle ∷ Int → Text → Text
          dropDetitle n (drop n → x) = toLower (take 1 x) <> drop 1 x


recover  ∷ ∀ (t ∷ Type) c m d s xss xs.
           ( c, Record t m d s
           , SOP.HasDatatypeInfo s
           , Code s ~ xss, xss ~ '[xs]
           , All2 (Field t m d s) xss
           , HasCallStack, Monad m, Applicative d)
         ⇒ Proxy c
         → Proxy (t, s)
         → RecordCtx d s
         → (m :. d) s
recover pC _pT ctxR = O $ do
    let ADT _ name cinfos = datatypeInfo (Proxy @s) ∷ DatatypeInfo (Code s)
        nc     ∷ ((,) Int :. ConstructorInfo) xs = SOP.hd $ enumerate cinfos
        ct     ∷ NP (m :. d)      xs             = recoverCtor pC (Proxy @(t, d s)) ctxR (pack name) nc
        O msop ∷ (m :. d) (SOP I (Code s))       = hsequence (SOP.SOP $ SOP.Z ct)
    (SOP.to <$>) <$> msop

-- * 1. Extract the constructor's product of field names
--   2. Feed that to the field-name→action interpreter
recoverCtor
  ∷ ∀ (t ∷ Type) c m (d ∷ Type → Type) s xs.
    ( c, Record t m d s, Code s ~ '[xs]
    , All (Field t m d s) xs
    , HasCallStack, Monad m)
  ⇒ Proxy c → Proxy (t, d s) → RecordCtx d s → Text → (((,) Int) :. ConstructorInfo) xs
  → NP (m :. d) xs
recoverCtor pC pTDS ctxR _ (O (consNr, (Record consName fis))) =
  recoverFields pC pTDS ctxR (consCtx (Proxy @(t, d s, m s)) (pack consName) consNr ctxR) $ hliftA (K ∘ pack ∘ SOP.fieldName) fis

recoverCtor _ _ _ name _ =
  error $ printf "Non-Record (plain Constructor, Infix) ADTs not supported: type %s." (unpack name)

-- * Key part:  NP (K Text) xs → NP m xs
--   convert a product of field names to a product of monadic actions yielding 'a'
recoverFields
  ∷ ∀ (t ∷ Type) c m (d ∷ Type → Type) s xs.
    ( c, Record t m d s, Code s ~ '[xs]
    , All (Field t m d s) xs
    , SListI xs
    , HasCallStack, Monad m)
  ⇒ Proxy c
  → Proxy (t, d s)
  → RecordCtx d s
  → ConsCtx (d s)
  → NP (K Text) xs → NP (m :. d) xs
recoverFields pC _pTDS _ctxR ctxC fss =
  hcliftA2 (Proxy @(Field t m d s)) recoverField fss SOP.glenses
  where
    recoverField ∷ ∀ fs. (Field t m d s fs)
                 ⇒ K Text fs
                 → SOP.GLens (→) (→) s fs
                 → (m :. d) fs
    recoverField (K fi) glens =
      readField pC (Proxy @(t, s)) (fieldCtx (Proxy @(t, s, fs, m fs)) ctxC (SOP.get glens ∷ s → fs))
      (toFieldName (Proxy @(s, d s, m s)) fi)

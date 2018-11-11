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
  , Structure, Result
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


type family Structure   s ∷ Type
data family Result    t s ∷ Type
type family ConsCtx   t s ∷ Type
type family FieldCtx  t s ∷ Type

class (Monad m) ⇒ Field t m u a where
  fieldCtx          ∷ Proxy (t, u, a, m a)
                    → ConsCtx t u
                    → (Structure u → a)
                    → FieldCtx t a
  readField         ∷ ∀ c. (HasCallStack, c)
                    ⇒ Proxy c
                    → Proxy (t, u)
                    → FieldCtx t a
                    → FieldName
                    → (m :. Result t) a
  -- default readField ∷ ( HasCallStack, c
  --                     , s ~ Structure a
  --                     , Record t m a
  --                     , SOP.HasDatatypeInfo s, SOP.Generic s, GHC.Generic s
  --                     , Code s ~ xss, xss ~ '[xs]
  --                     , All2 (Field t m s) xss
  --                     , FieldCtx t a ~ RecordCtx t a
  --                     , Applicative (Result t)
  --                     )
  --                   ⇒ Proxy c
  --                   → Proxy (t, u)
  --                   → FieldCtx t a
  --                   → FieldName
  --                   → (m :. Result t) (Structure a)
  -- readField pC _pTUMS fctx _ =
  --   recover pC (Proxy @t) fctx

newtype FieldName = FieldName { fromFieldName ∷ Text } deriving (Eq, IsString, Ord, Show)
type ADTChoiceT        = Int

class ( Monad m, SOP.Generic (Structure a), SOP.HasDatatypeInfo (Structure a)
      ) ⇒ Record t m a where
  type RecordCtx t a ∷ Type
  consCtx           ∷ Proxy (t, m a)           -- ^ Given a bunch of proxies
                    → Text                     -- ^ …the constructor's name
                    → ADTChoiceT               -- ^ …its number
                    → RecordCtx t a            -- ^ …the record recovery context
                    → ConsCtx   t a            -- ^ …produce the constructor recovery context.
  prefixChars       ∷ Proxy (a, t, m a)        -- ^ Given a bunch of proxies
                    → Int                      -- ^ …tell how many prefix characters to drop from field names.
  nameMap           ∷ Proxy (a, t, m a)        -- ^ Given the type of the record
                    → [(Text, Text)]           -- ^ …produce the partial field renaming map.
  toFieldName       ∷ Proxy (a, t, m a)        -- ^ Given the type of the record
                    → Text                     -- ^ …the record's field name
                    → FieldName                -- ^ …produce the serialised field name.
  -- *
  nameMap           = const []
  toFieldName r x   =
    FieldName $ maybeRemap $ dropDetitle (prefixChars r) x
    where maybeRemap x = maybe x id (lookup x $ nameMap r)
          dropDetitle ∷ Int → Text → Text
          dropDetitle n (drop n → x) = toLower (take 1 x) <> drop 1 x


recover  ∷ ∀ (t ∷ Type) c m a s xss xs.
           ( c, Record t m a, s ~ Structure a
           , SOP.HasDatatypeInfo s
           , Code s ~ xss, xss ~ '[xs]
           , All2 (Field t m a) xss
           , HasCallStack, Monad m, Applicative (Result t))
         ⇒ Proxy c
         → Proxy t
         → RecordCtx t a
         → (m :. Result t) s
recover pC _pT ctxR = O $ do
    let ADT _ rName cInfos                              = datatypeInfo (Proxy @s) ∷ DatatypeInfo (Code s)
        nCInfos ∷ NP ((,) Int :. ConstructorInfo) '[xs] = enumerate cInfos
        sop     ∷                   SOP (m :. Result t) (Code s)  = SOP.SOP $ SOP.Z $ recoverCtor pC (Proxy @(t, a)) ctxR (pack rName) $ SOP.hd nCInfos
        O mdsop ∷ (m :. Result t) (SOP I                (Code s)) = hsequence sop
    (SOP.to <$>) <$> mdsop

-- * 1. Extract the constructor's product of field names
--   2. Feed that to the field-name→action interpreter
recoverCtor
  ∷ ∀ (t ∷ Type) c m a s xs.
    ( c, Record t m a, s ~ Structure a
    , Code s ~ '[xs]
    , All (Field t m a) xs
    , HasCallStack, Monad m)
  ⇒ Proxy c → Proxy (t, a) → RecordCtx t a → Text → (((,) Int) :. ConstructorInfo) xs
  → NP (m :. Result t) xs
recoverCtor pC pTS ctxR _ (O (consNr, (Record consName fis))) =
  recoverFields pC pTS ctxR (consCtx (Proxy @(t, m a)) (pack consName) consNr ctxR) $ hliftA (K ∘ pack ∘ SOP.fieldName) fis

recoverCtor _ _ _ name _ =
  error $ printf "Non-Record (plain Constructor, Infix) ADTs not supported: type %s." (unpack name)

-- * Key part:  NP (K Text) xs → NP m xs
--   convert a product of field names to a product of monadic actions yielding 'a'
recoverFields
  ∷ ∀ (t ∷ Type) c m u s xs.
    ( c, Record t m u, s ~ Structure u
    , Code s ~ '[xs]
    , All (Field t m u) xs
    , SListI xs
    , HasCallStack, Monad m)
  ⇒ Proxy c
  → Proxy (t, u)
  → RecordCtx t u
  → ConsCtx t u
  → NP (K Text) xs → NP (m :. Result t) xs
recoverFields pC _pTS _ctxR ctxC fss =
  hcliftA2 (Proxy @(Field t m u)) recoverField fss SOP.glenses
  where
    recoverField ∷ ∀ a. (Field t m u a)
                 ⇒ K Text a
                 → SOP.GLens (→) (→) s a
                 → (m :. Result t) a
    recoverField (K fi) glens =
      readField pC (Proxy @(t, u))
         (fieldCtx (Proxy @(t, u, a, m a)) ctxC (SOP.get glens ∷ s → a))
      (toFieldName (Proxy @(u, t, m u)) fi)

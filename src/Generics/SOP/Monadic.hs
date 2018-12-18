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
  ( HasFieldCtx(..)
  , HasReadField(..)
  , Record(..)
  , Structure, Result
  , ConsCtx
  , FieldName(..)
  , recover

  -- reexports
  , (:.), O
  )
where

import           Control.Compose
-- import           Control.Lens
-- import           Data.Generics.Product
import qualified Data.List                        as L
import           Data.Maybe
import           Data.String
import           Data.Text                           (Text, pack, unpack, toLower, drop, take)
import           Data.Typeable
-- import           GHC.Generics
import           GHC.Types                           (Constraint, Type)
import           Prelude                      hiding (read, take, drop, length)
import           Prelude.Unicode
import           Text.Printf

import           GHC.Stack
import           Generics.SOP                        (NP(..), SOP(..), POP(..), I(..), K(..), Code, All, All2
                                                     ,HasDatatypeInfo(..), DatatypeInfo(..), ConstructorInfo(..), SListI
                                                     , hcliftA2, hliftA, hsequence
                                                     )
import           Generics.SOP.NP                     (pure_NP)
import qualified Generics.SOP                     as SOP
import qualified Generics.SOP.NP                  as SOP
-- import qualified Generics.SOP.Lens                as SOP
import qualified Generics.SOP.Prism               as SOP


-- * Somewhat generic
enumerate ∷ SListI xs ⇒ NP ConstructorInfo xs → NP (((,) Int) :. ConstructorInfo) xs
enumerate cs = SOP.hliftA2 (\c (K n)→ O (n, c)) cs (fromJust $ SOP.fromList $ L.take (SOP.lengthSList cs) [0..])


type family Structure   s ∷ Type
data family Result    t s ∷ Type
type family ConsCtx   t s ∷ Type

class (Monad m) ⇒ HasFieldCtx t m u a where
  type family FieldCtx t a ∷ Type
  fieldCtx          ∷ Proxy (t, u, a, m a)
                    → ConsCtx t u
                    → (Structure u → a)
                    → FieldCtx t a

class ( Monad m
      , HasFieldCtx t m u a
      ) ⇒
  HasReadField t m u a where
  readField         ∷ (HasCallStack)
                    ⇒ Proxy (t, u, a)
                    → FieldCtx t a
                    → FieldName
                    → (m :. Result t) a
  -- default readField ∷ ( HasCallStack
  --                     , s ~ Structure a
  --                     , Record t m a
  --                     , SOP.HasDatatypeInfo s, SOP.Generic s, GHC.Generic s
  --                     , Code s ~ xss, xss ~ '[xs]
  --                     , All2 (Field t m s) xss
  --                     , FieldCtx t a ~ RecordCtx t a
  --                     , Applicative (Result t)
  --                     )
  --                   ⇒ Proxy (t, u)
  --                   → FieldCtx t a
  --                   → FieldName
  --                   → (m :. Result t) (Structure a)
  -- readField _pTUMS fctx _ =
  --   recover (Proxy @t) fctx

newtype FieldName = FieldName { fromFieldName ∷ Text } deriving (Eq, IsString, Ord, Show)
type ADTChoiceT        = Int
type ADTChoice   m xss = m ADTChoiceT

class ( Monad m, SOP.Generic (Structure a), SOP.HasDatatypeInfo (Structure a)
      ) ⇒ Record t m a where
  type RecordCtx t a ∷ Type
  consCtx           ∷ Proxy (t, m a)           -- ^ Given a bunch of proxies
                    → Text                     -- ^ …the constructor's name
                    → ADTChoiceT               -- ^ …its number
                    → RecordCtx t a          -- ^ …the record recovery context
                    → ConsCtx   t a          -- ^ …produce the constructor recovery context.
  prefixChars       ∷ Proxy (a, t, m a)        -- ^ Given a bunch of proxies
                    → Int                      -- ^ …tell how many prefix characters to drop from field names.
  nameMap           ∷ Proxy (a, t, m a)        -- ^ Given the type of the record
                    → [(Text, Text)]           -- ^ …produce the partial field renaming map.
  toFieldName       ∷ Proxy (a, t, m a)        -- ^ Given the type of the record
                    → Text                     -- ^ …the record's field name
                    → FieldName                -- ^ …produce the serialised field name.
  restoreChoice     ∷ (HasCallStack, Monad m)
                    ⇒ Proxy (t, a)             -- ^ ..the type of the record
                    → RecordCtx t a            -- ^ Givent the record context
                    → ADTChoice m xss          -- ^ action to determine the record's constructor index.
  -- *
  nameMap           = const []
  toFieldName r x =
    FieldName $ maybeRemap $ dropDetitle (prefixChars r) x
    where maybeRemap x = maybe x id (lookup x $ nameMap r)
          dropDetitle ∷ Int → Text → Text
          dropDetitle n (drop n → x) = toLower (take 1 x) <> drop 1 x


recover
  ∷ ∀ (t ∷ Type) m a s xss xs.
    ( Record t m a, s ~ Structure a
    , SOP.HasDatatypeInfo s
    , Code s ~ xss
    , All2 (HasReadField t m a) xss
    , HasCallStack, Monad m, Applicative (Result t))
  ⇒ Proxy (t, a)
  → RecordCtx t a
  → (m :. Result t) s
recover _pTA ctxR = O $ case datatypeInfo (Proxy @s) ∷ DatatypeInfo (Code s) of
  ADT _moduleName typeName cInfos → do
    choice ← restoreChoice (Proxy @(t, a)) ctxR
    let pop    ∷ POP (m :. Result t) xss          = recover' (Proxy @(t, a)) ctxR $ (datatypeInfo (Proxy @s) ∷ DatatypeInfo (Code s))
        ct     ∷ SOP (m :. Result t) (Code s)     = (!! choice) $ SOP.apInjs_POP pop
        O msop ∷ (m :. Result t) (SOP I (Code s)) = hsequence ct
    case SOP.sList ∷ SOP.SList (Code s) of
      SOP.SCons → (SOP.to <$>) <$> msop
  -- Newtype _moduleName typeName cInfo → do
  --   let nCInfos ∷ NP ((,) Int :. ConstructorInfo) '[xs] = enumerate $ cInfo :* Nil
  --       sop     ∷                   SOP (m :. Result t) (Code s)  = SOP.SOP $ SOP.Z $ recoverCtor (Proxy @(t, a)) ctxR (pack typeName) $ SOP.hd nCInfos
  --       O mdsop ∷ (m :. Result t) (SOP I                (Code s)) = hsequence sop
  --   (SOP.to <$>) <$> mdsop

recover'
  ∷ ∀ (t ∷ Type) m a s xss xs.
    ( Record t m a, s ~ Structure a
    , Code s ~ xss
    , All2 (HasReadField t m a) xss
    , HasCallStack, Monad m)
  ⇒ Proxy (t, a)
  → RecordCtx t a
  → DatatypeInfo xss
  → POP (m :. Result t) xss
recover' pTDS ctxR (ADT _ name cs) =
  POP $ SOP.hcliftA2 (Proxy @(All (HasReadField t m a)))
        (recoverCtor (Proxy @(t, a)) ctxR (pack name))
        (enumerate cs)
        (SOP.gprisms ∷ NP (NP (SOP.GPrism (→) (→) s)) xss)
recover' _ _ _ = error "Non-ADTs not supported."
-- gprisms :: forall r w a xss. (Generic a, Code a ~ xss, Arrow r, ArrowApply w) => NP (NP (GPrism r w a)) xss

-- * 1. Extract the constructor's product of field names
--   2. Feed that to the field-name→action interpreter
recoverCtor
  ∷ ∀ (t ∷ Type) m a s xs.
    ( Record t m a, s ~ Structure a
    , All (HasReadField t m a) xs
    , HasCallStack, Monad m)
  ⇒ Proxy (t, a)
  → RecordCtx t a
  → Text
  → (((,) Int) :. ConstructorInfo) xs
  → NP (SOP.GPrism (→) (→) s) xs
  → NP (m :. Result t) xs
recoverCtor pTA ctxR _typeName (O (consNr, (Record consName fis)))  prisms =
  recoverFields pTA ctxR (consCtx (Proxy @(t, m a)) (pack consName) consNr ctxR) (hliftA (K ∘ pack ∘ SOP.fieldName) fis) prisms
recoverCtor pTA ctxR _typeName (O (consNr, (Constructor consName))) prisms =
  recoverFields pTA ctxR (consCtx (Proxy @(t, m a)) (pack consName) consNr ctxR) (pure_NP (K ""))                        prisms
recoverCtor _ _ name _ _ =
  error $ printf "Non-Record (plain Constructor, Infix) ADTs not supported: type %s." (unpack name)

-- * Key part:  NP (K Text) xs → NP m xs
--   convert a product of field names to a product of monadic actions yielding 'a'
recoverFields
  ∷ ∀ (t ∷ Type) m u s xs.
    ( Record t m u, s ~ Structure u
    , All (HasReadField t m u) xs
    , SListI xs
    , HasCallStack, Monad m)
  ⇒ Proxy (t, u)
  → RecordCtx t u
  → ConsCtx t u ----------------------------v
  → NP (K Text) xs
  → NP (SOP.GPrism (→) (→) s) xs
  → NP (m :. Result t) xs
recoverFields _pTU _ctxR cctxU fss prisms =
  hcliftA2 (Proxy @(HasReadField t m u)) recoverField fss prisms
  where
    recoverField ∷ ∀ a. (HasReadField t m u a)
                 ⇒ K Text a
                 → SOP.GPrism (→) (→) s a
                 → (m :. Result t) a
    recoverField (K fi) gprism =
  --   readField         ∷ HasCallStack
  --                     ⇒ Proxy c
  --                     → Proxy (t, u, a)
  --                     → FieldCtx t a
  --                     → FieldName
  --                     → (m :. Result t) a
      let xtract = SOP.get gprism ∷ s → a
      in readField
         (Proxy @(t, u, a))
         (fieldCtx (Proxy @(t, u, a, m a)) cctxU xtract)
      -- fieldCtx ∷ Proxy (t, u, a, m a)
      --          → ConsCtx t u
      --          → (Structure u → a)
      --          → FieldCtx t a
         (toFieldName (Proxy @(u, t, m u)) fi)

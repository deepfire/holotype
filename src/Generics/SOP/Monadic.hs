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
import           Generics.SOP                        (NP(..), SOP(..), I(..), K(..), Code, All, All2
                                                     ,HasDatatypeInfo(..), DatatypeInfo(..), ConstructorInfo(..), SListI
                                                     , hcliftA2, hliftA, hsequence
                                                     )
import           Generics.SOP.NP                     (pure_NP)
import qualified Generics.SOP                     as SOP
import qualified Generics.SOP.Lens                as SOP


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
  -- *
  nameMap           = const []
  toFieldName r x =
    FieldName $ maybeRemap $ dropDetitle (prefixChars r) x
    where maybeRemap x = maybe x id (lookup x $ nameMap r)
          dropDetitle ∷ Int → Text → Text
          dropDetitle n (drop n → x) = toLower (take 1 x) <> drop 1 x


recover  ∷ ∀ (t ∷ Type) m a s xss xs.
           ( Record t m a, s ~ Structure a
           , SOP.HasDatatypeInfo s
           , Code s ~ xss, xss ~ '[xs]
           , All2 (HasReadField t m a) xss
           , HasCallStack, Monad m, Applicative (Result t))
         ⇒ Proxy (t, a)
         → RecordCtx t a
         → (m :. Result t) s
recover _pTA ctxR = O $ do
    let ADT _ rName cInfos                              = datatypeInfo (Proxy @s) ∷ DatatypeInfo (Code s)
        nCInfos ∷ NP ((,) Int :. ConstructorInfo) '[xs] = enumerate cInfos
        sop     ∷                   SOP (m :. Result t) (Code s)  = SOP.SOP $ SOP.Z $ recoverCtor (Proxy @(t, a)) ctxR (pack rName) $ SOP.hd nCInfos
        O mdsop ∷ (m :. Result t) (SOP I                (Code s)) = hsequence sop
    (SOP.to <$>) <$> mdsop

-- * 1. Extract the constructor's product of field names
--   2. Feed that to the field-name→action interpreter
recoverCtor
  ∷ ∀ (t ∷ Type) m a s xs.
    ( Record t m a, s ~ Structure a
    , Code s ~ '[xs]
    , All (HasReadField t m a) xs
    , HasCallStack, Monad m)
  ⇒ Proxy (t, a) → RecordCtx t a → Text → (((,) Int) :. ConstructorInfo) xs
  → NP (m :. Result t) xs
recoverCtor pTS ctxR _ (O (consNr, (Record consName fis))) =
  recoverFields pTS ctxR (consCtx (Proxy @(t, m a)) (pack consName) consNr ctxR) $ hliftA (K ∘ pack ∘ SOP.fieldName) fis
recoverCtor pTS ctxR _ (O (consNr, (Constructor consName))) =
  recoverFields pTS ctxR (consCtx (Proxy @(t, m a)) (pack consName) consNr ctxR) $ (pure_NP (K ""))

recoverCtor _ _ name _ =
  error $ printf "Non-Record (plain Constructor, Infix) ADTs not supported: type %s." (unpack name)

-- * Key part:  NP (K Text) xs → NP m xs
--   convert a product of field names to a product of monadic actions yielding 'a'
recoverFields
  ∷ ∀ (t ∷ Type) m u s xs.
    ( Record t m u, s ~ Structure u
    , Code s ~ '[xs]
    , All (HasReadField t m u) xs
    , SListI xs
    , HasCallStack, Monad m)
  ⇒ Proxy (t, u)
  → RecordCtx t u
  → ConsCtx t u ----------------------------v
  → NP (K Text) xs
  → NP (m :. Result t) xs
recoverFields _pTU _ctxR cctxU fss =
    -- • Could not deduce: Code a0 ~ '[xs]
    --     arising from a use of ‘SOP.glenses’
    --   The type variable ‘a0’ is ambiguous
    -- • Couldn't match type ‘a0’ with ‘s’
    --     ‘a0’ is untouchable
    --       inside the constraints: HasReadField t m u a
    --       bound by a type expected by the context:
    --                  forall a.
    --                  HasReadField t m u a =>
    --                  K Text a -> SOP.GLens r0 w0 a0 a -> (:.) m (Result t) a
    --   ‘s’ is a rigid type variable bound by
    --     the type signature for:
    --       recoverFields :: forall t (m :: * -> *) (c :: *
    --                                                     -> Constraint) u s (xs :: [*]).
    --                        (Record t m u, s ~ Structure u, Code s ~ '[xs],
    --                         All (HasReadField t m u) xs, SListI xs, HasCallStack, Monad m) =>
    --                        Proxy c
    --                        -> Proxy (t, u)
    --                        -> RecordCtx t u
    --                        -> ConsCtx t u
    --                        -> NP (K Text) xs
    --                        -> NP (m :. Result t) xs
    --     at /run/user/1000/danteI7lu9z.hs:(172,1)-(184,25)
    --   Expected type: K Text a
    --                  -> SOP.GLens r0 w0 a0 a -> (:.) m (Result t) a
    --     Actual type: K Text a
    --                  -> SOP.GLens (->) (->) s a -> (:.) m (Result t) a
  hcliftA2 (Proxy @(HasReadField t m u)) recoverField fss SOP.glenses
  where
    recoverField ∷ ∀ a. (HasReadField t m u a)
                 ⇒ K Text a
                 → SOP.GLens (→) (→) s a
                 → (m :. Result t) a
    recoverField (K fi) glens =
  --   readField         ∷ HasCallStack
  --                     ⇒ Proxy c
  --                     → Proxy (t, u, a)
  --                     → FieldCtx t a
  --                     → FieldName
  --                     → (m :. Result t) a
      readField
         (Proxy @(t, u, a))
         (fieldCtx (Proxy @(t, u, a, m a)) cctxU (SOP.get glens ∷ s → a))
      -- fieldCtx ∷ Proxy (t, u, a, m a)
      --          → ConsCtx t u
      --          → (Structure u → a)
      --          → FieldCtx t a
         (toFieldName (Proxy @(u, t, m u)) fi)

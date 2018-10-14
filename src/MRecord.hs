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
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns #-}
module MRecord
  ( Field(..), Record(..)
  , Derived(..), Structure(..), FieldCtx(..), CtxRecord(..)
  , FieldName(..)
  , recover
  , Prod(..)
  )
where

import           Control.Exception
import           Control.Lens                        ((<&>))
import           Control.Monad                       (foldM, forM, forM_, join, liftM, when)
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

data A = A { a ∷ String, b ∷ Int } deriving (Show, GHC.Generic)
instance SOP.Generic A
instance SOP.HasDatatypeInfo A
x = mapFields @Show (\fi val→ fi<>": "<>pack (show val)) $ A "a" 1

-- mapFields ∷ ∀ cst a c xs. (SOP.Generic a, SOP.HasDatatypeInfo a, Code a ~ '[xs], All cst xs)
--           ⇒ (∀ b . cst b ⇒ b → c) → a → [c]
-- mapFields f x = case datatypeInfo (Proxy ∷ Proxy a) of
--                   info@(ADT _ _ ((Record _ _) :* Nil)) →
--                     hcollapse $ hcliftA (Proxy ∷ Proxy cst) (\(I x)→ K $ f x) (SOP.unZ ∘ SOP.unSOP $ from x)
--                   _ → error "Non-ADTs/non-Records/sums not supported."



type family ConsCtx ctx ∷ Type

class Ctx ctx where
  errCtxDesc        ∷ ctx             -- ^ Given the record context
                    → ConsCtx ctx     -- ^ ..the constructor-specific context
                    → FieldName           -- ^ ..the field name
                    → Text            -- ^ produce the error message
  dropField         ∷ Monad m         -- ^
                    ⇒ ctx             -- ^ Given the record context
                    → ConsCtx ctx     -- ^ ..the constructor-specific context
                    → FieldName           -- ^ ..the field name
                    → m ()            -- ^ remove the field.
  errCtxDesc _ _ (FieldName f) = "field '"<>f<>"'"

type family Structure d ∷ Type
data family Derived   t s ∷ Type
type family FieldCtx t (m ∷ Type → Type) s ∷ Type

class (Monad m, s ~ Structure (d s), d ~ Derived t)
  ⇒ Field t m d s where
  readField         ∷ ∀ c. (HasCallStack, c)
                    ⇒ Proxy t
                    → Proxy c
                    → Proxy m                 -- ^ Given the result type
                    → FieldCtx t m s          -- ^ ..the recovery context
                    → FieldName               -- ^ ..the field name
                    → (Prod m (Derived t)) s  -- ^ restore the point.
  -- default readField    ∷ (CtxRecord ctx a, Code a ~ xss, All2 (RestoreField ctx) xss, HasCallStack, Typeable a, Monad m)
  default readField ∷ (HasCallStack, c
                      , CtxRecord t m d s
                      , SOP.HasDatatypeInfo s, SOP.Generic s, GHC.Generic s
                      , Code s ~ xss
                      , All2 (Field t m d) xss
                      , FieldCtx t m s ~ RecordCtx d s
                      , Applicative (Derived t)
                      )
                    ⇒ Proxy t
                    → Proxy c
                    → Proxy m
                    → FieldCtx t m s
                    → FieldName
                    → (Prod m (Derived t)) s
  readField pT pC pM fctx _ = do
    -- newCtx ∷ FieldCtx ← ctxSwitch p fctx
    recover pT pC pM (Proxy @(d s)) fctx

class ( Monad m, s ~ Structure (d s), d ~ Derived t
      , SOP.Generic s, SOP.HasDatatypeInfo s)
      ⇒ Record t m d s where
  prefixChars     ∷ Proxy (m (d s),d s,s) -- ^ Given the type of the record
                  → Int             -- ^ ..produce the number of prefix characters to ignore.
  nameMap         ∷ Proxy (m (d s),d s,s) -- ^ Given the type of the record
                  → [(Text, Text)]  -- ^ produce the partial field renaming map.
  toFieldName     ∷ Proxy (m (d s),d s,s) -- ^ Given the type of the record
                  → Text            -- ^ ..the record's field name
                  → FieldName       -- ^ produce the serialised field name.
  -- *
  nameMap         = const []
  toFieldName r x = --trace (T.unpack x <> "→" <> T.unpack (maybeRemap $ dropDetitle (prefixChars r) x)) $
    FieldName $ maybeRemap $ dropDetitle (prefixChars r) x
    where maybeRemap x = maybe x id (lookup x $ nameMap r)
          dropDetitle ∷ Int → Text → Text
          dropDetitle n (drop 2 → x) = toLower (take 1 x) <> drop 1 x



newtype FieldName = FieldName { fromFieldName ∷ Text } deriving (Eq, IsString, Ord, Show)

type ADTChoiceT        = Int
type ADTChoice   m xss = m ADTChoiceT
-- type ADTChoice   m xss = m (NS (K ()) xss)

class ( Monad m, s ~ Structure (d s), d ~ Derived t
      , SOP.HasDatatypeInfo s, SOP.Generic s, Record t m d s, Monad m) ⇒
      CtxRecord t m d s where
  type RecordCtx   d s ∷ Type
  -- consCtx           ∷ ctx             -- ^ Given the record context
  --                   → Proxy a         -- ^ ..the type of the record
  --                   → Text            -- ^ ..the constructor's name
  --                   → ADTChoiceT      -- ^ ..its number
  --                   → ConsCtx ctx     -- ^ produce the constructor-specific context.
  -- * Defaulted methods
  -- presence          ∷ Monad m
  --                   ⇒ ctx             -- ^ Given the record context
  --                   → Proxy a         -- ^ ..the type of the record
  --                   → m Bool          -- ^ action to determine the record's presence.
  --presenceByField   ∷ ctx → Proxy a → IO (Maybe FieldName) -- not clear how to implement generically -- what constructor to look at?
  restoreChoice     ∷ (HasCallStack, Monad m)
                    ⇒ RecordCtx d s     -- ^ Givent the record context
                    → Proxy (d s)     -- ^ ..the type of the record
                    → ADTChoice m xss -- ^ action to determine the record's constructor index.
  -- saveChoice        ∷ Monad m
  --                   ⇒ ctx             -- ^ Given the record context
  --                   → a               -- ^ ..record itself
  --                   → m ()            -- ^ action to save the record's constructor index.
  ctxSwitch         ∷ (HasCallStack, Monad m)
                    ⇒ RecordCtx d s      -- ^ Given the type of the record
                    → m (RecordCtx d s)  -- ^ action to update the context, before each field's presence test/recovery.
  -- * Method defaults
  -- presence      _ p = pure True
  -- restoreChoice _ _ = pure 0
  -- saveChoice    _ _ = pure ()
  -- ctxSwitch  to ctx = pure ctx



class Interpret a where
  -- XXX: sadly unused
  fromText ∷ Text → a
  toText   ∷ a → Text

  -- XXX: the below establishes recursion
  -- default readField    ∷ (CtxRecord ctx a, Code a ~ xss, All2 (RestoreField ctx) xss, HasCallStack, Typeable a, Monad m)
  --                      ⇒ ctx                -- ^ Given the record context
  --                      → ConsCtx ctx        -- ^ ..the constructor-specific context
  --                      → FieldName              -- ^ ..the field name
  --                      → m (ReadType ctx a) -- ^ restore the record's field.
  -- readField ctx _ _    = do
  --   let p = Proxy ∷ Proxy a
  --   recover ctx

class WriteField   ctx a where
  writeField           ∷ (HasCallStack, Monad m)
                       ⇒ ctx             -- ^ Given the record context
                       → ConsCtx ctx     -- ^ ..the constructor-specific context
                       → FieldName           -- ^ ..the field name
                       → a               -- ^ ..the record's field value.
                       → m ()            -- ^ store the record's field.

  -- XXX: the below establishes recursion
  -- default writeField   ∷ (CtxRecord ctx a, Code a ~ xss, All2 (StoreField ctx) xss, HasCallStack, Monad m)
  --                      ⇒ ctx             -- ^ Given the record context
  --                      → ConsCtx ctx     -- ^ ..the constructor-specific context
  --                      → FieldName           -- ^ ..the field name
  --                      → a               -- ^ ..the record's field value.
  --                      → m ()            -- ^ store the record's field.
  -- writeField ctx _ _ x = store   ctx x



fieldError ∷ HasCallStack ⇒ Ctx ctx ⇒ ctx → ConsCtx ctx → FieldName → Text → b
fieldError ctx cc field mesg = error $ unpack $ errCtxDesc ctx cc field <> ": " <> mesg



-- instance {-# OVERLAPPABLE #-} (Ctx ctx, WriteField ctx a) ⇒ StoreField   ctx a where
--   storeField   ctx cc fi x = writeField ctx cc fi x

-- instance {-# OVERLAPPABLE #-} (Ctx ctx,  ReadField ctx a) ⇒ RestoreField ctx a where
--   restoreField ctx cc fi   = trace ("restoreFi→readFi "<>unpack (fromFieldName fi)) $ readField  ctx cc fi
--     <&> fromMaybe (fieldError ctx cc fi "mandatory field absent")

-- instance                      (Ctx ctx, WriteField ctx a) ⇒ StoreField   ctx (Maybe a) where
--   storeField ctx cc fi Nothing  = dropField  ctx cc fi
--   storeField ctx cc fi (Just x) = writeField ctx cc fi x

-- instance                      (Ctx ctx,  ReadField ctx a) ⇒ RestoreField ctx (Maybe a) where
--   restoreField a b fi = trace ("restoreFi Maybe→readFi "<>unpack (fromFieldName fi)) $ readField a b fi



-- to        ∷ Generic a => SOP I (Code a) → a
-- SOP       ∷ NS (NP f) xss → SOP f xss
-- S         ∷ NS a xs → NS a (x : xs)
-- Z         ∷ a x → NS a (x : xs)
-- hcpure    ∷ (AllN h c xs, HPure h)
--           ⇒ proxy c → (forall a. c a ⇒ f a) → h f xs
-- hsequence ∷ (SListIN h xs, SListIN (Prod h) xs, HSequence h, Applicative f)
--           ⇒ h f xs → f (h I xs)
-- hcollapse ∷ (SListIN h xs, HCollapse h)
--           ⇒ h (K a) xs → CollapseTo h a
-- hcliftA2  ∷ (AllN (Prod h) c xs, HAp h, HAp (Prod h))
--           ⇒ proxy c → (forall a. c a ⇒ f a → f' a → f'' a)
--           → Prod h f xs → h f' xs → h f'' xs
-- hcliftA   ∷ (AllN (Prod h) c xs, HAp h)
--           ⇒ proxy c → (forall a. c a ⇒ f a → f' a) → h f xs → h f' xs
-- readField ∷ ∀ c. (HasCallStack, c)
--           ⇒ Proxy t
--           → Proxy c
--           → Proxy m             -- ^ Given the result type
--           → Proxy s             -- ^ Given the result type
--           → FieldCtx t m s      -- ^ ..the recovery context
--           → FieldName           -- ^ ..the field name
--           → (Prod m (Derived t)) s  -- ^ restore the point.
-- class ( Monad m, s ~ Structure d, d ~ Derived t s
--       , SOP.HasDatatypeInfo s, SOP.Generic s, Record t m d s, Monad m) ⇒
--       CtxRecord t m d s where
--   type RecordCtx       d ∷ Type
    -- recover  ∷ ∀ a ctx xss.
    --            (CtxRecord ctx a
    --            , HasDatatypeInfo a
    --            , Code a ~ xss
    --            , All2 (RestoreField ctx) xss
    --            , HasCallStack)
    --          ⇒ ctx → IO a
recover  ∷ ∀ (t ∷ Type) c m d s xss.
           ( c, CtxRecord t m d s
           , SOP.HasDatatypeInfo s
           , Code s ~ xss
           , All2 (Field t m (Derived t)) xss
           , HasCallStack, Monad m, Applicative (Derived t))
-- recover  ∷ ∀ (t ∷ Type) c m d s xss.
--            ( c, Record t m d s, CtxRecord t m d s
--            , SOP.Generic s, SOP.HasDatatypeInfo s, SOP.GTo s, Generic s, Code s ~ xss
--            , All2 (Field t m (Derived t)) xss
--            , HasCallStack, Monad m, Applicative (Derived t))
         ⇒ Proxy t
         → Proxy c
         → Proxy m
         → Proxy (d s)
         → RecordCtx d s
         → (Prod m (Derived t)) s
recover pT pC pM pDS ctx = Prod $ do
    choice ← restoreChoice ctx pDS
    let pop       ∷ POP (Prod m (Derived t)) xss          = recover' pT pC pDS ctx $ (datatypeInfo (Proxy @s) ∷ DatatypeInfo (Code s))
        ct        ∷ SOP (Prod m (Derived t)) (Code s)     = (!! choice) $ SOP.apInjs_POP pop
        Prod msop ∷ (Prod m (Derived t)) (SOP I (Code s)) = hsequence ct
    (SOP.to <$>) <$> msop

-- * 1. Construct a POP, mapping the (NConstructorInfo → NP m) interpreter over its rows
--   3. Return the resultant POP of actions
recover'
  ∷ ∀ (t ∷ Type) c m (d ∷ Type → Type) s xss.
    ( c, CtxRecord t m d s
    , Code s ~ xss, All2 (Field t m d) xss, All SListI xss
    , HasCallStack, Monad m)
  ⇒ Proxy t → Proxy c → Proxy (d s) → RecordCtx d s → DatatypeInfo xss → POP (Prod m (Derived t)) xss

recover' pT pC pD ctx (ADT _ name cs) =
  POP $ hcliftA (Proxy @(All (Field t m d)))
  (recoverCtor pT pC pD ctx (pack name)) (enumerate cs)

recover' _ _ _ _ _ = error "Non-ADTs not supported."

-- * 1. Extract the constructor's product of field names
--   2. Feed that to the field-name→action interpreter
recoverCtor
  ∷ ∀ (t ∷ Type) c m (d ∷ Type → Type) s xs.
    ( c, CtxRecord t m d s
    , All (Field t m d) xs
    , HasCallStack, Monad m)
  ⇒ Proxy t → Proxy c → Proxy (d s) → RecordCtx d s → Text → NConstructorInfo xs → NP (Prod m d) xs

recoverCtor pT pC pD ctx _ (NC (Record consName fis) consNr) =
  recoverFields pT pC pD ctx (pack consName) consNr $ hliftA (K ∘ pack ∘ SOP.fieldName) fis

recoverCtor _ _ _ _ name _ =
  error $ printf "Non-Record (plain Constructor, Infix) ADTs not supported: type %s." (unpack name)

newtype Prod (a ∷ Type → Type) (b ∷ Type → Type) (c ∷ Type) =
  Prod (a (b c))

instance (Functor a, Functor b) ⇒ Functor (Prod a b) where
  fmap fs (Prod xs) = Prod $ fmap (fs <$>) xs

instance (Applicative a, Applicative b) ⇒ Applicative (Prod a b) where
  pure x = Prod (pure (pure x))
  Prod fs <*> Prod xs = Prod $ (<*>) <$> fs <*> xs

-- * Key part:  NP (K Text) xs → NP m xs
--   convert a product of field names to a product of monadic actions yielding 'a'
recoverFields
  ∷ ∀ (t ∷ Type) c m (d ∷ Type → Type) s xs.
    ( c, CtxRecord t m d s
    , All (Field t m d) xs, SListI xs
    , HasCallStack, Monad m)
  ⇒ Proxy t → Proxy c → Proxy (d s) → RecordCtx d s → Text → Int → NP (K Text) xs → NP (Prod m d) xs

recoverFields pT pC pD ctx consName consNr fs =
  hcliftA (Proxy @(Field t m d)) recoverField (fs ∷ NP (K Text) xs)
  where
    recoverField ∷ ∀ fs. Field t m d fs ⇒ K Text fs → (Prod m (Derived t)) fs
    recoverField (K fi) = -- trace ("withNames/aux "<>unpack fi<>"/"<>unpack consName) $
      readField pT pC (Proxy @m) (undefined ∷ FieldCtx t m fs) (toFieldName (Proxy @(m (d s),d s,s)) fi)
      

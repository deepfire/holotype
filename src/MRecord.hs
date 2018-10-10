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
                                                     , AllN, Prod, HAp, hap
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

type family Structure a ∷ Type

class Monad m ⇒ Field m a where
  type FieldCtx   a ∷ Type                   -- ^ Field recovery context
  readField         ∷ (HasCallStack)
                    ⇒ -- Proxy (Structure a)    -- ^ Given the result type
                    -- → 
                      FieldCtx a -- ^ ..the recovery context
                    → FieldName              -- ^ ..the field name
                    → m a                    -- ^ restore the point.
  -- default readField    ∷ (CtxRecord a, Code a ~ xss, All2 (Field m) xss, HasCallStack, Monad m)
  --                      ⇒ RecordCtx  a   -- ^ Given the record context
  --                      -- → ConsCtx ctx    -- ^ ..the constructor-specific context
  --                      → FieldCtx a        -- ^ ..the recovery context
  --                      → FieldName      -- ^ ..the field name
  --                      → m a            -- ^ restore the record's field.
  -- readField ctx _ _    = do
  --   let p = Proxy ∷ Proxy a
  --   recover ctx

class (SOP.Generic (Structure a), SOP.HasDatatypeInfo (Structure a))
      ⇒ Record a where
  -- liftF             ∷ (a → b)
  --                   → a
  prefixChars       ∷ Proxy a         -- ^ Given the type of the record
                    → Int             -- ^ produce the number of prefix characters to ignore.
  nameMap           ∷ Proxy a         -- ^ Given the type of the record
                    → [(Text, Text)]  -- ^ produce the partial field renaming map.
  toFieldName       ∷ Proxy a         -- ^ Given the type of the record
                    → Text            -- ^ ..the record's field name
                    → FieldName           -- ^ produce the serialised field name.
  -- *
  nameMap           = const []
  toFieldName r x = --trace (T.unpack x <> "→" <> T.unpack (maybeRemap $ dropDetitle (prefixChars r) x)) $
    FieldName $ maybeRemap $ dropDetitle (prefixChars r) x
    where maybeRemap x = maybe x id (lookup x $ nameMap r)
          dropDetitle ∷ Int → Text → Text
          dropDetitle n (drop 2 → x) = toLower (take 1 x) <> drop 1 x



newtype FieldName = FieldName { fromFieldName ∷ Text } deriving (Eq, IsString, Ord, Show)

type ADTChoiceT        = Int
type ADTChoice   m xss = m ADTChoiceT
-- type ADTChoice   m xss = m (NS (K ()) xss)

class (SOP.HasDatatypeInfo a, SOP.Generic a, Record a, Monad m) ⇒ CtxRecord m a where
  type RecordCtx       a ∷ Type
  mapStructure           ∷ (Structure b → Structure a) → b → m a
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
                    ⇒ RecordCtx a     -- ^ ..the type of the record
                    → Proxy a         -- ^ ..the type of the record
                    → ADTChoice m xss -- ^ action to determine the record's constructor index.
  -- saveChoice        ∷ Monad m
  --                   ⇒ ctx             -- ^ Given the record context
  --                   → a               -- ^ ..record itself
  --                   → m ()            -- ^ action to save the record's constructor index.
  -- ctxSwitch         ∷ (HasCallStack, Monad m)
  --                   ⇒ Proxy a         -- ^ Given the type of the record
  --                   → ctx             -- ^ ..the record context
  --                   → m ctx           -- ^ action to update the context, before each field's presence test/recovery.
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

-- * 1. Monadically determine the constructor #
--   2. Get the POP of actions for the record's structure
--   3. Select the slice of actions for the #th constructor
--   4. Exchange the order of monad vs. the typed list
--   5. The monadic action is then fmapped over, yielding the recovered record.

-- Q:
--   - how do we go from a list of monadic actions for a derived type
-- type GCode (a :: Type) = ToSumCode (GHC.Rep a) '[]
-- gSumTo :: SOP I (ToSumCode a xss) -> (a x -> r) -> (SOP I xss -> r) -> r
-- type family ToSumCode (a :: Type -> Type) (xs :: [[Type]]) :: [[Type]]

gto :: forall a sa. (sa ~ a, SOP.GTo sa, GHC.Generic sa)
    => SOP I (SOP.ToSumCode (GHC.Rep sa) '[]) -> sa
-- gto x = GHC.to (gSumTo x id ((\y -> case y of {}) :: SOP I '[] -> (GHC.Rep a) x))
gto x = (GHC.to
         ∷ GHC.Rep a x → a)
        ((SOP.gSumTo x id ((\y → case y of {})
                           ∷ SOP I '[]
                           → (GHC.Rep sa) x))
        -- ∷ SOP I (SOP.ToSumCode (GHC.Rep sa) '[]) -> ((GHC.Rep sa) x -> r) -> (SOP I '[] -> r) -> r
        )
     -- m (f0:f1:f2:[])
recover  ∷ ∀ a sa xss m.
           ( Record a, CtxRecord m a
           , sa ~ Structure a
           , SOP.Generic sa, SOP.HasDatatypeInfo sa , SOP.GTo sa , Generic sa , Rep sa ~ SOP I (SOP.GCode sa) , Code sa ~ xss, All2 (Field m) xss
           , HasCallStack, Monad m)
         ⇒ RecordCtx a → m a
recover ctx = do
    choice ← restoreChoice ctx (Proxy @a)
    let pop ∷ POP m xss       = recover' (Proxy @a) ctx $ (datatypeInfo (Proxy @sa) ∷ DatatypeInfo (Code sa))
        ct  ∷ SOP m (Code sa) = (!! choice) $ SOP.apInjs_POP pop
    msop ∷ SOP I (Code sa) ← (hsequence ct ∷ m (SOP I (Code sa)))
    -- (SOP I (SOP.ToSumCode (GHC.Rep a0) '[]) -> a0) -> SOP I xss -> m a
    -- _ $ SOP.gto msop
    (undefined)
   --    ((undefined ∷ (SOP I (Code sa) -> sa)
   --            →  SOP I (Code sa) ->  a)
   -- SOP.gto) <$>
    -- We'll get a monadic lifted product of (Widget t fieldtype)
    -- how do we turn:
    -- 1. lifted product of (Dynamic t Subscription, Dynamic t (x, HoloBlank))
    --  into:
    -- 2. unlifted (Dynamic t Subscription, Dynamic t (Record, HoloBlank))
    --  where:
    --    Structure (Dynamic t Subscription, Dynamic t (x, HoloBlank)) = x
    -- ..We need a functor:
    -- (Structure a → Structure b) → a → b
    --           a = (Dynamic t Subscription, Dynamic t (Record, HoloBlank))
    -- Structure a = Record

-- * 1. Construct a POP, mapping the (NConstructorInfo → NP m) interpreter over its rows
--   3. Return the resultant POP of actions
recover' ∷ ∀ a ctx xss m. (CtxRecord m a, All2 (Field m) xss, All SListI xss, HasCallStack, Monad m)
         ⇒ Proxy a → RecordCtx a → DatatypeInfo xss → POP m xss
recover' pxA ctx (ADT _ name cs) =
  POP $ hcliftA (pAllRFields (Proxy @m) (Proxy @ctx))
  (recoverCtor pxA ctx (pack name)) (enumerate cs)
recover' _ _ _ = error "Non-ADTs not supported."

-- * 1. Extract the constructor's product of field names
--   2. Feed that to the field-name→action interpreter
recoverCtor ∷ ∀ a ctx xs m. (CtxRecord m a, All (Field m) xs, HasCallStack, Monad m)
           ⇒ Proxy a → RecordCtx a → Text → NConstructorInfo xs → NP m xs
recoverCtor proxy ctx _ (NC (Record consName fis) consNr) =
  recoverFields proxy ctx (pack consName) consNr $ hliftA (K ∘ pack ∘ SOP.fieldName) fis
recoverCtor _ _ name _ =
  error $ printf "Non-Record (plain Constructor, Infix) ADTs not supported: type %s." (unpack name)

newtype Prod' a b c = Prod' (a (b c))

-- * Key part:  NP (K Text) xs → NP m xs
--   convert a product of field names to a product of monadic actions yielding 'a'
recoverFields ∷ ∀ ctx xs m a. (CtxRecord m a, All (Field m) xs, SListI xs, HasCallStack, Monad m)
          ⇒ Proxy a → RecordCtx a → Text → Int → NP (K Text) xs → NP (Prod' m Structure) xs
recoverFields proxy ctx consName consNr fs =
  hcliftA (Proxy @(Field m)) recoverField (fs ∷ NP (K Text) xs)
  where
    recoverField ∷ ∀ fv. Field m fv ⇒ K Text (Structure fv) → m fv
    recoverField (K fi) = -- trace ("withNames/aux "<>unpack fi<>"/"<>unpack consName) $
      readField (undefined) (toFieldName proxy fi)

pRecord       ∷ Proxy ctx → Proxy (CtxRecord ctx)
pRecord     _ = Proxy
pRField       ∷ Proxy ctx → Proxy (Field ctx)
pRField     _ = Proxy
pSField       ∷ Proxy ctx → Proxy (WriteField ctx)
pSField     _ = Proxy
pAllRFields   ∷ Proxy m → Proxy ctx → Proxy (All (Field m))
pAllRFields _ _ = Proxy
pAllSFields   ∷ Proxy ctx → Proxy (All (WriteField ctx))
pAllSFields _ = Proxy

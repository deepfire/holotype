{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors -Wno-orphans -Wno-type-defaults #-}

module Holo.Record
  ( TypeAs(..)
  , liftRecord
  , HoloName(..)
  )
where

import           Control.Arrow
import           Control.Compose
import           Data.Text                                (Text)
import           Data.Typeable
import           Generics.SOP.Monadic
import           Reflex.GLFW                              (RGLFW)
import qualified Data.TypeMap.Dynamic              as TM
import qualified Generics.SOP                      as SOP
import           GHC.Types
import           Reflex                            hiding (Query, Query(..))

import           HoloPrelude
import           Holo
import qualified HoloPort                          as Port
import           Holo.Instances


-- * Lifted records (depends on Holo Text instance)
--
instance SOP.Generic         Port.Settings
instance SOP.HasDatatypeInfo Port.Settings

liftRecord ∷ ∀ a t m s xs.
  ( RGLFW t m, Record t m a, s ~ Structure a
  , SOP.Code s ~ '[xs]
  , SOP.All (HasReadField t m a) xs
  ) ⇒ RecordCtx t a → m (Widget t s)
liftRecord ctxR = unO $ recover (Proxy @(t, a)) ctxR
-- recover  ∷ ∀ (t ∷ Type) c m a s xss xs.
--            ( Record t m a, s ~ Structure a
--            , SOP.HasDatatypeInfo s
--            , Code s ~ xss, xss ~ '[xs]
--            , All2 (HasReadField t m c a) xss
--            , HasCallStack, Monad m, Applicative (Result t))
--          ⇒ Proxy c
--          → Proxy (t, a)
--          → RecordCtx t a
--          → (m :. Result t) s

-- Design derivation for Holo lifts:
-- 1. P: low-friction definition for structure types
-- 2. P: low-friction definition for structure field types
-- 3. 1+2 ?→ Field definition not proportional to structure and field types
-- 4. …

-- A name and a piece of evidence of its relevance to 'b'.
data HoloName b where
  HoloName ∷ (As n, Denoted n ~ a, Mutable a, Interp a b, Named a b, Holo b) ⇒ n → HoloName b

data HoloTag
type instance TM.Item HoloTag b = HoloName b
newtype TypeAs c = TypeAs (TM.TypeMap HoloTag)

instance ( RGLFW t m
         , d ~ Result t
         , ConsCtx t u ~ (InputEventMux t, TypeAs Holo, Structure u)
         ) ⇒
         HasFieldCtx t m u a where
  type instance FieldCtx t a  = (InputEventMux t, TypeAs Holo, a)
  fieldCtx _ (mux, tas, x) proj = (mux, tas, proj x)

instance ( RGLFW t m
         , HasFieldCtx t m  u a
         , d ~ Result t
         , ConsCtx t u ~ (InputEventMux t, TypeAs Holo, Structure u)
         , As TextLine, Holo Text
         , Typeable b
         ) ⇒
         HasReadField t m u b where
  readField _ (mux, TypeAs tam, initV ∷ b) (FieldName fname) = O $ do
    tok ← liftIO $ Port.newId $ "record label '" <> fname <> "'"
    let addLabel x = hbox [ (defLeaf ∷ (x ~ TextLine, As x, Unconstr (Denoted x))
                              ⇒ Port.IdToken → x → Denoted x → Blank)
                            tok TextLine (fname <> ": ")
                          , x
                          ]
        fP = Proxy @b
    case TM.lookup fP tam of
      Nothing      → error $ printf "Record recovery has no As element for field of type %s." (show $ typeRep fP)
      Just (HoloName x) →
        W ∘ (id *** (<&> (id *** addLabel))) ∘ fromW <$> liftW mux (defAs $ proxy x) initV
        -- liftW ∷ (As n, Denoted n ~ a, Mutable a, Interp a b, Named a b, Holo b, RGLFW t m)
        -- ⇒ InputEventMux t → n → b → m (Widget t b)

-- record lifting for Dynamic initials
type instance ConsCtx  t (Dynamic t a) = Dynamic t a
type instance Structure    (Dynamic _ a) = a


type instance ConsCtx  t (Static t a)  = (InputEventMux t, TypeAs Holo, a)
type instance Structure    (Static _ a)  = a

instance ( Monad m, SOP.Generic a, SOP.HasDatatypeInfo a
         , RGLFW t m
         ) ⇒ Record t m (Static t a) where
  type RecordCtx t (Static t a) = (InputEventMux t, TypeAs Holo, a)
  prefixChars _ = 3
  consCtx _ _ _ (mux, ta, a) = (mux, ta, a)

instance ( Monad m, SOP.Generic a, SOP.HasDatatypeInfo a
         , RGLFW t m
         ) ⇒ Record t m (Dynamic t a) where
  type RecordCtx t (Dynamic t a) = Dynamic t a
  prefixChars _ = 3
  consCtx _ _ _ x = x
  -- toFieldName _ = (⊥)
  -- nameMap       = (⊥)

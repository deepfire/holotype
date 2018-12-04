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

liftRecord ∷ ∀ t m a s xs.
  ( RGLFW t m, Record t m Holo a, s ~ Structure a
  , SOP.Code s ~ '[xs]
  , SOP.All (HasReadField t m Holo a) xs
  ) ⇒ RecordCtx t Holo a → m (Widget t s)
liftRecord ctxR = unO $ recover (Proxy @Holo) (Proxy @(t, a)) ctxR
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

-- * A style map of types to their As types.
data WXs c b where
  WXs ∷ (As n, Denoted n ~ a, Mutable a, Interp a b, Named a b, c b) ⇒ n → WXs c b

data TypeAsTag (с ∷ Type → Constraint)
type instance TM.Item (TypeAsTag с) b = WXs с b
newtype TypeAs c = TypeAs (TM.TypeMap (TypeAsTag c))

instance ( RGLFW t m
         , Holo a
         , d ~ Result t
         , ConsCtx t Holo u ~ (InputEventMux t, TypeAs Holo, Structure u)) ⇒
         HasFieldCtx t m Holo u a where
  type instance FieldCtx t Holo a  = (InputEventMux t, TypeAs Holo, a)
  fieldCtx _ _ (mux, tas, x) proj = (mux, tas, proj x)

instance ( RGLFW t m
         , As n, Denoted n ~ a, Mutable a, Interp a b, Named a b, RGLFW t m
         , Holo ~ Holo, Holo b, Typeable b
         , d ~ Result t
         , ConsCtx t Holo u ~ (InputEventMux t, TypeAs c, Structure u)
         , As TextLine, Holo Text
         , c ~ Holo
         ) ⇒
         HasReadField t m (c ∷ Type → Constraint) u b where
  readField _ _ (mux, TypeAs tam, initV ∷ b) (FieldName fname) = O $ do
    tok ← liftIO $ Port.newId $ "record label '" <> fname <> "'"
    let addLabel x = hbox [ (defLeaf ∷ (x ~ TextLine, As x, Unconstr (Denoted x))
                              ⇒ Port.IdToken → x → Denoted x → Blank)
                            tok TextLine (fname <> ": ")
                          , x
                          ]
        fP = Proxy @b
    case TM.lookup fP tam of
      Nothing      → error $ printf "Record recovery has no As element for field of type %s." (show $ typeRep fP)
      Just (WXs x) →
        W ∘ (id *** (<&> (id *** addLabel))) ∘ fromW <$> liftW mux (defAs $ proxy x) initV
        -- liftW ∷ (As n, Denoted n ~ a, Mutable a, Interp a b, Named a b, Holo b, RGLFW t m)
        -- ⇒ InputEventMux t → n → b → m (Widget t b)

-- record lifting for Dynamic initials
type instance ConsCtx  t c (Dynamic t a) = Dynamic t a
type instance Structure    (Dynamic _ a) = a


type instance ConsCtx  t c (Static t a)  = (InputEventMux t, TypeAs c, a)
type instance Structure    (Static _ a)  = a

instance ( Monad m, SOP.Generic a, SOP.HasDatatypeInfo a
         , RGLFW t m
         ) ⇒ Record t m c (Static t a) where
  type RecordCtx t c (Static t a) = (InputEventMux t, TypeAs c, a)
  prefixChars _ _ = 3
  consCtx _ _ _ _ (mux, ta, a) = (mux, ta, a)

instance ( Monad m, SOP.Generic a, SOP.HasDatatypeInfo a
         , RGLFW t m
         ) ⇒ Record t m c (Dynamic t a) where
  type RecordCtx t c (Dynamic t a) = Dynamic t a
  prefixChars _ _ = 3
  consCtx _ _ _ _ x = x
  -- toFieldName _ = (⊥)
  -- nameMap       = (⊥)

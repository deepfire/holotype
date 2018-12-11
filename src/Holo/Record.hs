{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
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
  , liftWRecord
  , HoloName(..)
  )
where

import           Control.Arrow
import           Control.Compose
import           Control.Newtype.Generics
import           Data.Text                                (Text)
import           Data.Typeable
import           Generics.SOP.Monadic
import           Reflex.GLFW                              (RGLFW)
import qualified Data.TypeMap.Dynamic              as TM
import qualified Generics.SOP                      as SOP
import           Reflex                            hiding (Query, Query(..))

import           HoloPrelude
import           Holo
import qualified HoloPort                          as Port
import           Holo.Instances


-- * Lifted records (depends on Holo Text instance)
--
instance SOP.Generic         Port.Settings
instance SOP.HasDatatypeInfo Port.Settings

liftWRecord ∷ ∀ a i t m s xs.
  ( HGLFW i t m, Record i m a, s ~ Structure a
  , SOP.Code s ~ '[xs]
  , SOP.All (HasReadField i m a) xs
  ) ⇒ RecordCtx i a → m (Widget i s)
liftWRecord ctxR = unO $ recover (Proxy @(i, a)) ctxR
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
data HoloName i b where
  HoloName ∷ (As n, Denoted n ~ a, Mutable a, Interp a b, Named a b, Holo i b) ⇒ n → HoloName i b

data (HoloTag i)
type instance TM.Item (HoloTag i) b = HoloName i b
newtype TypeAs i c = TypeAs (TM.TypeMap (HoloTag i))

instance ( HGLFW i t m
         , d ~ Result i
         , ConsCtx i u ~ (InputEventMux t, TypeAs i (Holo i), Structure u)
         ) ⇒
         HasFieldCtx i m u a where
  type instance FieldCtx i a  = (InputEventMux (HoloAPIt i), TypeAs i (Holo i), a)
  fieldCtx _ (mux, tas, x) proj = (mux, tas, proj x)

instance ( HGLFW i t m
         , HasFieldCtx i m u a
         , d ~ Result i
         , ConsCtx i u ~ (InputEventMux t, TypeAs i (Holo i), Structure u)
         , As TextLine, Holo i Text
         , Typeable b
         ) ⇒
         HasReadField i m u b where
  readField _ (mux, TypeAs tam, initV ∷ b) (FieldName fname) = O $ do
    tok ← liftIO $ Port.newId $ "record label '" <> fname <> "'"
    let addLabel x = hbox [ (defLeaf ∷ (x ~ TextLine, As x, Unconstr (Denoted x))
                              ⇒ Port.IdToken → x → Denoted x → Blank i)
                            tok TextLine (fname <> ": ")
                          , x
                          ]
        fP = Proxy @b
    case TM.lookup fP tam of
      Nothing      → error $ printf "Record recovery has no As element for field '%s' of type %s." fname (show $ typeRep fP)
      Just (HoloName x ∷ HoloName i b) →
        W ∘ (id *** (<&> (id *** addLabel))) ∘ fromW <$> liftW @i mux (defAs $ proxy x) initV
        -- liftW ∷ (As n, Denoted n ~ a, Mutable a, Interp a b, Named a b, Holo b, RGLFW t m)
        -- ⇒ InputEventMux t → n → b → m (Widget t b)

type instance ConsCtx  i a = (InputEventMux (HoloAPIt i), TypeAs i (Holo i), a)
type instance Structure  a = a

instance ( Monad m, SOP.Generic a, SOP.HasDatatypeInfo a
         , HGLFW i t m
         ) ⇒ Record i m a where
  type RecordCtx i a = (InputEventMux (HoloAPIt i), TypeAs i (Holo i), a)
  prefixChars _ = 3
  consCtx _ _ _ (mux, ta, a) = (mux, ta, a)

-- * !!!!!!! This is now almost possible (modulo the instance overlap) !!!!!!!!
--
-- instance {-# OVERLAPPABLE #-} (Typeable b, SOP.Generic b, SOP.HasDatatypeInfo b
--          , SOP.Code b ~ '[xs]
--          , SOP.All (HasReadField i m b) xs
--          , Structure b ~ b
--          , HGLFW i t m
--          ) ⇒ Holo i b where
--   liftW mux name init =
--     liftWRecord (mux, (⊥), init)

    -- @b @t @m @b
    -- ∷ m (Widget t b)
--
-- liftWRecord ∷ ∀ a t m s xs.
--   ( RGLFW t m, Record t m a, s ~ Structure a
--   , SOP.Code s ~ '[xs]
--   , SOP.All (HasReadField t m a) xs
--   ) ⇒ RecordCtx t a → m (Widget t s)
-- liftWSeed ∷ ∀ m t n a b.
--   (As n, Denoted n ~ a, Mutable a, Interp a b, Named a b, Holo b, RGLFW t m)
--   ⇒ InputEventMux t → n → b → m (Widget t b)
-- liftWSeed imux n initial = do
--   tok ← iNewToken $ Proxy @b
--   mut ← mutate (forget initial) $ select imux $ Const2 tok
--   liftDynW tok n mut


-- * The below constitutes an attempt to allow Holo lifting of dynamic-supplied records.
--
-- type instance ConsCtx  t (Dynamic t a) = Dynamic t a
-- type instance Structure    (Dynamic _ a) = a

-- newtype Static t a = Static a -- XXX: once we're successful with the lift, let's drop the 't'
--   deriving newtype (Newtype)

-- type instance ConsCtx  t (Static t a)  = (InputEventMux t, TypeAs Holo, a)
-- type instance Structure    (Static _ a)  = a

-- instance ( Monad m, SOP.Generic a, SOP.HasDatatypeInfo a
--          , RGLFW t m
--          ) ⇒ Record t m (Static t a) where
--   type RecordCtx t (Static t a) = (InputEventMux t, TypeAs Holo, a)
--   prefixChars _ = 3
--   consCtx _ _ _ (mux, ta, a) = (mux, ta, a)

-- instance ( Monad m, SOP.Generic a, SOP.HasDatatypeInfo a
--          , RGLFW t m
--          ) ⇒ Record t m (Dynamic t a) where
--   type RecordCtx t (Dynamic t a) = Dynamic t a
--   prefixChars _ = 3
--   consCtx _ _ _ x = x
  -- toFieldName _ = (⊥)
  -- nameMap       = (⊥)

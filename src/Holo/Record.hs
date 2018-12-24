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
  ( Vocab(..)
  , Definition(..)
  )
where

import           Control.Compose
import           Data.Text                                (Text)
import           Data.Typeable
import           Generics.SOP.Monadic
import qualified Generics.SOP                      as SOP
import           Reflex                            hiding (Query, Query(..))

import           HoloPrelude
import           Holo
import qualified HoloPort                          as Port
import qualified HoloCairo                         as Cr
import           Holo.Instances


-- * Lifted records (depends on Widgety Text instance)
--
instance SOP.Generic         Port.Settings
instance SOP.HasDatatypeInfo Port.Settings
instance SOP.Generic         Cr.FontSpec
instance SOP.HasDatatypeInfo Cr.FontSpec
instance SOP.Generic         Cr.FontSizeRequest
instance SOP.HasDatatypeInfo Cr.FontSizeRequest

liftWProduct ∷ ∀ a i t m xss.
  ( HGLFW i t m, Record i m a
  , SOP.Code a ~ xss
  , SOP.All2 (HasReadField i m a) xss
  , HasCallStack
  ) ⇒ RecordCtx i a → m (Widget i a)
liftWProduct ctxR = unO $ recover (Proxy @(i, a)) ctxR

instance ( HGLFW i t m
         , d ~ Result i
         , ConsCtx i u ~ (InputEventMux t, Vocab i (Present i), u)
         ) ⇒
         HasFieldCtx i m u a where
  type instance FieldCtx i a  = (InputEventMux (APIt i), Vocab i (Present i), a)
  fieldCtx _ (mux, tas, x) proj = (mux, tas, proj x)

instance ( HGLFW i t m
         , d ~ Result i
         , ConsCtx i u ~ (InputEventMux t, Vocab i (Present i), u)
         , As TextLine, Present i Text
         , Typeable a
         , Present i a
         ) ⇒
         HasReadField i m u a where
  readField _ (mux, voc, initV ∷ a) (FieldName fname) = O $ do
    tok ← liftIO $ Port.newId $ "record label '" <> fname <> "'"
    let addLabel ""  x = x
        addLabel lab x = hbox [ (defLeaf ∷ (x ~ TextLine, As x, Unconstr (Denoted x))
                                  ⇒ Port.IdToken → x → Denoted x → Blank i)
                                tok TextLine (lab <> ": ")
                              , x
                              ]
    mapWItem @i (addLabel fname) <$> present @i mux voc initV

type instance ConsCtx  i a = (InputEventMux (APIt i), Vocab i (Present i), a)

instance ( Monad m, SOP.Generic a, SOP.HasDatatypeInfo a
         , HGLFW i t m
         ) ⇒ Record i m a where
  type RecordCtx i a = (InputEventMux (APIt i), Vocab i (Present i), a)
  restoreChoice _p _rctx = pure 0
  prefixChars _ = 3
  consCtx _ _ _ (mux, ta, a) = (mux, ta, a)

instance {-# OVERLAPPABLE #-} (Typeable a, SOP.Generic a, SOP.HasDatatypeInfo a
         , SOP.Code a ~ xss
         , SOP.All2 (HasReadField i m a) xss
         , HGLFW i t m
         ) ⇒ Present i a where
  present mux voc initial =
    liftWProduct (mux, voc, initial)


-- * The below constitutes an attempt to allow Widgety lifting of dynamic-supplied records.
--
-- type instance ConsCtx  t (Dynamic t a) = Dynamic t a
-- type instance Structure    (Dynamic _ a) = a

-- newtype Static t a = Static a -- XXX: once we're successful with the lift, let's drop the 't'
--   deriving newtype (Newtype)

-- type instance ConsCtx  t (Static t a)  = (InputEventMux t, Vocab Holo, a)
-- type instance Structure    (Static _ a)  = a

-- instance ( Monad m, SOP.Generic a, SOP.HasDatatypeInfo a
--          , RGLFW t m
--          ) ⇒ Record t m (Static t a) where
--   type RecordCtx t (Static t a) = (InputEventMux t, Vocab Holo, a)
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

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

import           Data.Text                                (Text, pack)
import           Data.Typeable
import           Generics.SOP.Monadic
import qualified Generics.SOP                      as SOP

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
liftWProduct ctxR = SOP.unComp $ recover (Proxy @(i, a)) ctxR

instance ( HGLFW i t m
         , d ~ Result i
         , As TextLine, Present i Text
         , Typeable a
         , Present i a
         ) ⇒
         HasReadField i m u a where
  readField _ (mux, voc, initV ∷ u) _dtinfo _consNr _cinfo (FieldInfo fname) proj = Comp $ do
    tok ← liftIO $ Port.newId $ "record label '" <> pack fname <> "'"
    let addLabel ""  x = x
        addLabel lab x = hbox [ (defLeaf ∷ (x ~ TextLine, As x, Unconstr (Denoted x))
                                  ⇒ Port.IdToken → x → Denoted x → Blank i)
                                tok TextLine (pack lab <> ": ")
                              , x
                              ]
    mapWItem @i (addLabel fname) <$> present @i mux voc (proj initV)

instance ( Monad m, SOP.Generic a, SOP.HasDatatypeInfo a
         , HGLFW i t m
         ) ⇒ Record i m a where
  type RecordCtx i a = (InputEventMux (APIt i), Vocab i (Present i), a)
  restoreChoice _p _rctx = pure 0

instance {-# OVERLAPPABLE #-} (Typeable a, SOP.Generic a, SOP.HasDatatypeInfo a
         , SOP.Code a ~ xss
         , SOP.All2 (HasReadField i m a) xss
         , HGLFW i t m
         ) ⇒ Present i a where
  present mux voc initial =
    liftWProduct (mux, voc, initial)

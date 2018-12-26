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
import           Reflex

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

instance {-# OVERLAPPABLE #-}
  (Typeable a, SOP.Generic a, SOP.HasDatatypeInfo a
  , SOP.Code a ~ xss
  , SOP.All2 (Present i) xss
  , HGLFW i t m
  ) ⇒ Present i a where
  present mux voc initial =
    SOP.unComp $ recover (Proxy @(Present i)) (Proxy @(i, a))
    (\_p _dti → pure 0)
    (recoverFieldPresent (mux, voc, initial))

recoverFieldPresent ∷ ∀ i t m u a xss xs.
  ( HGLFW i t m
  , SOP.HasDatatypeInfo u, SOP.Code u ~ xss
  , As TextLine, Present i Text
  , Typeable a
  , Present i a
  )
  ⇒ (InputEventMux t, Vocab i (Present i), u)
  → ReadFieldT (Present i) i m u a xss xs
recoverFieldPresent (mux, voc, initV ∷ u) _pC _ _dtinfo _consNr _cinfo (FieldInfo fname) proj = Comp $ do
    tok ← liftIO $ Port.newId $ "record label '" <> pack fname <> "'"
    let addLabel ""  x = x
        addLabel lab x = hbox [ (defLeaf ∷ (x ~ TextLine, As x, Unconstr (Denoted x))
                                  ⇒ Port.IdToken → x → Denoted x → Blank i)
                                tok TextLine (pack lab <> ": ")
                              , x
                              ]
    mapWItem @i (addLabel fname) <$> present @i mux voc (proj initV)

instance ( Typeable a, HGLFW i t m
         , SOP.Generic a, SOP.HasDatatypeInfo a, SOP.Code a ~ xss, SOP.All2 (Interact i) xss
         ) ⇒ Interact i a where
  dynWidget = dynWidgetStaticSubsRecord

recoverFieldInteractDynamic
  ∷ ∀ i t m a f xss xs.
    ( Typeable f
    , Mutable a, Named a, HGLFW i t m
    , SOP.Generic a
    , SOP.HasDatatypeInfo a
    , SOP.Code a ~ xss, SOP.All2 (Interact i) xss
    )
  ⇒ (Port.IdToken, Vocab i (Present i), Dynamic t a)
  → Proxy (Interact i)
  → Proxy (i, a, f)
  → DatatypeInfo xss
  → SumChoiceT
  → ConstructorInfo xs
  → FieldInfo f
  → (a → f)
  → (:.:) m (Result i) f
recoverFieldInteractDynamic (tok, voc, dRec) _pC _pIAF _dtinfo _consNr _cinfo _finfo proj =
  Comp $ do
    let fieldD = proj <$> dRec
    case Holo.vocInteractName (Proxy @f) voc of
      Nothing        → error $ printf "Dynamic lift has no visual name for value of type %s." (show $ typeRep (Proxy @f))
      Just (IName _) → error $ printf "Dynamic lift has no visual name for value of type %s." (show $ typeRep (Proxy @f))
      Just (WName n) → do
        dynWidget tok n fieldD
      Just (IWName n) → do
        dynWidget tok n fieldD

dynWidgetStaticSubsRecord ∷ ∀ i t m n a xss.
  (As n, Denoted n ~ a, Mutable a, Named a, HGLFW i t m
  , SOP.Generic a
  , SOP.HasDatatypeInfo a
  , SOP.Code a ~ xss, SOP.All2 (Interact i) xss
  )
  ⇒ Port.IdToken → Vocab i (Present i) → Dynamic t a → m (Widget i a)
dynWidgetStaticSubsRecord tok voc da =
    SOP.unComp $ recover (Proxy @(Interact i)) (Proxy @(i, a))
      (\_px _dti→ pure 0)
      (recoverFieldInteractDynamic (tok, voc, da))
    -- let name ∷ Name n = compName (Proxy @a) tok n
    -- pure $ W ( constDyn $ subscription tok (Proxy @a)
    --          , leaf name <$> da
    --          , da)

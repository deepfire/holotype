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

import qualified Graphics.Cairo                    as Cr
import           Holo
import           Holo.Instances
import           Holo.Prelude
import qualified Holo.Port                         as Port


-- * Lifted records (depends on Widgety Text instance)
--
instance SOP.Generic         Port.Settings
instance SOP.HasDatatypeInfo Port.Settings
instance SOP.Generic         Cr.FontSpec
instance SOP.HasDatatypeInfo Cr.FontSpec
instance SOP.Generic         Cr.FontSizeRequest
instance SOP.HasDatatypeInfo Cr.FontSizeRequest

instance {-# OVERLAPPABLE #-}
  (Typeable a
  , SOP.Generic a, SOP.HasDatatypeInfo a, SOP.Code a ~ xss
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

instance {-# OVERLAPPABLE #-}
  ( Typeable a
  , SOP.Generic a, SOP.HasDatatypeInfo a, SOP.Code a ~ xss
  , SOP.All2 Mutable      xss
  , SOP.All2 (Widgety i) xss
  , HGLFW i t m
  ) ⇒ Widgety i a where
  dynWidget' = dynWidgetStaticSubsRecord

recoverFieldWidgetyDynamic
  ∷ ∀ i t m a f xss xs.
    ( HasCallStack, Typeable f
    , Named a, HGLFW i t m
    , SOP.Generic a
    , SOP.HasDatatypeInfo a
    , SOP.Code a ~ xss, SOP.All2 (Widgety i) xss
    )
  ⇒ (Port.IdToken, Vocab i (Present i), Dynamic t a)
  → ReadFieldT (Widgety i) i m a f xss xs
recoverFieldWidgetyDynamic (tok, voc, dRec) _pC _pIAF _dtinfo _consNr _cinfo _finfo proj =
  Comp $ do
    let fieldD = proj <$> dRec
        -- XXX:  face the need for dynPresent
        vocabErr (desc ∷ String) = error $ printf "Dynamic record lift has no Denot for value of type %s (%s).\n%s" (show $ typeRep (Proxy @f)) desc (ppVocab voc)
    case Holo.vocDenot (Proxy @f) voc of
      Nothing        → vocabErr "Nothing"
      Just (Desig _) → vocabErr "Just Desig"
      Just (Denot _)      → dynWidget' tok voc fieldD
      Just (DesigDenot _) → dynWidget' tok voc fieldD

dynWidgetStaticSubsRecord ∷ ∀ i t m a xss.
  ( HasCallStack, Named a, HGLFW i t m
  , SOP.Generic a
  , SOP.HasDatatypeInfo a
  , SOP.Code a ~ xss, SOP.All2 (Widgety i) xss
  )
  ⇒ Port.IdToken → Vocab i (Present i) → Dynamic t a → m (Widget i a)
dynWidgetStaticSubsRecord tok voc da =
    SOP.unComp $ recover (Proxy @(Widgety i)) (Proxy @(i, a))
      (\_px _dti→ pure 0)
      (recoverFieldWidgetyDynamic (tok, voc, da))

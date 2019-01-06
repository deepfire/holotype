{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors -Wno-orphans -Wno-type-defaults #-}
module Holo.Record
  ( Vocab(..)
  , Definition(..)
  )
where

import           Data.Text                                (Text, pack)
import           Data.Typeable
import           Generics.SOP.Monadic
import           Generics.SOP                             (Top)
import qualified Generics.SOP                      as SOP
import           Reflex

import qualified Graphics.Cairo                    as Cr
import           Holo.Instances
import           Holo.Input
import           Holo.Item
import           Holo.Prelude
import qualified Holo.Port                         as Port
import           Holo.Widget


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
  ) ⇒ Widgety i a where
  dynWidget' tok voc da =
    SOP.unComp $ recover (Proxy @(Present i)) (Proxy @(i, a))
    (\_p _dti → pure 0)
    (recoverFieldWidget (tok, voc, da))

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
  dynPresent mux voc da  =
    SOP.unComp $ recover (Proxy @(Present i)) (Proxy @(i, a))
    (\_px _dti→ pure 0)
    (recoverFieldPresentDynamic (mux, voc, da))

recoverFieldWidget ∷ ∀ i t m u f xss xs.
  ( HGLFW i t m
  , SOP.HasDatatypeInfo u, SOP.Code u ~ xss
  , As TextLine, Present i Text
  , Typeable f
  , Present i f
  )
  ⇒ (Port.IdToken, Vocab i (Present i), Dynamic t u)
  → ReadFieldT (Present i) i m u f xss xs
recoverFieldWidget (tok, voc, dRec) _pC _pIAF _dtinfo _consNr _cinfo _fname proj = Comp $
  let vocabErr (desc ∷ String) = error $ printf "Dynamic record lift/Widgety has no Desig for value of type %s (%s).\n%s" (show $ typeRep (Proxy @f)) desc (ppVocab voc)
  in
  case vocDesig (Proxy @f) voc of
    Nothing        → vocabErr "Nothing"
    Just (Denot _) → vocabErr "Just Denot"
    Just (Desig      (_ ∷ n)) → do
      W (sD,iD,vD) ← dynWidget' @i @(Denoted n) tok voc (forget ∘ proj <$> dRec)
      ivD ← interpretate @i vD
      pure $ W (sD,iD,ivD)
    Just (DesigDenot (_ ∷ n)) → do
      W (sD,iD,vD) ← dynWidget' tok voc (proj <$> dRec)
      ivD ← interpretate @i vD
      pure $ W (sD,iD,ivD)

recoverFieldPresent ∷ ∀ i t m u a xss xs.
  ( HGLFW i t m
  , SOP.HasDatatypeInfo u, SOP.Code u ~ xss
  , As TextLine, Present i Text
  , Typeable a
  , Present i a
  )
  ⇒ (InputEventMux t, Vocab i (Present i), u)
  → ReadFieldT (Present i) i m u a xss xs
recoverFieldPresent (mux, voc, initV ∷ u) _pC _pIAF _dtinfo _consNr _cinfo (FieldInfo fname) proj = Comp $ do
  tok ← liftIO $ Port.newId $ "record label '" <> pack fname <> "'"
  let addLabel ""  x = x
      addLabel lab x = hbox [ (defLeaf ∷ (x ~ TextLine, As x, Top (Denoted x))
                                ⇒ Port.IdToken → x → Denoted x → Blank i)
                              tok TextLine (pack lab <> ": ")
                            , x
                            ]
  mapWItem @i (addLabel fname) <$> present @i mux voc (proj initV)

recoverFieldPresentDynamic
  ∷ ∀ i t m a f xss xs.
    ( HasCallStack, Typeable f
    , Named a, HGLFW i t m
    , SOP.Generic a
    , SOP.HasDatatypeInfo a
    , SOP.Code a ~ xss, SOP.All2 (Present i) xss
    )
  ⇒ (InputEventMux t, Vocab i (Present i), Dynamic t a)
  → ReadFieldT (Present i) i m a f xss xs
recoverFieldPresentDynamic (mux, voc, dRec) _pC _pIAF _dtinfo _consNr _cinfo (FieldInfo fname) proj = Comp $ do
  tok ← liftIO $ Port.newId $ "record label '" <> pack fname <> "'"
  let addLabel ""  x = x
      addLabel lab x = hbox [ (defLeaf ∷ (x ~ TextLine, As x, Top (Denoted x))
                                ⇒ Port.IdToken → x → Denoted x → Blank i)
                              tok TextLine (pack lab <> ": ")
                            , x
                            ]
  mapWItem @i (addLabel fname) <$> dynPresent @i mux voc (proj <$> dRec)

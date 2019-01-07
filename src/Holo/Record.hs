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
  dynWidget' lbs input tok voc da =
    SOP.unComp $ recover (Proxy @(Present i)) (Proxy @(i, a))
    (\_p _dti → pure 0)
    (recoverFieldWidget (lbs, input, tok, voc, da))

instance {-# OVERLAPPABLE #-}
  (Typeable a
  , SOP.Generic a, SOP.HasDatatypeInfo a, SOP.Code a ~ xss
  , SOP.All2 (Present i) xss
  , HGLFW i t m
  ) ⇒ Present i a where
  present lbs input voc initial =
    SOP.unComp $ recover (Proxy @(Present i)) (Proxy @(i, a))
    (\_p _dti → pure 0)
    (recoverFieldPresent (lbs, input, voc, initial))
  dynPresent lbs input voc da  =
    SOP.unComp $ recover (Proxy @(Present i)) (Proxy @(i, a))
    (\_px _dti→ pure 0)
    (recoverFieldPresentDynamic (lbs, input, voc, da))

recoverFieldWidget ∷ ∀ i t m u f xss xs.
  ( HGLFW i t m
  , SOP.HasDatatypeInfo u, SOP.Code u ~ xss
  , As TextLine, Present i Text
  , Typeable f
  , Present i f
  )
  ⇒ (LBinds, Input t, Port.IdToken, Vocab i (Present i), Dynamic t u)
  → ReadFieldT (Present i) i m u f xss xs
recoverFieldWidget (lbs@(LBinds (_as, _own, chibinds)), input, tok, voc, dRec) _pC _pIAF _dtinfo _consNr _cinfo (FieldInfo fname) proj = Comp $
  mapDesig @i @f voc
  \(_ ∷ n)→ do
      let chiLBS@(LBinds (_, subs, _)) = childBinds lbs (AElt $ pack fname)
      Widget' (ae, sDf,iD,vD) ← dynWidget' @i @(Denoted n) chiLBS input tok voc (forget ∘ proj <$> dRec)
      ivD ← interpretate @i vD
      pure $ Widget' (ae, const $ sDf subs,iD,ivD)

recoverFieldPresent ∷ ∀ i t m u a xss xs.
  ( HGLFW i t m
  , SOP.HasDatatypeInfo u, SOP.Code u ~ xss
  , As TextLine, Present i Text
  , Typeable a
  , Present i a
  )
  ⇒ (LBinds, Input t, Vocab i (Present i), u)
  → ReadFieldT (Present i) i m u a xss xs
recoverFieldPresent (lbs, input, voc, initV ∷ u) _pC _pIAF _dtinfo _consNr _cinfo (FieldInfo fname) proj = Comp $ do
  let fname' = pack fname
  tok ← liftIO $ Port.newId $ "record label '" <> fname' <> "'"
  let addLabel ""  x = x
      addLabel lab x = hbox [ (defLeaf ∷ (x ~ TextLine, As x, Top (Denoted x))
                                ⇒ Port.IdToken → x → Denoted x → Blank i)
                              tok TextLine (pack lab <> ": ")
                            , x
                            ]
      chiLBS@(LBinds (_, subs, _)) = childBinds lbs (AElt $ pack fname)
  Widget' (ae, subsF, item, val) ←  present @i chiLBS input voc (proj initV)
  pure $ Widget' (ae, const $ subsF subs, addLabel fname <$> item, val)


recoverFieldPresentDynamic
  ∷ ∀ i t m a f xss xs.
    ( HasCallStack, Typeable f
    , Named a, HGLFW i t m
    , SOP.Generic a
    , SOP.HasDatatypeInfo a
    , SOP.Code a ~ xss, SOP.All2 (Present i) xss
    )
  ⇒ (LBinds, Input t, Vocab i (Present i), Dynamic t a)
  → ReadFieldT (Present i) i m a f xss xs
recoverFieldPresentDynamic (lbs, input, voc, dRec) _pC _pIAF _dtinfo _consNr _cinfo (FieldInfo fname) proj = Comp $ do
  let fname' = pack fname
  tok ← liftIO $ Port.newId $ "record label '" <> fname' <> "'"
  let addLabel ""  x = x
      addLabel lab x = hbox [ (defLeaf ∷ (x ~ TextLine, As x, Top (Denoted x))
                                ⇒ Port.IdToken → x → Denoted x → Blank i)
                              tok TextLine (pack lab <> ": ")
                            , x
                            ]
      chiLBS@(LBinds (_, subs, _)) = childBinds lbs (AElt $ pack fname)
  Widget' (ae, subsF, item, val) ←  dynPresent @i chiLBS input voc (proj <$> dRec)
  pure $ Widget' (ae, const $ subsF subs, addLabel fname <$> item, val)

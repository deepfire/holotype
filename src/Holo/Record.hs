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

setAE ∷ AElt → Widget i a → Widget i a
setAE ae (Widget' (_,a,b,c)) = Widget' (ae,a,b,c)

instance {-# OVERLAPPABLE #-}
  (Typeable a
  , SOP.Generic a, SOP.HasDatatypeInfo a, SOP.Code a ~ xss
  , SOP.All2 (Present i) xss
  , HGLFW i t m
  ) ⇒ Widgety i a where
  dynWidget' ae tok voc da = do
    lbs   ← getSubLBinds @i ae
    w     ← runWidgetMLBinds @i lbs $ SOP.unComp $ recover (Proxy @(Present i)) (Proxy @(i, a))
            (\_p _dti → pure 0)
            (recoverFieldWidget (lbs, tok, voc, da))
    pure $ setAE ae w

instance {-# OVERLAPPABLE #-}
  (Typeable a
  , SOP.Generic a, SOP.HasDatatypeInfo a, SOP.Code a ~ xss
  , SOP.All2 (Present i) xss
  , HGLFW i t m
  ) ⇒ Present i a where
  present ae voc initial = do
    lbs   ← getSubLBinds @i ae
    w     ← runWidgetMLBinds @i lbs $ SOP.unComp $ recover (Proxy @(Present i)) (Proxy @(i, a))
            (\_p _dti → pure 0)
            (recoverFieldPresent (lbs, voc, initial))
    pure $ setAE ae w
  dynPresent ae voc da  = do
    lbs   ← getSubLBinds @i ae
    w     ← runWidgetMLBinds @i lbs $ SOP.unComp $ recover (Proxy @(Present i)) (Proxy @(i, a))
            (\_px _dti→ pure 0)
            (recoverFieldPresentDynamic (lbs, voc, da))
    pure $ setAE ae w

recoverFieldWidget ∷ ∀ i sig t m u f xss xs.
  ( MonadW i sig t m
  , SOP.HasDatatypeInfo u, SOP.Code u ~ xss
  , As TextLine, Present i Text
  , Typeable f
  , Present i f
  )
  ⇒ (LBinds, Port.IdToken, Vocab i (Present i), Dynamic t u)
  → ReadFieldT (Present i) i m u f xss xs
recoverFieldWidget (lbs, tok, voc, dRec) _pC _pIAF _dtinfo _consNr _cinfo (FieldInfo fname) proj = Comp $
  mapDesig @i @f voc
  \(_ ∷ n)→ do
      Widget' (ae,sD,iD,vD) ← dynWidget' @i @(Denoted n) (AElt $ pack fname) tok voc (forget ∘ proj <$> dRec)
      ivD ← interpretate @i vD
      pure $ Widget' (ae,sD,iD,ivD)

recoverFieldPresent ∷ ∀ i sig t m u a xss xs.
  ( MonadW i sig t m
  , SOP.HasDatatypeInfo u, SOP.Code u ~ xss
  , As TextLine, Present i Text
  , Typeable a
  , Present i a
  )
  ⇒ (LBinds, Vocab i (Present i), u)
  → ReadFieldT (Present i) i m u a xss xs
recoverFieldPresent (lbs, voc, initV ∷ u) _pC _pIAF _dtinfo _consNr _cinfo (FieldInfo fname) proj = Comp $ do
  let fname' = pack fname
  tok ← Port.newId $ "record label '" <> fname' <> "'"
  let addLabel ""  x = x
      addLabel lab x = hbox [ (defLeaf ∷ (x ~ TextLine, As x, Top (Denoted x))
                                ⇒ Port.IdToken → x → Denoted x → Blank i)
                              tok TextLine (pack lab <> ": ")
                            , x
                            ]
  Widget' (ae, subsD, item, val) ←  present @i (AElt $ pack fname) voc (proj initV)
  pure $ Widget' (ae, subsD, addLabel fname <$> item, val)


recoverFieldPresentDynamic
  ∷ ∀ i sig t m a f xss xs.
    ( MonadW i sig t m
    , HasCallStack, Typeable f
    , Named a, HGLFW i t m
    , SOP.Generic a
    , SOP.HasDatatypeInfo a
    , SOP.Code a ~ xss, SOP.All2 (Present i) xss
    )
  ⇒ (LBinds, Vocab i (Present i), Dynamic t a)
  → ReadFieldT (Present i) i m a f xss xs
recoverFieldPresentDynamic (lbs, voc, dRec) _pC _pIAF _dtinfo _consNr _cinfo (FieldInfo fname) proj = Comp $ do
  let fname' = pack fname
  tok ← Port.newId $ "record label '" <> fname' <> "'"
  let addLabel ""  x = x
      addLabel lab x = hbox [ (defLeaf ∷ (x ~ TextLine, As x, Top (Denoted x))
                                ⇒ Port.IdToken → x → Denoted x → Blank i)
                              tok TextLine (pack lab <> ": ")
                            , x
                            ]
  Widget' (ae, subsD, item, val) ←  dynPresent @i (AElt $ pack fname) voc (proj <$> dRec)
  pure $ Widget' (ae, subsD, addLabel fname <$> item, val)

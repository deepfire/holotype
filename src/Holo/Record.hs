{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors -Wno-orphans -Wno-type-defaults #-}
{-# OPTIONS_GHC -ddump-tc-trace #-}
module Holo.Record
  ( Vocab(..)
  , Definition(..)
  )
where
import           ExternalImports

import qualified Graphics.Cairo                    as Cr
import           Holo.Instances
import           Holo.Input
import           Holo.Item
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
  ( Typeable a
  , SOP.Generic a, SOP.HasDatatypeInfo a, SOP.Code a ~ xss
  , SOP.All2 (Present i) xss
  , MonadW i t r m
  ) ⇒ Widgety i a where
  dynWidget' ae tok voc da = do
    lbs ← getSubLBinds @i ae
    w   ← runWidgetMLBinds @i lbs $ do
      recover (Proxy @(Present i)) (Proxy @(i, a))
        (\_p _dti → pure 0) -- XXX: stub that'll obviously break sums -- should be a dynamic for choice
        (recoverFieldWidget (tok, voc, da))
    setAE ae <$> finaliseNodeWidget w

instance {-# OVERLAPPABLE #-}
  (Typeable a
  , SOP.Generic a, SOP.HasDatatypeInfo a, SOP.Code a ~ xss
  , SOP.All2 (Present i) xss
  , MonadW i t r m
  ) ⇒ Present i a where
  present ae voc initial = do
    lbs ← getSubLBinds @i ae
    w   ← runWidgetMLBinds @i lbs $ do
      recover (Proxy @(Present i)) (Proxy @(i, a))
        (\_p _dti → pure 0) -- XXX: stub that'll obviously break sums -- should be a dynamic for choice
        (\a b c d e f g → finaliseNodeWidget =<<
          recoverFieldPresent (voc, initial) a b c d e f g)
    setAE ae <$> finaliseNodeWidget w
  dynPresent ae voc da  = do
    lbs ← getSubLBinds @i ae
    w   ← runWidgetMLBinds @i lbs $ do
      recover (Proxy @(Present i)) (Proxy @(i, a))
        (\_px _dti→ pure 0) -- XXX: stub that'll obviously break sums -- should be a dynamic for choice
        (\a b c d e f g → --finaliseNodeWidget =<<
          recoverFieldPresentDynamic (voc, da) a b c d e f g)
    setAE ae <$> finaliseNodeWidget w

recoverFieldWidget ∷ ∀ i t r m u f xss xs.
  ( MonadW i t r m
  , SOP.HasDatatypeInfo u, SOP.Code u ~ xss
  , As TextLine, Present i Text
  , Typeable f
  , Present i f
  )
  ⇒ (Port.IdToken, Vocab i (Present i), Dynamic t u)
  → ReadFieldT (Present i) i m u f xss xs
recoverFieldWidget (tok, voc, dRec) _pC _pIAF _dtinfo _consNr _cinfo (FieldInfo fname) proj =
  mapDesig @i @f voc
  \(_ ∷ n)→ do
      Widget' (ae,sD,iD,vD) ← dynWidget' @i @(Denoted n) (AElt $ pack fname) tok voc (forget ∘ proj <$> dRec)
      ivD ← interpretate @i vD
      pure $ Widget' (ae,sD,iD,ivD)

recoverFieldPresent ∷ ∀ i t r m u a xss xs.
  ( MonadW i t r m
  , SOP.HasDatatypeInfo u, SOP.Code u ~ xss
  , As TextLine, Present i Text
  , Typeable a
  , Present i a
  )
  ⇒ (Vocab i (Present i), u)
  → ReadFieldT (Present i) i m u a xss xs
recoverFieldPresent (voc, initV ∷ u) _pC _pIAF _dtinfo _consNr _cinfo (FieldInfo fname) proj = do
  let fname' = pack fname
  tok ← Port.newId $ "record label '" <> fname' <> "'"
  let addLabel ""  x = x
      addLabel lab x = hbox [ (defLeaf ∷ (x ~ TextLine, As x, Top (Denoted x))
                                ⇒ Port.IdToken → x → Denoted x → Blank i)
                              tok TextLine (pack lab <> ": ")
                            , x
                            ]
  Widget' (ae, subsD, item, val) ← present @i (AElt $ pack fname) voc (proj initV)
  pure $ Widget' (ae, subsD, addLabel fname <$> item, val)

recoverFieldPresentDynamic
  ∷ ∀ i t r m a f xss xs.
    ( MonadW i t r m
    , HasCallStack, Typeable f
    , Named a
    , SOP.Generic a
    , SOP.HasDatatypeInfo a
    , SOP.Code a ~ xss, SOP.All2 (Present i) xss
    )
  ⇒ (Vocab i (Present i), Dynamic t a)
  → ReadFieldT (Present i) i m a f xss xs
recoverFieldPresentDynamic (voc, dRec) _pC _pIAF _dtinfo _consNr _cinfo (FieldInfo fname) proj = do
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

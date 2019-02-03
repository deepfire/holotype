--{-# OPTIONS_GHC -Wextra -fplugin ThinErr #-}
module Generics.SOP.Monadic
  ( SumChoiceT, PerformChoiceT, ReadFieldT
  , Result
  , recover

  -- reexports
  , DatatypeInfo(..), ConstructorInfo(..), FieldInfo(..)
  )
where
import qualified Data.List                        as L
import qualified Generics.SOP                     as SOP
import qualified Generics.SOP.NP                  as SOP
import qualified Generics.SOP.Traversal           as SOP
import           ExternalImports


-- * Somewhat generic
enumerate ∷ SListI xs ⇒ NP ConstructorInfo xs → NP (((,) Int) :.: ConstructorInfo) xs
enumerate cs = SOP.hliftA2 (\c (K n)→ Comp (n, c)) cs (fromJust $ SOP.fromList $ L.take (SOP.lengthSList cs) [0..])


data family Result    t s ∷ Type

type SumChoiceT        = Int
type ADTChoice   m xss = m SumChoiceT

type PerformChoiceT t m a xss
  = ()
  ⇒ Proxy (t, a)
  → DatatypeInfo xss
  → m SumChoiceT

type ReadFieldT c t m u a xss xs
  = (All c xs, c a)
  ⇒ Proxy c
  → Proxy (t, u, a)
  → DatatypeInfo xss
  → SumChoiceT
  → ConstructorInfo xs
  → FieldInfo a
  → (u → a)
  → m (Result t a)

recover
  ∷ ∀ a (c ∷ Type → Constraint) (t ∷ Type) m xss.
    ( Code a ~ xss, HasDatatypeInfo a
    , All2 c xss
    , HasCallStack, Monad m, Applicative (Result t))
  ⇒ Proxy c
  → Proxy (t, a)
  → PerformChoiceT t m a xss
  → (forall f xs. (c f, All2 c xss) ⇒ ReadFieldT c t m a f xss xs)
  → m (Result t a)
recover pC pTA choicef fieldf = let dti = datatypeInfo (Proxy @a) ∷ DatatypeInfo xss
  in
  case dti of
    ADT _moduleName typeName cInfos → do
      choice ← choicef pTA dti
      let pop        ∷ POP (m :.: Result t) xss     = recover' pC pTA fieldf $ (datatypeInfo (Proxy @a) ∷ DatatypeInfo xss)
          ct         ∷ SOP (m :.: Result t) xss     = (!! choice) $ SOP.apInjs_POP pop
          Comp mrsop ∷ (m :.: Result t) (SOP I xss) = hsequence ct
      case SOP.sList ∷ SOP.SList xss of
        SOP.SCons → (SOP.to <$>) <$> mrsop
    Newtype _moduleName typeName cInfo → do
      let nCInfos -- ~∷ NP ((,) Int :.: ConstructorInfo) '[ '[x]]
            = enumerate $ cInfo :* Nil
          sop        ∷ SOP (m :.: Result t) xss =
            SOP.SOP $ SOP.Z $
            recoverCtor pC pTA fieldf dti
            (SOP.hd nCInfos)
            (SOP.hd ((gtraversals -- ~∷ NP (NP (GTraversal (→) (→) s)) '[ '[x]]
                        )))
          Comp mdsop ∷     (m :.: Result t) (SOP I xss) = hsequence sop
      (SOP.to <$>) <$> mdsop

recover'
  ∷ ∀ a (c ∷ Type → Constraint) (t ∷ Type) m xss.
    ( SOP.Generic a, Code a ~ xss
    , All2 c xss
    , HasCallStack, Monad m)
  ⇒ Proxy c
  → Proxy (t, a)
  → (forall f xs. c f ⇒ ReadFieldT c t m a f xss xs)
  → DatatypeInfo xss
  → POP (m :.: Result t) xss
recover' pC pTA fieldf dti@(ADT _ name cs) =
  POP $ SOP.hcliftA2 (Proxy @(All c))
                     --(Proxy @(All (HasReadField t m a)))
        (recoverCtor pC pTA fieldf dti)
        (enumerate cs)
        (gtraversals ∷ NP (NP (GTraversal (→) (→) a)) xss)
recover' _ _ _ _ = error "Non-ADTs not supported."

-- * 1. Extract the constructor's product of field names
--   2. Feed that to the field-name→action interpreter
recoverCtor
  ∷ ∀ a (c ∷ Type → Constraint) (t ∷ Type) m xss xs.
    ( Code a ~ xss
    , All c xs
    , HasCallStack, Monad m)
  ⇒ Proxy c
  → Proxy (t, a)
  → (forall f. c f ⇒ ReadFieldT c t m a f xss xs)
  → DatatypeInfo xss
  → (((,) SumChoiceT) :.: ConstructorInfo) xs
  → NP (GTraversal (→) (→) a) xs
  → NP (m :.: Result t) xs
recoverCtor pC pTA fieldf dti (Comp (consNr, consi@(Record _ finfos))) travs = recoverFields pC pTA fieldf dti consNr consi travs finfos
recoverCtor pC pTA fieldf dti (Comp (consNr, consi@Constructor{}))     travs = recoverFields pC pTA fieldf dti consNr consi travs (SOP.hpure (FieldInfo ""))
recoverCtor _ _ _ (ADT _ name _) _ _ =
  error $ printf "Infix ADTs not supported: type %s." name

-- * Key part:  NP (K Text) xs → NP m xs
--   convert a product of field names to a product of monadic actions yielding 'a'
recoverFields
  ∷ ∀ (c ∷ Type → Constraint) (t ∷ Type) m u xss xs.
    ( Code u ~ xss
    , All c xs
    , SListI xs
    , HasCallStack, Monad m)
  ⇒ Proxy c
  → Proxy (t, u)
  → (forall a. ReadFieldT c t m u a xss xs)
  → DatatypeInfo xss
  → SumChoiceT
  → ConstructorInfo xs
  → NP (GTraversal (→) (→) u) xs
  → NP (FieldInfo) xs
  → NP (m :.: Result t) xs
recoverFields pC _pTU fieldf dtinfo consNr cinfo traversals finfos =
  hcliftA2 pC
  (recoverField dtinfo consNr cinfo)
  finfos
  traversals
  where
    recoverField ∷ ∀ a. (c a)
                 ⇒ DatatypeInfo xss
                 → SumChoiceT
                 → ConstructorInfo xs
                 → FieldInfo a
                 → GTraversal (→) (→) u a
                 → (m :.: Result t) a
    recoverField dtinfo consNr cinfo finfo trav =
      Comp $ fieldf (Proxy @c) (Proxy @(t, u, a))
      dtinfo consNr cinfo finfo (gtravget trav ∷ u → a)

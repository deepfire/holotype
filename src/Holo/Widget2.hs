{-# OPTIONS_GHC -Weverything #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors -Wno-missing-import-lists -Wno-implicit-prelude -Wno-monomorphism-restriction -Wno-name-shadowing -Wno-all-missed-specialisations -Wno-unsafe -Wno-missing-export-lists -Wno-type-defaults -Wno-partial-fields -Wno-missing-local-signatures -Wno-orphans #-}
{-# OPTIONS_GHC -ddump-tc-trace #-}
module Holo.Widget2
  ( module Holo.Classes
  , module Holo.Item
  )
where

import           Data.Foldable
import           Data.Functor.Misc                        (Const2(..))
import           Data.Typeable
import           GHC.Types                                (Constraint)
import qualified Data.TypeMap.Dynamic              as TM
import           Generics.SOP                             (Proxy, Top)
import           Generics.SOP.Monadic
import           GHC.TypeLits
import           Reflex                            hiding (Query, Query(..))
import           Reflex.GLFW                              (RGLFW)
import "GLFW-b"  Graphics.UI.GLFW                  as GL
import qualified Data.Map.Monoidal.Strict          as MMap
import qualified Data.Sequence                     as Seq
import qualified Data.Set                          as Set
import qualified Data.Text                         as T
import qualified Reflex.GLFW                       as GLFW

-- Local imports
import {-# SOURCE #-}
                 Holo.Classes
import           Holo.Input
import {-# SOURCE #-}
                 Holo.Item
import {-# SOURCE #-}
                 Holo.Name
import           Holo.Port
import           Holo.Prelude



-- * The final type class assembly.
--
-- | Vocabulary stores two kinds of entries: interpretation
data Definition (i ∷ Type) (a ∷ Type) where
  Denot      ∷ (Typeable a, As n, Denoted n ~ a, Mutable a, Named a, Widgety i a) ⇒             n → Definition i a

-- | 'Vocab' establishes names for a bunch of types.
newtype Vocab (i ∷ Type) (c ∷ Type → Constraint) = Vocab (TM.TypeMap (HoloTag i))
type instance              TM.Item    (HoloTag i) a = Definition i a
data                                   HoloTag i

type HGLFW i t m = (t ~ (APIt i), m ~ (APIm i), RGLFW t m)

data API t m

type family APIt a ∷ Type where
  APIt (API t _) = t
  APIt _         = TypeError ('Text "APIt on non-API.")

type family APIm a ∷ (Type → Type) where
  APIm (API _ m) = m
  APIm _         = TypeError ('Text "APIm on non-API.")

type Blank   i   = Item Top PBlank
type WH      i   = (Dynamic (APIt i) Subscription, Dynamic (APIt i) (Blank i))
type WF      i b = (Dynamic (APIt i) Subscription, Dynamic (APIt i) (Blank i), Dynamic (APIt i) b)

type Widget  i b = Result i b
data instance      Result i b =
  Reflex (APIt i) ⇒ W { fromW ∷ WF i b }

vocDenot ∷ Typeable a ⇒ Proxy a → Vocab i c → Maybe (Definition i a)
vocDenot = undefined

newMutatedSeedWidget
  ∷ ∀ i t m a
  . (HGLFW i t m, Typeable a, HasCallStack)
  ⇒ InputEventMux t → Vocab i (Present i) → a → m (Widget i a)
newMutatedSeedWidget imux voc initial =
  case vocDenot (Proxy @a) voc of
    Just (Denot _ ∷ Definition i a) → do
      tok ← iNewToken $ Proxy @a
      mut ← mutate (forget initial) $ select imux $ Const2 tok
      dynWidget' tok voc mut

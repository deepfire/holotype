-- | Generalized traversals
--
-- Intended to be imported qualified
--
-- > import Generics.SOP.Traversal as GTraversal
--
module Generics.SOP.Traversal (
    -- * Generalized traversals
    GTraversal
  , traversal
  , get
  , modify
  , set
    -- * Conversion
  , fromLens
  , fromIso
  , toLens
    -- * Generic computation of lenses for record type
  , gtraversals
    -- * Labels for the representation types
  , np
  , rep
  , sop
  , head
  , tail
  , i
  ) where

--import Prelude hiding (id, (.), curry, uncurry, const, head, tail)
import           Control.Arrow                     hiding ((<+>))
import           Control.Category                  hiding ((.), id, const)
import qualified Control.Category                  as C
import qualified Data.Label.Mono                   as DLM
import qualified Data.Label.Point                  as DLP
import qualified Data.Label.Mono                   as Lens

import           Generics.SOP                      hiding (Generic, from)
import qualified Generics.SOP                      as SOP
import qualified GHC.Generics                      as GHC

{-------------------------------------------------------------------------------
  Generalized lens using two categories
-------------------------------------------------------------------------------}

-- | GTraversal generalizes a monomorphic lens by allowing for different categories
-- for the getter and modifier
data GTraversal r w z b = GTraversal (r z b) (w (w b b, z) z)

instance (Category r, ArrowApply w) => Category (GTraversal r w) where
  id = GTraversal C.id app
  (.) (GTraversal f m) (GTraversal g n) = GTraversal (f C.. g) (suncurry (scurry n . scurry m))

traversal :: r a b -> w (w b b, a) a -> GTraversal r w a b
traversal = GTraversal

gtravget :: GTraversal r w a b -> r a b
gtravget (GTraversal f _) = f

smodify :: GTraversal r w a b -> w (w b b, a) a
smodify (GTraversal _ g) = g

gtravset :: Arrow w => GTraversal r w a b -> w (b, a) a
gtravset l = smodify l . first (arr const)

{-------------------------------------------------------------------------------
  Conversion
-------------------------------------------------------------------------------}

fromLens :: (Arrow r, ArrowApply w) => DLM.Lens (->) a b -> GTraversal r w a b
fromLens l =
  GTraversal (arr (Lens.get l))
        (suncurry $ \h -> arr (Lens.set l) . (h . arr (Lens.get l) &&& id))

fromIso :: (Arrow r, ArrowApply w) => DLP.Iso (->) a b -> GTraversal r w a b
fromIso (DLP.Iso f g) = GTraversal (arr f) (suncurry $ \h -> arr g . h . arr f)

toLens :: GTraversal cat cat a b -> DLM.Lens cat a b
toLens (GTraversal f g) = Lens.lens f g

{-------------------------------------------------------------------------------
  Generic computation of all traversals for a record type
-------------------------------------------------------------------------------}

gtraversals :: forall r w y xss . ( SOP.Generic y, Code y ~ xss
                                  , Arrow r, ArrowApply w
                                  , SListI xss
                                  , All SListI xss)
            => NP (NP (GTraversal r w y)) xss
gtraversals =
  hliftA
  (\(CTraversals p :: CTraversals r w xss xs)->
   (hliftA
    (\l -> l . p . sop . rep)
    np))
  nptraversals

-- | Traversal pack selector for some constructor
data CTraversals r w xss xs
  = (SListI xss, All SListI xss, SListI xs)
  => CTraversals (GTraversal r w (NS (NP I) xss) (NP I xs))

nptraversals :: forall r w xss. (Arrow r, ArrowApply w, SListI xss, All SListI xss) => NP (CTraversals r w xss) xss
nptraversals = case sList :: SList xss of
  SNil  -> Nil
  SCons ->
    (CTraversals
      ((fromIso $ Iso (\(Z x) -> x) Z)
        :: GTraversal r w (NS (NP I) (xs1 ': xss')) (NP I xs1))
      :: (SListI xss', All SListI xss', SListI xs) =>       CTraversals r w (xs ': xss') xs)
    :*
    (hliftA
     ((\(CTraversals x) -> CTraversals (x . tail'))
      :: (SListI xss', All SListI xss', SListI xs)
      => CTraversals r w        xss'  x
      -> CTraversals r w (xs ': xss') x)
     (nptraversals :: (SListI xss', All SListI xss') => NP (CTraversals r w        xss')  xss')
     :: (SListI xss', All SListI xss', SListI xs) =>    NP (CTraversals r w (xs ': xss')) xss')

stail' :: (Arrow r, ArrowApply w, SListI xss, SListI xs) => GTraversal r w (NS (NP I) (xs ': xss)) (NS (NP I) xss)
stail' = fromLens $ Lens.lens (\(S xs) -> xs) (\(f, S xs) -> (S (f xs)))

stail  :: (Arrow r, ArrowApply w) =>                        GTraversal r w (NP f       (x ': xs))      (NP f  xs)
stail  = fromLens $ Lens.lens (\(_ :* xs) -> xs) (\(f, x :* xs) -> (x :* f xs))

np :: forall r w xs. (Arrow r, ArrowApply w, SListI xs) => NP (GTraversal r w (NP I xs)) xs
np = case sList :: SList xs of
      SNil  -> Nil
      SCons -> (i . shead            ::                   GTraversal r w (NP I (x ': xs1)) x)
               :*
               (hliftA (. stail) (np :: SListI xs1 => NP (GTraversal r w (NP I       xs1))  xs1)
                                     :: SListI xs1 => NP (GTraversal r w (NP I (x ': xs1))) xs1)

sop :: (Arrow r, ArrowApply w) => GTraversal r w (SOP I xss) (NS (NP I) xss)
sop = fromIso $ Iso (\(SOP x) -> x)  SOP

{-------------------------------------------------------------------------------
  Generalized lenses for representation types
-------------------------------------------------------------------------------}

i :: (Arrow r, ArrowApply w) => GTraversal r w (I a) a
i = fromIso $ Iso unI I

head :: (Arrow r, ArrowApply w) => GTraversal r w (NP f (x ': xs)) (f x)
head = fromLens $ Lens.lens
  (\(x :* _) -> x)
  (\(f, x :* xs) -> (f x :* xs))

rep :: (Arrow r, ArrowApply w, SOP.Generic a) => GTraversal r w a (Rep a)
rep = fromIso $ Iso SOP.from SOP.to

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

sconst :: Arrow arr => c -> arr b c
sconst a = arr (\_ -> a)

scurry :: Arrow cat => cat (a, b) c -> (a -> cat b c)
scurry m a = m . (const a &&& id)

suncurry :: ArrowApply cat => (a -> cat b c) -> cat (a, b) c
suncurry a = app . arr (first a)

------------------------------------------------------------------------------

data Ex = Num Int | Add { left :: Ex, right :: Ex } | Yay { x :: Ex } deriving (Show, GHC.Generic)
instance SOP.Generic Ex

expps :: Code Ex ~ xss => NP (NP (GTraversal (->) (->) Ex)) xss
expps = gtraversals

(,,,) (numX,pnx) (addX,pax) (addY,pay) (yayX,pyx) =
  let (((pnx@(GTraversal numX numX') :* Nil)) :*
       ((pax@(GTraversal addX addX') :* pay@(GTraversal addY addY') :* Nil)) :*
       ((pyx@(GTraversal yayX yayX') :* Nil)) :* Nil) = expps
  in (,,,) (numX,pnx) (addX,pax) (addY,pay) (yayX,pyx)

num = Num 0
add = Add (Num 1) (Num 2)
yay = Yay (Num 3)

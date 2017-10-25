--{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}
--{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# LANGUAGE ConstraintKinds, DataKinds, KindSignatures, TypeApplications, TypeInType #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, InstanceSigs, MultiParamTypeClasses, NoMonomorphismRestriction, RankNTypes, TypeSynonymInstances, UndecidableInstances #-}
{-# LANGUAGE GADTs, TypeFamilies, TypeFamilyDependencies #-}
{-# LANGUAGE BangPatterns, MultiWayIf, OverloadedStrings, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TypeOperators #-}
{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveGeneric, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Flatland where

-- Basis
import           Prelude.Unicode

-- Type-level
import           GHC.Generics                             (Generic)
import           GHC.TypeLits
import           GHC.Types                         hiding (Constraint)

-- General types
import           Control.Applicative
import           Control.Applicative.Free
import           Control.Lens                      hiding (children)
import           Control.Monad.Random              hiding (lift)
import           Control.Monad.State               hiding (lift)
import           Data.Complex
import           Data.Function
import           Data.List
import           Data.Lub
import           Data.Maybe
import           Data.Glb
import           Data.MeasuredMonoid
import           Data.MonoTraversable
import           Data.Monoid
import           Data.Text.Format
import qualified Data.Text.Format                  as T
import qualified Data.Text.Lazy                    as TL
import           Data.Type.Bool

-- Algebra
import           Linear                            hiding (trace)

-- glib-introspection -based Pango
import qualified GI.Pango                          as GIP (unitsToDouble, unitsFromDouble)

-- Misc
import           Debug.Trace                              (trace)
import           Text.PrettyPrint.Leijen.Text      hiding ((<>), (<$>), space)
import           Text.Printf                              (printf)

-- Dirty stuff
import qualified Foreign                           as F
import qualified System.IO.Unsafe                  as UN

-- Local
import           Elsewhere

import qualified Data.Map                          as Map


-- * Dimensional density.
newtype PΠ = PΠ { pπVal ∷ Double } deriving (Num, Show)
pπ ∷ PΠ
pπ = 72

newtype DΠ = DΠ { fromDΠ ∷ Double } deriving (Num, Show)

-- $Note [Pango resolution & unit conversion]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--   > Sets the resolution for the fontmap. This is a scale factor between
--   > points specified in a #PangoFontDescription and Cairo units. The
--   > default value is 96, meaning that a 10 point font will be 13
--   > units high. (10 * 96. / 72. = 13.3).
--   cited from: https://git.gnome.org/browse/pango/tree/pango/pangocairo-fontmap.c#n236
--   I.e.:
--     units  = points ⋅ dπ / pπ
--     points = units  / dπ ⋅ pπ
--
--   A good situation report is also at:
--     https://mail.gnome.org/archives/gtk-i18n-list/2002-May/msg00004.html
--
-- double pango_units_to_double   (int i)    { return (double)i / PANGO_SCALE; }
-- int    pango_units_from_double (double d) { return (int)floor (d * PANGO_SCALE + 0.5); }


-- * Universal, multi-density linear size.

data Unit = PU | PUI | Pt | Ratio

type instance Element (Size PU)    = Double
type instance Element (Size PUI)   = F.Int32
type instance Element (Size Pt)    = F.Int32
type instance Element (Size Ratio) = Double

data Size (u ∷ Unit) where
  PUs    ∷ { fromPU    ∷ !(Element (Size PU))    } → Size PU    -- ^ Pango size, in device units
  PUIs   ∷ { fromPUI   ∷ !(Element (Size PUI))   } → Size PUI   -- ^ Pango size, in device units, scaled by PANGO_SCALE
  Pts    ∷ { fromPt    ∷ !(Element (Size Pt))    } → Size Pt    -- ^ Pango size, in points (at 72ppi--see PΠ above--rate), device-agnostic
  PRatio ∷ { fromRatio ∷ !(Element (Size Ratio)) } → Size Ratio -- ^ Relative
type family SizeType a = r | r → a where
  SizeType (Size PU)    = PU
  SizeType (Size PUI)   = PUI
  SizeType (Size Pt)    = Pt
  SizeType (Size Ratio) = Ratio

-- <Boilerplate>
deriving instance Eq   (Size u)
deriving instance Show (Size u)
instance Fractional (Size PU) where
  fromRational x = PUs $ fromRational x
  recip          = omap recip
instance MonoFunctor (Size PU)    where omap f (PUs    x) = PUs    (f x)
instance MonoFunctor (Size PUI)   where omap f (PUIs   x) = PUIs   (f x)
instance MonoFunctor (Size Pt)    where omap f (Pts    x) = Pts    (f x)
instance MonoFunctor (Size Ratio) where omap f (PRatio x) = PRatio (f x)
instance Ord (Size PU)    where PUs    l <= PUs    r = l <= r
instance Ord (Size PUI)   where PUIs   l <= PUIs   r = l <= r
instance Ord (Size Pt)    where Pts    l <= Pts    r = l <= r
instance Ord (Size Ratio) where PRatio l <= PRatio r = l <= r
instance Num (Size PU)    where fromInteger = PUs    ∘ fromIntegral; PUs    x + PUs    y = PUs    $ x + y; PUs    x * PUs    y = PUs    $ x * y; abs = omap abs; signum = omap signum; negate = omap negate
instance Num (Size PUI)   where fromInteger = PUIs   ∘ fromIntegral; PUIs   x + PUIs   y = PUIs   $ x + y; PUIs   x * PUIs   y = PUIs   $ x * y; abs = omap abs; signum = omap signum; negate = omap negate
instance Num (Size Pt)    where fromInteger = Pts    ∘ fromIntegral; Pts    x + Pts    y = Pts    $ x + y; Pts    x * Pts    y = Pts    $ x * y; abs = omap abs; signum = omap signum; negate = omap negate
instance Num (Size Ratio) where fromInteger = PRatio ∘ fromIntegral; PRatio x + PRatio y = PRatio $ x + y; PRatio x * PRatio y = PRatio $ x * y; abs = omap abs; signum = omap signum; negate = omap negate

instance Random (Size PU) where
  randomR (PUs a, PUs a')   = runState $ liftA PUs $ state $ randomR (a, a')
  random                    = runState $ liftA PUs $ state random
instance Random (Size PUI) where
  randomR (PUIs a, PUIs a') = runState $ liftA PUIs $ state $ randomR (a, a')
  random                    = runState $ liftA PUIs $ state random
instance Random (Size Pt) where
  randomR (Pts a, Pts a')   = runState $ liftA Pts $ state $ randomR (a, a')
  random                    = runState $ liftA Pts $ state random
instance Random a ⇒ Random (V2 a) where
  randomR (V2 x y, V2 x' y') = runState $ liftA2 V2 (state $ randomR (x, x')) (state $ randomR (y, y'))
  random                     = runState $ liftA2 V2 (state random) (state random)
instance Random a ⇒ Random (V3 a) where
  randomR (V3 x y z, V3 x' y' z') = runState $ liftA3 V3 (state $ randomR (x, x')) (state $ randomR (y, y')) (state $ randomR (z, z'))
  random                          = runState $ liftA3 V3 (state random) (state random) (state random)
instance Random a ⇒ Random (V4 a) where
  randomR (V4 x y z w, V4 x' y' z' w') = runState $ V4 <$> (state $ randomR (x, x')) <*> (state $ randomR (y, y')) <*> (state $ randomR (z, z')) <*> (state $ randomR (w, w'))
  random                               = runState $ V4 <$> (state random) <*> (state random) <*> (state random) <*> (state random)
-- </Boilerplate>

-- | Conversion between unit sizes -- See Note [Pango resolution & unit conversion]
-- class a ~ (Size (SizeType a)) ⇒ Sizely a where
--   fromSz ∷ Sizely b ⇒ DΠ → b → a
class Sizely a where
  fromSz ∷ Sizely (Size b) ⇒ DΠ → (Size b) → a

instance Sizely (Size PU) where
  fromSz _       x@(PUs _)    = x
  fromSz _         (PUIs pis) = PUs $ UN.unsafePerformIO ∘ GIP.unitsToDouble   $ pis -- fromIntegral pis / fromIntegral GIP.SCALE
  fromSz (DΠ dπ)   (Pts pts)  = PUs $ (fromIntegral pts) ⋅ dπ / pπVal pπ
  fromSz _         _          = error "x -fromSz→ PU: unsupported combination, for an unknown type of 'x'."
instance Sizely (Size PUI) where
  fromSz _       x@(PUIs _)   = x
  fromSz _         (PUs pus)  = PUIs $ UN.unsafePerformIO ∘ GIP.unitsFromDouble $ pus -- floor $ pus ⋅ fromIntegral GIP.SCALE
  fromSz (DΠ dπ)   (Pts pts)  = PUIs $ UN.unsafePerformIO ∘ GIP.unitsFromDouble $ fromIntegral pts ⋅ dπ / pπVal pπ
  fromSz _         _          = error "x -fromSz→ PUI: unsupported combination, for an unknown type of 'x'."
instance Sizely (Size Pt) where
  fromSz _       x@(Pts _)    = x
  fromSz (DΠ dπ)   (PUs pus)  = Pts $ floor $                                                                 pus ⋅ pπVal pπ / dπ
  fromSz (DΠ dπ)   (PUIs pis) = Pts $ floor $ UN.unsafePerformIO ∘ GIP.unitsToDouble ∘ floor $ (fromIntegral pis) ⋅ pπVal pπ / dπ
  fromSz _         _          = error "x -fromSz→ Pt: unsupported combination, for an unknown type of 'x'."
-- instance Sizely (Size u) where
--   fromSz dπ x = fromSz dπ x
-- not sure if this even has any added value..
-- class (Functor a, Sizely b) ⇒ Sizeable a b where
--   fromSzable ∷ Sizeable a (Size c) ⇒ DΠ → a (Size c) → a b
--   fromSzable dπ = fmap (fromSz dπ)


-- * Specialized linear dimension classification:

infinity ∷ Double
infinity = read "Infinity"

type Lin a = (Fractional a, Ord a, Num a)

newtype R   a = R   { _r'val  ∷ a } deriving (Eq, Fractional, Functor, Num) -- ^ Radius
newtype An  a = An  { _an'val ∷ a } deriving (Eq, Fractional, Functor, Num) -- ^ Angle
newtype Th  a = Th  { _th'val ∷ a } deriving (Eq, Fractional, Functor, Num) -- ^ Thickness
newtype He  a = He  { _he'val ∷ a } deriving (Eq, Fractional, Functor, Num) -- ^ Height
newtype Wi  a = Wi  { _wi'val ∷ a } deriving (Eq, Fractional, Functor, Num) -- ^ Width
instance Applicative R  where pure = R;  R  f <*> R  x = R  $ f x
instance Applicative An where pure = An; An f <*> An x = An $ f x
instance Applicative Th where pure = Th; Th f <*> Th x = Th $ f x
instance Applicative He where pure = He; He f <*> He x = He $ f x
instance Applicative Wi where pure = Wi; Wi f <*> Wi x = Wi $ f x
instance Additive R  where zero = R  0
instance Additive An where zero = An 0
instance Additive Th where zero = Th 0
instance Additive He where zero = He 0
instance Additive Wi where zero = Wi 0
deriving instance Foldable R
deriving instance Foldable An
deriving instance Foldable Th
deriving instance Foldable He
deriving instance Foldable Wi
-- instance Metric R  where
-- instance Metric An where
-- instance Metric Th where
-- instance Metric He where
-- instance Metric Wi where
deriving instance Show a ⇒ Show (R  a)
deriving instance Show a ⇒ Show (An a)
deriving instance Show a ⇒ Show (Th a)
deriving instance Show a ⇒ Show (He a)
deriving instance Show a ⇒ Show (Wi a)
deriving instance Random a ⇒ Random (R a)
deriving instance Random a ⇒ Random (An a)
deriving instance Random a ⇒ Random (Th a)
deriving instance Random a ⇒ Random (He a)
deriving instance Random a ⇒ Random (Wi a)
makeLenses ''R
makeLenses ''An
makeLenses ''Th
makeLenses ''He
makeLenses ''Wi


-- * Pairing dimensions:
--
-- XXX: HUGE NOTE: V2/Co/Wrap mass of code below is likely silliness,
--      easily replaced with a couple of short lens operators.
--      Nevertheless, it is what it is -- because of blissful lack of education.
--      Typical.
--
-- In particular, this screams for Linear.Affine
--
newtype  Di a =  Di { _di'v  ∷ V2 a }      deriving (Additive, Applicative, Eq, Fractional, Functor, Num) -- ^ Dimensions
newtype  Po a =  Po { _po'v  ∷ V2 a }      deriving (Additive, Applicative, Eq, Fractional, Functor, Num) -- ^ Coordinates, Descartes
newtype RPo a = RPo { _rpo'v ∷ Complex a } deriving (Additive, Applicative, Eq, Fractional, Functor, Num) -- ^ Coordinates, polar

-------- <boilerplate>
deriving instance Show a ⇒ Show  (Di a); deriving instance Show a ⇒ Show  (Po a); deriving instance Show a ⇒ Show  (RPo a)
deriving instance (Ord a) ⇒ HasLub (Di a)
deriving instance (Ord a) ⇒ HasGlb (Di a)
deriving instance (Ord a) ⇒ HasLub (Po a)
deriving instance (Ord a) ⇒ HasGlb (Po a)
newtype An2 a = An2 { _an2V ∷ V2 a } deriving (Eq, Functor) -- ^ Unordered pair of angles
newtype Co  a = Co  { _coV  ∷ V4 a } deriving (Eq, Functor) -- ^ Color
deriving instance Show a ⇒ Show (An2 a)
deriving instance Show a ⇒ Show (Co a)
deriving instance Random a ⇒ Random (Di a)
deriving instance Random a ⇒ Random (Po a)
deriving instance Random a ⇒ Random (RPo a)
deriving instance Random a ⇒ Random (Co a)
deriving instance Random a ⇒ Random (An2 a)

-- XXX: -ddump-deriv this:
-- deriving instance Monoid a ⇒ Monoid (Di a)
instance Num a ⇒ Monoid (Di a) where
  mempty              = Di zero
  Di l `mappend` Di r = Di $ l + r

instance Num a ⇒ Monoid (Po a) where
  mempty              = Po zero
  Po l `mappend` Po r = Po $ l + r

-- instance Applicative RPo where pure x = RPo (R x, An x); RPo (R fr, An fan) <*> RPo (R r, An an) = RPo (R $ fr r, An $ fan an)
-- instance Additive    RPo where zero = RPo (zero, zero)
-- instance Num a ⇒ Num (RPo a) where
--   (+) = liftA2 (+)

makeLenses ''Di
makeLenses ''Po
makeLenses ''RPo
makeLenses ''An2
makeLenses ''Co
instance Show a ⇒ Pretty (Di a) where pretty = text ∘ ("#<Di " <>) ∘ (<> ">") ∘ ppV2 ∘ (^.di'v)
instance Show a ⇒ Pretty (Po a) where pretty = text ∘ ("#<Po " <>) ∘ (<> ">") ∘ ppV2 ∘ (^.po'v)
---------- </boilerplate>


di ∷ Wi a → He a  → Di a
di  (Wi x) (He y) = Di $ V2 x y
po ∷ a → a → Po a
po x y = Po $ V2 x y
rpo ∷ Floating a ⇒ R a → An a → RPo a
rpo (R r) (An a) = RPo (mkPolar r a)
an2 ∷ a → a → An2 a
an2 x y = An2 $ V2 x y
co ∷ a → a → a → a → Co a
co r g b a = Co $ V4 r g b a

infinite ∷ Di Double
infinite = Di $ V2  infinity infinity

po2rpo ∷ Po a → RPo a
po2rpo (Po (V2 x y))  = RPo $ x :+ y
rpo2po ∷ RPo a → Po a
rpo2po (RPo (x :+ y)) = Po $ V2 x y

rotateRPo ∷ RealFloat a ⇒ An a → RPo a → RPo a
rotateRPo (An a) (RPo c) = RPo $ mkPolar (magnitude c) (phase c + a)

po'add ∷ Num a ⇒ V2 a → Po a → Po a
po'add v _po = Po (v ^+^ _po'v _po)

poDelta ∷ Num a ⇒ Po a → Po a → Di a
poDelta (Po v) (Po v') = Di $ v ^-^ v'

goldXdi ∷ RealFrac a ⇒ Wi a → Di a
goldXdi (Wi x) = Di $ V2  x               (x / realToFrac goldenRatio)
goldYdi ∷ RealFrac a ⇒ He a → Di a
goldYdi (He y) = Di $ V2 (y / realToFrac goldenRatio) y

di'd   ∷ Axes → Lens' (Di a) a
di'd   X f (Di (V2 x y)) = Di ∘ (flip V2 y) <$> f x
di'd   Y f (Di (V2 x y)) = Di ∘ (id   V2 x) <$> f y
po'd   ∷ Axes → Lens' (Po a) a
po'd   X f (Po (V2 x y)) = Po ∘ (flip V2 y) <$> f x
po'd   Y f (Po (V2 x y)) = Po ∘ (id   V2 x) <$> f y


-- * Axes
--
data Axes = X | Y deriving (Eq, Show)

other'axis ∷ Axes → Axes
other'axis X = Y
other'axis Y = X

-- * Axis-derived operation
--
di'axisMajor'add'max ∷ (Num a, Ord a) ⇒ Axes → Di a → Di a → Di a
di'axisMajor'add'max X  (Di (V2 lx ly)) (Di (V2 rx ry)) = Di $ V2 (lx   +   rx) (ly `max` ry)
di'axisMajor'add'max Y  (Di (V2 lx ly)) (Di (V2 rx ry)) = Di $ V2 (lx `max` rx) (ly   +   ry)


-- * Orientation: _ north-west, clockwise to west.
--
data OKind  = Card | Corn
data Orient (k ∷ OKind) where
  ONW ∷ Orient Corn
  ON  ∷ Orient Card
  ONE ∷ Orient Corn
  OE  ∷ Orient Card
  OSE ∷ Orient Corn
  OS  ∷ Orient Card
  OSW ∷ Orient Corn
  OW  ∷ Orient Card
deriving instance Eq   (Orient k)
deriving instance Show (Orient k)

ori'vec ∷ Num a ⇒ Orient b → V2 a
ori'vec ON  = V2  0 (-1)
ori'vec ONE = V2  1 (-1)
ori'vec OE  = V2  1   0
ori'vec OSE = V2  1   1
ori'vec OS  = V2  0   1
ori'vec OSW = V2(-1)  1
ori'vec OW  = V2(-1)  0
ori'vec ONW = V2(-1)(-1)

-- | Given a radius 'r' and an orientation 'o', return a decomposition
--   of the corresponding vector as a pair of two-dimensional vectors.
ori'vec'pair ∷ Num a ⇒ R a → Orient b → (V2 a, V2 a)
ori'vec'pair (R r) o | ON  ← o = (z, n) | ONE ← o = (e, n)
                  | OE  ← o = (e, z) | OSE ← o = (e, s)
                  | OS  ← o = (z, s) | OSW ← o = (w, s)
                  | OW  ← o = (w, z) | ONW ← o = (w, n)
  where n = V2  0(-r)
        s = V2  0  r
        e = V2  r  0
        w = V2(-r) 0
        z = V2  0  0

-- | Given an orientation 'o', a center-point at 'c' and a radius 'r', compute a
--   clockwise chord vector, with endpoints residing on a circle of specified
--   radius, and that is orthogonal to the specified orientation.
chord'CW ∷ Num a ⇒ Orient Corn → Po a → R a → (Po a, Po a)
chord'CW o c r
  | ONE ← o = (oy, ox)
  | OSE ← o = (ox, oy)
  | OSW ← o = (oy, ox)
  | ONW ← o = (ox, oy)
  where (vx, vy) = ori'vec'pair r o
        (ox, oy) = (po'add vx c, po'add vy c)


-- * Cstr, Reqt, Orig, LU:
-- - constraint
-- - requirement
-- - origin, center-based
-- - left-upper corner
--
newtype Cstr d = Cstr { _cstr'di ∷ Di d } deriving (Additive, Applicative, Functor, Eq, Monoid, Num, Show)
newtype Reqt d = Reqt { _reqt'di ∷ Di d } deriving (Additive, Applicative, Functor, Eq, Monoid, Num, Show)
newtype Orig d = Orig { _orig'po ∷ Po d } deriving (Additive, Applicative, Functor, Eq, Monoid, Num, Show)
newtype LU   d = LU   { _lu'po   ∷ Po d } deriving (Additive, Applicative, Functor, Eq, Monoid, Num, Show)
makeLenses ''Cstr
makeLenses ''Reqt
makeLenses ''Orig
makeLenses ''LU

cstr'v ∷ Lens' (Cstr a) (V2 a)
cstr'v f (Cstr (Di v)) = Cstr ∘ Di <$> f v
reqt'v ∷ Lens' (Reqt a) (V2 a)
reqt'v f (Reqt (Di v)) = Reqt ∘ Di <$> f v
orig'v ∷ Lens' (Orig a) (V2 a)
orig'v f (Orig (Po v)) = Orig ∘ Po <$> f v
lu'v   ∷ Lens' (LU   a) (V2 a)
lu'v   f (LU   (Po v)) = LU   ∘ Po <$> f v

cstr'd ∷ Axes → Lens' (Cstr a) a
cstr'd X f (Cstr (Di (V2 x y))) = Cstr ∘ Di ∘ (flip V2 y) <$> f x
cstr'd Y f (Cstr (Di (V2 x y))) = Cstr ∘ Di ∘ (id   V2 x) <$> f y
reqt'd ∷ Axes → Lens' (Reqt a) a
reqt'd X f (Reqt (Di (V2 x y))) = Reqt ∘ Di ∘ (flip V2 y) <$> f x
reqt'd Y f (Reqt (Di (V2 x y))) = Reqt ∘ Di ∘ (id   V2 x) <$> f y
orig'd ∷ Axes → Lens' (Orig a) a
orig'd X f (Orig (Po (V2 x y))) = Orig ∘ Po ∘ (flip V2 y) <$> f x
orig'd Y f (Orig (Po (V2 x y))) = Orig ∘ Po ∘ (id   V2 x) <$> f y
lu'd   ∷ Axes → Lens' (LU a) a
lu'd   X f (LU   (Po (V2 x y))) = LU   ∘ Po ∘ (flip V2 y) <$> f x
lu'd   Y f (LU   (Po (V2 x y))) = LU   ∘ Po ∘ (id   V2 x) <$> f y

instance Show a ⇒ Pretty (Cstr a) where pretty = text ∘ ("#<Cstr " <>) ∘ (<> ">") ∘ ppV2 ∘ (^.cstr'v)
instance Show a ⇒ Pretty (Reqt a) where pretty = text ∘ ("#<Reqt " <>) ∘ (<> ">") ∘ ppV2 ∘ (^.reqt'v)
instance Show a ⇒ Pretty (Orig a) where pretty = text ∘ ("#<Orig " <>) ∘ (<> ">") ∘ ppV2 ∘ (^.orig'v)
instance Show a ⇒ Pretty (LU   a) where pretty = text ∘ ("#<LU "   <>) ∘ (<> ">") ∘ ppV2 ∘ (^.lu'v)

reqt'add  ∷ Lin d ⇒ Axes → Reqt d → Reqt d → Reqt d
reqt'add X  (Reqt (Di (V2 lx ly))) (Reqt (Di (V2 rx ry))) = Reqt ∘ Di $ V2 (lx   +   ly) (rx `max` ry)
reqt'add  Y (Reqt (Di (V2 lx ly))) (Reqt (Di (V2 rx ry))) = Reqt ∘ Di $ V2 (lx `max` ly) (rx   +   ry)

orig'lu ∷ Lin d ⇒ Reqt d → Orig d → LU d
orig'lu (Reqt (Di size)) = LU   ∘ (& po'v %~ (flip (-) (size / 2))) ∘ _orig'po

lu'orig ∷ Lin d ⇒ Reqt d → LU d → Orig d
lu'orig (Reqt (Di size)) = Orig ∘ (& po'v %~ (+ (size / 2))) ∘ _lu'po

reqt'axisMajor'add'max ∷ Lin d ⇒ Axes → Reqt d → Reqt d → Reqt d
reqt'axisMajor'add'max axes = Reqt .: (di'axisMajor'add'max axes `on` _reqt'di)


-- * TODO:
-- - alignment as parameter, instead of hard-coded N/W edge alignment
-- - switch to centre-based origin
--
orig'beside ∷ Lin d ⇒ Orient Card → Orig d → Reqt d → Reqt d → Orig d
orig'beside ON o r _t = o & orig'v._y %~ ((-)(r^.reqt'v._y))
orig'beside OS o _r t = o & orig'v._y %~ ((+)(t^.reqt'v._y))
orig'beside OW o r _t = o & orig'v._x %~ ((-)(r^.reqt'v._x))
orig'beside OE o _r t = o & orig'v._x %~ ((+)(t^.reqt'v._x))

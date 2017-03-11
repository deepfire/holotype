{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# LANGUAGE DataKinds, KindSignatures, TypeApplications, TypeInType #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, InstanceSigs, MultiParamTypeClasses, NoMonomorphismRestriction, RankNTypes, TypeSynonymInstances, UndecidableInstances #-}
{-# LANGUAGE GADTs, TypeFamilies, TypeFamilyDependencies #-}
{-# LANGUAGE BangPatterns, MultiWayIf, RecordWildCards, StandaloneDeriving, TypeOperators #-}
{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveGeneric, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
module Flatland where

-- Basis
import           Prelude.Unicode

-- Type-level
import           GHC.TypeLits

-- General types
import qualified Alib                              as M
import           GHC.Stack
import           Control.Applicative
import           Control.Lens
import           Control.Lens.TH
import           Control.Monad.Random
import           Control.Monad.State
import           Data.Complex
import           Data.Function
import           Data.Lub
import           Data.Glb
import           Data.Map (Map)
import qualified Data.Map                          as Map
import           Data.Maybe
import           Data.Profunctor
import           Data.Semigroup
import           Data.MeasuredMonoid
import           Data.MonoTraversable
import           Data.String                              (IsString)

-- Algebra
import           Linear

-- Manually-bound Cairo
import qualified Graphics.Rendering.Cairo          as GRC
import qualified Graphics.Rendering.Cairo.Internal as GRCI

-- glib-introspection -based Pango
import qualified GI.Pango                          as GIP (unitsToDouble, unitsFromDouble)

-- Misc
import           Text.Printf                              (printf)
import           Text.Show.Pretty                         (ppShow)

-- System
import           System.Random
import           Reflex.Random

-- Dirty stuff
import qualified Foreign                           as F
import qualified System.IO.Unsafe                  as UN


-- * Elsewhere
instance Ord a ⇒ HasLub (V2 a) where lub = liftA2 max
instance Ord a ⇒ HasGlb (V2 a) where glb = liftA2 min

goldenRatio ∷ Double
goldenRatio = 1.61803398875

(.:) ∷ ∀ a f g b. (b → a) → (f → g → b) → f → g → a
(.:) = M.o

instance Random a ⇒ Random (Complex a) where
  randomR (r:+i, r':+i') = runState $ liftA2 (:+) (state $ randomR (r,r')) (state $ randomR (i,i'))
  random                 = runState $ liftA2 (:+) (state $ random)         (state $ random)


-- * Dimensional density.
newtype PΠ = PΠ { pπVal ∷ Double } deriving (Num, Show)
pπ ∷ PΠ
pπ = 72

newtype DΠ = DΠ { fromDΠ ∷ Double } deriving (Num, Show)

-- $Note [Pango resolution & unit conversion]
--  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

data Unit = PU | PUI | Pt

type instance Element (Size PU)  = Double
type instance Element (Size PUI) = F.Int32
type instance Element (Size Pt)  = F.Int32

data Size (u ∷ Unit) where
  PUs  ∷ { fromPU  ∷ !(Element (Size PU))  } → Size PU  -- ^ Pango size, in device units
  PUIs ∷ { fromPUI ∷ !(Element (Size PUI)) } → Size PUI -- ^ Pango size, in device units, scaled by PANGO_SCALE
  Pts  ∷ { fromPt  ∷ !(Element (Size Pt))  } → Size Pt  -- ^ Pango size, in points (at 72ppi--see PΠ above--rate), device-agnostic
type family SizeType a = r | r → a where
  SizeType (Size PU)  = PU
  SizeType (Size PUI) = PUI
  SizeType (Size Pt)  = Pt

-- <Boilerplate>
deriving instance Eq   (Size u)
deriving instance Show (Size u)
instance Fractional (Size PU) where
  fromRational x = PUs $ fromRational x
  recip          = omap recip
instance MonoFunctor (Size PU)  where omap f (PUs x)  = PUs (f x)
instance MonoFunctor (Size PUI) where omap f (PUIs x) = PUIs (f x)
instance MonoFunctor (Size Pt)  where omap f (Pts x)  = Pts (f x)
instance Ord (Size PU)  where PUs  l <= PUs  r = l <= r
instance Ord (Size PUI) where PUIs l <= PUIs r = l <= r
instance Ord (Size Pt)  where Pts  l <= Pts  r = l <= r
instance Num (Size PU)  where fromInteger = PUs  ∘ fromIntegral; PUs  x + PUs  y = PUs  $ x + y; PUs  x * PUs  y = PUs  $ x * y; abs = omap abs; signum = omap signum; negate = omap negate
instance Num (Size PUI) where fromInteger = PUIs ∘ fromIntegral; PUIs x + PUIs y = PUIs $ x + y; PUIs x * PUIs y = PUIs $ x * y; abs = omap abs; signum = omap signum; negate = omap negate
instance Num (Size Pt)  where fromInteger = Pts  ∘ fromIntegral; Pts  x + Pts  y = Pts  $ x + y; Pts  x * Pts  y = Pts  $ x * y; abs = omap abs; signum = omap signum; negate = omap negate

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
  fromSz _       x@(PUIs pis) = PUs $ UN.unsafePerformIO ∘ GIP.unitsToDouble   $ pis -- fromIntegral pis / fromIntegral GIP.SCALE
  fromSz (DΠ dπ) x@(Pts pts)  = PUs $ (fromIntegral pts) ⋅ dπ / pπVal pπ
instance Sizely (Size PUI) where
  fromSz _       x@(PUIs _)   = x
  fromSz _       x@(PUs pus)  = PUIs $ UN.unsafePerformIO ∘ GIP.unitsFromDouble $ pus -- floor $ pus ⋅ fromIntegral GIP.SCALE
  fromSz (DΠ dπ) x@(Pts pts)  = PUIs $ UN.unsafePerformIO ∘ GIP.unitsFromDouble $ fromIntegral pts ⋅ dπ / pπVal pπ
instance Sizely (Size Pt) where
  fromSz _       x@(Pts _)    = x
  fromSz (DΠ dπ) x@(PUs pus)  = Pts $ floor $                                                                 pus ⋅ pπVal pπ / dπ
  fromSz (DΠ dπ) x@(PUIs pis) = Pts $ floor $ UN.unsafePerformIO ∘ GIP.unitsToDouble ∘ floor $ (fromIntegral pis) ⋅ pπVal pπ / dπ
-- instance Sizely (Size u) where
--   fromSz dπ x = fromSz dπ x
-- not sure if this even has any added value..
-- class (Functor a, Sizely b) ⇒ Sizeable a b where
--   fromSzable ∷ Sizeable a (Size c) ⇒ DΠ → a (Size c) → a b
--   fromSzable dπ = fmap (fromSz dπ)


-- * Specialized linear dimension classification:

newtype R   a = R   { _rV  ∷ a } deriving (Eq, Fractional, Functor, Num) -- ^ Radius
newtype An  a = An  { _anV ∷ a } deriving (Eq, Fractional, Functor, Num) -- ^ Angle
newtype Th  a = Th  { _thV ∷ a } deriving (Eq, Fractional, Functor, Num) -- ^ Thickness
newtype He  a = He  { _heV ∷ a } deriving (Eq, Fractional, Functor, Num) -- ^ Height
newtype Wi  a = Wi  { _wiV ∷ a } deriving (Eq, Fractional, Functor, Num) -- ^ Width
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
newtype  Di a =  Di { _diV   ∷ V2 a }      deriving (Additive, Applicative, Eq, Fractional, Functor, Num) -- ^ Dimensions
newtype  Po a =  Po { _poV   ∷ V2 a }      deriving (Additive, Applicative, Eq, Fractional, Functor, Num) -- ^ Coordinates, Descartes
newtype RPo a = RPo { _rpoV  ∷ Complex a } deriving (Additive, Applicative, Eq, Fractional, Functor, Num) -- ^ Coordinates, polar
newtype SDi a = SDi { _sdiV  ∷ V4 a }      deriving                        (Eq, Fractional, Functor, Num) -- ^ Side-wise dimensions: N, E, S, W

-------- <boilerplate>
deriving instance Show a ⇒ Show  (Di a); deriving instance Show a ⇒ Show  (Po a); deriving instance Show a ⇒ Show  (RPo a); deriving instance Show a ⇒ Show (SDi a)
deriving instance (Ord a, HasLub a) ⇒ HasLub (Di a)
deriving instance (Ord a, HasGlb a) ⇒ HasGlb (Di a)
deriving instance (Ord a, HasLub a) ⇒ HasLub (Po a)
deriving instance (Ord a, HasGlb a) ⇒ HasGlb (Po a)
newtype An2 a = An2 { _an2V ∷ V2 a } deriving (Eq, Functor) -- ^ Unordered pair of angles
newtype Co  a = Co  { _coV  ∷ V4 a } deriving (Eq, Functor) -- ^ Color
deriving instance Show a ⇒ Show (An2 a)
deriving instance Show a ⇒ Show (Co a)
deriving instance Random a ⇒ Random (Di a)
deriving instance Random a ⇒ Random (Po a)
deriving instance Random a ⇒ Random (RPo a)
deriving instance Random a ⇒ Random (SDi a)
deriving instance Random a ⇒ Random (Co a)
deriving instance Random a ⇒ Random (An2 a)

-- instance Applicative RPo where pure x = RPo (R x, An x); RPo (R fr, An fan) <*> RPo (R r, An an) = RPo (R $ fr r, An $ fan an)
-- instance Additive    RPo where zero = RPo (zero, zero)
-- instance Num a ⇒ Num (RPo a) where
--   (+) = liftA2 (+)

makeLenses ''Di
makeLenses ''Po
makeLenses ''RPo
makeLenses ''SDi
makeLenses ''An2
makeLenses ''Co
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

po2rpo ∷ Po a → RPo a
po2rpo (Po (V2 x y))  = RPo $ x :+ y
rpo2po ∷ RPo a → Po a
rpo2po (RPo (x :+ y)) = Po $ V2 x y

rotateRPo ∷ RealFloat a ⇒ An a → RPo a → RPo a
rotateRPo (An a) (RPo c) = RPo $ mkPolar (magnitude c) (phase c + a)

poBy ∷ Num a ⇒ Po a → V2 a → Po a
poBy po = Po ∘ (^+^ _poV po)
poByDi ∷ Num a ⇒ Po a → Di a → Po a
poByDi po = poBy po ∘ _diV

poSub ∷ Num a ⇒ Po a → Po a → Di a
poSub (Po v) (Po v') = Di $ v ^-^ v'

-- | Orientation: _ north-west, clockwise to west.
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

vROri ∷ Num a ⇒ R a → Orient b → (V2 a, V2 a)
vROri (R r) o | ON  ← o = (z, n) | ONE ← o = (e, n)
              | OE  ← o = (e, z) | OSE ← o = (e, s)
              | OS  ← o = (z, s) | OSW ← o = (w, s)
              | OW  ← o = (w, z) | ONW ← o = (w, n)
  where n = V2  0(-r)
        s = V2  0  r
        e = V2  r  0
        w = V2(-r) 0
        z = V2  0  0

oriCenterRChordCW ∷ Num a ⇒ Orient Corn → Po a → R a → (Po a, Po a)
oriCenterRChordCW o c r
  | ONE ← o = (oy, ox)
  | OSE ← o = (ox, oy)
  | OSW ← o = (oy, ox)
  | ONW ← o = (ox, oy)
  where (vx, vy) = vROri r o
        (ox, oy) = (poBy c vx, poBy c vy)

goldXdi ∷ RealFrac a ⇒ Wi a → Di a
goldXdi (Wi x) = Di $ V2  x               (x / realToFrac goldenRatio)
goldYdi ∷ RealFrac a ⇒ He a → Di a
goldYdi (He y) = Di $ V2 (y / realToFrac goldenRatio) y

poRectOppo ∷ Po a → Po a → (Po a, Po a)
poRectOppo !(Po (V2 c00 c01)) !(Po (V2 c10 c11))
  = (Po (V2 c00 c11), Po (V2 c10 c01))

thLineSet ∷ Th Double → GRCI.Render ()
thLineSet !(Th th)
  = GRC.setLineWidth th

-- | Description of rounded rectangle features -- sides and corners.
data RoundRectFeature dk a where
  RRCorn ∷
    { rrcOri ∷ !(Orient Corn)
    , rrcCt ∷ !(Po a)
    , rrcAn ∷ !(An2 a)
    , rrcR  ∷ !(R a)
    } → RoundRectFeature Corn a
  RRSide ∷
    { rrsOri ∷ !(Orient Card)
    , rrsFr ∷ !(Po a)
    , rrsTo ∷ !(Po a)
    } → RoundRectFeature Card a
deriving instance Show a ⇒ Show (RoundRectFeature dk a)

data WRoundRectFeature a where
  WRR ∷ RoundRectFeature dk a → WRoundRectFeature a

poArc ∷ Po Double → Double → An2 Double → GRCI.Render ()
poArc !(Po (V2 x y)) !r !(An2 (V2 angs ange))
  = GRC.arc x y r angs ange
  where degrees = pi/180


-- * Colors
coOpaq ∷ Num a ⇒ a → a → a → Co a
coOpaq r g b = Co $ V4 r g b 1

coGray ∷ a → a → Co a
coGray x a = Co $ V4 x x x a

coMult ∷ Num a ⇒ a → Co a → Co a
coMult x (Co (V4 r g b a)) = Co $ V4 (r*x) (g*x) (b*x) a

coSetSourceColor ∷ Co Double → GRCI.Render ()
coSetSourceColor !(Co (V4 r g b a))
  = GRC.setSourceRGBA r g b a

coGradientSet ∷ Co Double → Co Double → GRC.Pattern → IO ()
coGradientSet !(Co (V4 rs gs bs as)) !(Co (V4 re ge be ae)) pat = do
  GRCI.patternAddColorStopRGBA pat 0 rs gs bs as
  GRCI.patternAddColorStopRGBA pat 1 re ge be ae

coPatternGradLinear ∷ Po Double → Co Double → Po Double → Co Double → IO GRC.Pattern
coPatternGradLinear !(Po (V2 xs ys)) !sco !(Po (V2 xe ye)) !eco = do
  p ← GRCI.patternCreateLinear xs ys xe ye
  coGradientSet sco eco p
  pure p

coPatternGradRadial ∷ Po Double → Double → Co Double → Po Double → Double → Co Double → IO GRC.Pattern
coPatternGradRadial !(Po (V2 xi yi)) !ir !ico !(Po (V2 xo yo)) !or !oco = do
  p ← GRCI.patternCreateRadial xi yi ir xo yo or
  coGradientSet ico oco p
  pure p



data Kind = Area | Wrap
-- | A 'Wrap' is a rectangular "donut", wrapping something inside it.
--   It effectively partitions space into:
--   - the __/wrap area/__, around the /wrapped area/ -- this is what 'Wrap' corresponds to,
--   - the __/wrapped area/__, that, which is inside the 'Wrap' itself.
data S (kind ∷ Kind) (pinned ∷ Bool) a where
  Farea ∷ -- ^ A non-positioned, free area.
    { _aD    ∷ !(Di a)
    } → S Area False a
  Parea ∷ -- ^ A positioned, pinned area.
    { _paD   ∷ !(Di a)
    , _paNWp ∷ !(Po a)
    } → S Area True a
  Fwrap ∷ -- ^ The non-positioned, dimensional-only, free wrap.
    { _wNWd  ∷ !(Di a) -- ^ The combined offsets _ the left and top sides.
    , _wSEd  ∷ !(Di a) -- ^ The combined offsets _ the right and bottom sides.
    } → S Wrap False a
  Pwrap ∷ -- ^ The pinned variant -- enriched with a position.
    { _pwNWd ∷ !(Di a) -- ^ ..same as above.
    , _pwSEd ∷ !(Di a) -- ^ ..same as above.
    , _pwNWp ∷ !(Po a) -- ^ Coordinates of the top-leftmost pixel of the wrap area.
    , _pwSEp ∷ !(Po a) -- ^ Coordinates of the bottom-rightmost pixel of the wrap area.
    } → S Wrap True a
deriving instance Show u ⇒ Show (S k p u)
aD :: Lens' (S Area p a) (Di a)
aD f Farea{..} = Farea <$> f _aD
aD f Parea{..} = flip Parea _paNWp <$> f _paD
instance Functor (S Area False) where fmap f (Farea x)       = Farea (fmap f x)
instance Functor (S Area True)  where fmap f (Parea x y)     = Parea (fmap f x) (fmap f y)
instance Functor (S Wrap False) where fmap f (Fwrap x y)     = Fwrap (fmap f x) (fmap f y)
instance Functor (S Wrap True)  where fmap f (Pwrap x y z w) = Pwrap (fmap f x) (fmap f y) (fmap f z) (fmap f w)
instance Applicative (S Area False) where
  pure x = Farea $ pure x
  Farea x <*> Farea y = Farea $ x <*> y
instance Applicative (S Area True)  where
  pure x = Parea (pure x) (pure x)
  Parea x x' <*> Parea y y' = Parea (x <*> y) (x' <*> y')
instance (HasLub a, Ord a) ⇒ HasLub (S Area False a) where lub = Farea .: on lub _aD
instance (HasGlb a, Ord a) ⇒ HasGlb (S Area False a) where glb = Farea .: on glb _aD
instance (HasLub a, HasGlb a, Num a, Ord a) ⇒ HasGlb (S Area True a) where
  Parea d p `glb` Parea d' p' = Parea (poSub lu gl) gl
    where gl = glb p p'
          lu = lub p p'


-- * Constructors
class Num a ⇒ AspectS a k where aspect ∷ Di a → S k False a
class Num a ⇒ SymmS   a k where symm   ∷ Th a → S k False a
class Num a ⇒ GoldXS  a k where goldX  ∷ Wi a → S k False a
class Num a ⇒ GoldYS  a k where goldY  ∷ He a → S k False a
instance Num a ⇒      AspectS a Area where aspect =      Farea
instance Num a ⇒      AspectS a Wrap where aspect = join Fwrap
instance Num a ⇒      SymmS   a Area where symm   =      Farea ∘ Di ∘ join V2 ∘ _thV
instance Num a ⇒      SymmS   a Wrap where symm   = join Fwrap ∘ Di ∘ join V2 ∘ _thV
instance RealFrac a ⇒ GoldXS  a Area where goldX  =      Farea ∘ goldXdi
instance RealFrac a ⇒ GoldXS  a Wrap where goldX  = join Fwrap ∘ goldXdi
instance RealFrac a ⇒ GoldYS  a Area where goldY  =      Farea ∘ goldYdi
instance RealFrac a ⇒ GoldYS  a Wrap where goldY  = join Fwrap ∘ goldYdi

instance (Num a, Random a) ⇒ Random (S Area True a) where
  random = runState $ Parea <$> (state random) <*> (state random)
  randomR (Parea di nw, Parea maxsz _) =
    let se = poByDi nw $ di ^-^ maxsz
    in runState $ liftA2 Parea (state $ randomR (zero, maxsz)) (state $ randomR (nw, se))


-- * Projections
--
paSEp ∷ Num a ⇒ S Area True a → Po a
paSEp (Parea di po) = poByDi po di

center ∷ Fractional a ⇒ S k True a → Po a
center (Parea d p)                 = poByDi p (d ^/ 2)
center (Pwrap _ _ (Po nw) (Po se)) = Po $ (se ^+^ nw) ^/ 2

dim ∷ Num a ⇒ S k p a → Di a
dim Farea{..} = _aD
dim Parea{..} = _paD
dim Fwrap{..} = _wNWd ^+^ _wSEd
dim Pwrap{..} = Di ∘ _poV $ _pwSEp ^-^ _pwNWp

-- | Narrowing into a positioned 'S Wrap'.
stepIntoNW, stepIntoSE ∷ Num a ⇒ S Wrap True a → Po a
stepIntoNW (Pwrap (Di pwNWd) _ (Po pwNWp) _) = Po $ pwNWp ^+^ pwNWd
stepIntoSE (Pwrap _ (Di pwSEd) _ (Po pwSEp)) = Po $ pwSEp ^-^ pwSEd

stepInto ∷ Num a ⇒ S Wrap True a → (Po a, Po a)
stepInto pw = (stepIntoNW pw, stepIntoNW pw)

-- | Narrowing into a positioned 'Wrap', halfway.
stepIntoNW'2, stepIntoSE'2 ∷ Fractional a ⇒ S Wrap True a → Po a
stepIntoNW'2 (Pwrap (Di pwNWd) _ (Po pwNWp) _) = Po $ pwNWp ^+^ pwNWd ⋅ 0.5
stepIntoSE'2 (Pwrap _ (Di pwSEd) _ (Po pwSEp)) = Po $ pwSEp ^-^ pwSEd ⋅ 0.5

stepInto'2 ∷                 Fractional a ⇒ S Wrap True a → (Po a, Po a)
stepInto'2 pw = (stepIntoNW'2 pw, stepIntoSE'2 pw)

-- | 'l', 't', 'r', 'b' -- sidewise dimensions.
l, r ∷ S Wrap p a → Wi a
t, b ∷ S Wrap p a → He a
l Fwrap{..} = Wi ∘ (view _x) $ _diV _wNWd; l Pwrap{..} = Wi ∘ (view _x) $ _diV _pwNWd
t Fwrap{..} = He ∘ (view _y) $ _diV _wNWd; t Pwrap{..} = He ∘ (view _y) $ _diV _pwNWd
r Fwrap{..} = Wi ∘ (view _x) $ _diV _wSEd; r Pwrap{..} = Wi ∘ (view _x) $ _diV _pwSEd
b Fwrap{..} = He ∘ (view _y) $ _diV _wSEd; b Pwrap{..} = He ∘ (view _y) $ _diV _pwSEd


-- * Combinators
--
area ∷ Num a ⇒ S k True a → S Area True a
area a@Parea{..} = a
area (Pwrap _ _ (Po nw) (Po se)) =
  Parea (Di $ se ^-^ nw) (Po nw)

areaSubtract ∷ Num a ⇒ S Area False a → S Area False a → S Area False a
Farea l `areaSubtract` Farea r = Farea $ l ^-^ r


-- * General transformations
--
narrowByTh ∷ Num a ⇒ Th a → S Area p a → S Area p a
narrowByTh (Th d) (Farea (Di a))        = Farea (Di $ a ^-^ (V2 d d) ^* 2)
narrowByTh (Th d) (Parea (Di a) (Po p)) = Parea (Di $ a ^-^ (V2 d d) ^* 2) (Po $ p ^+^ (V2 d d))


-- * Pinning
-- | Make pwNWp and pwSEp the top-leftmost and bottom-rightmost pixels of the 'Wrap'.
--   Warning:  no check on whether the coordinates are compatible with the dimensions is performed.
pinArea ∷ S Area False a → Po a → S Area True a
pinArea Farea{..} _paNWp = Parea{..}
  where _paD = _aD
pinWrap ∷ S Wrap False a → Po a → Po a → S Wrap True a
pinWrap Fwrap{..} _pwNWp _pwSEp = Pwrap{..}
  where _pwNWd = _wNWd
        _pwSEd = _wSEd


-- * Drawing
-- | Wrap rendering as a rounded rectangle.
--   XXX: see if `Linear.V2.perp` can help with that.
wrapRoundedRectFeatures ∷ Floating a ⇒ S Wrap True a → R a → Th a → [WRoundRectFeature a]
wrapRoundedRectFeatures pw@Pwrap{..} rr@(R r) th =
  let pa@(Parea (Di (V2 wi he)) (Po (V2 w n))) = narrowByTh ((/2) <$> th) $ area pw
      V2 e s = _poV $ paSEp pa
      !degrees         = pi/180
  in [WRR $ RRSide ON  (po (w + r)  n)      (po  (e - r) n)
     ,WRR $ RRCorn ONE (po (e - r) (n + r)) (an2 (-90 ⋅ degrees)   (0 ⋅ degrees)) rr
     ,WRR $ RRSide OE  (po  e      (n + r)) (po   e     (s - r))
     ,WRR $ RRCorn OSE (po (e - r) (s - r)) (an2   (0 ⋅ degrees)  (90 ⋅ degrees)) rr
     ,WRR $ RRSide OS  (po (w + r)  s)      (po  (e - r) s)
     ,WRR $ RRCorn OSW (po (w + r) (s - r)) (an2  (90 ⋅ degrees) (180 ⋅ degrees)) rr
     ,WRR $ RRSide OW  (po  w      (n + r)) (po   w     (s - r))
     ,WRR $ RRCorn ONW (po (w + r) (n + r)) (an2 (180 ⋅ degrees) (270 ⋅ degrees)) rr]

executeFeature ∷ Maybe (Co Double) → Maybe (Co Double) → WRoundRectFeature Double → GRC.Render ()
executeFeature !cStart !cEnd !(WRR (RRSide _ (Po (V2 sx sy)) (Po (V2 ex ey)))) = do
  -- pat ← UN.unsafeInterleaveIO $ coPatternGradLinear rrsFr cStart rrsTo cEnd
  if | cStart ≢ cEnd    → error "Not implemented: sidewise gradients."
     | Nothing ← cStart → pure ()
     | Just c  ← cStart → coSetSourceColor c
  GRC.moveTo sx sy
  GRC.lineTo ex ey
executeFeature !cStart !cEnd !(WRR (RRCorn o c@(Po (V2 cx cy)) (An2 (V2 sa ea)) r)) = do
  let (cs, ce) = oriCenterRChordCW o c r
  if | cStart ≢ cEnd    → GRC.setSource =<< (GRC.liftIO $ coPatternGradLinear cs (fromJust cStart) ce (fromJust cEnd))
     | Nothing ← cStart → pure ()
     | Just c  ← cStart → coSetSourceColor c
  GRC.arc cx cy (_rV r) sa ea

poNWSERectArcCentersCW ∷ Num a ⇒ Po a → Po a → R a → (Po a, Po a, Po a, Po a)
poNWSERectArcCentersCW !lt@(Po (V2 ltx lty)) !rb@(Po (V2 rbx rby)) (R r) =
  let (Po (V2 lbx lby), Po (V2 rtx rty)) = poRectOppo lt rb
  in (Po (V2 (ltx + r) (lty + r))
     ,Po (V2 (rtx - r) (rty + r))
     ,Po (V2 (rbx - r) (rby - r))
     ,Po (V2 (lbx + r) (lby - r)))

aRectAnglesNWCW ∷ (Fractional a, Floating a) ⇒ (An2 a, An2 a, An2 a, An2 a)
aRectAnglesNWCW
  = (An2 $ V2 (180 ⋅ degrees) (270 ⋅ degrees)
    ,An2 $ V2 (-90 ⋅ degrees)   (0 ⋅ degrees)
    ,An2 $ V2   (0 ⋅ degrees)  (90 ⋅ degrees)
    ,An2 $ V2  (90 ⋅ degrees) (180 ⋅ degrees))
  where !degrees = pi/180

aRectAngles_MidSWCW ∷ (Fractional a, Floating a) ⇒ (An2 a, An2 a, An2 a, An2 a, An2 a, An2 a)
aRectAngles_MidSWCW
  = (An2 $ V2 (135 ⋅ degrees) (180 ⋅ degrees)
    ,An2 $ V2 (180 ⋅ degrees) (270 ⋅ degrees)
    ,An2 $ V2 (-90 ⋅ degrees) (-45 ⋅ degrees)
    ,An2 $ V2 (-45 ⋅ degrees)   (0 ⋅ degrees)
    ,An2 $ V2   (0 ⋅ degrees)  (90 ⋅ degrees)
    ,An2 $ V2  (90 ⋅ degrees) (135 ⋅ degrees))
  where !degrees = pi/180



-- | Space partitioning
data Space (pinned ∷ Bool) a (n ∷ Nat) where
  End   ∷ Num a ⇒                Space p a 0
  Sarea ∷ Num a ⇒
    { sArea  ∷ !(S Area p a) } → Space p a 1
  Spc   ∷
    (Num a, n ~ (m + 1)) ⇒
    { sWrap  ∷ !(S Wrap p a)
    , sInner ∷  Space p a m }  → Space p a n

deriving instance Show a ⇒  Show (Space p a n)

instance (Num a, Show a) ⇒ MeasuredMonoid (Space p a) where
  mmempty    = End
  mmappend     End        End       = End
  mmappend     End      x@Sarea{..} = x
  mmappend     End      x@Spc{..}   = x
  mmappend  x@Sarea{..}   End       = x
  mmappend  x@Sarea{..}   tail      = error $ printf "Sarea `mmappend` non-End is ⊥: %s to %s" (show x) (show tail)
  mmappend  x@Spc{..}     End       = x
  mmappend tl@(Spc pl nl) tail      = Spc pl $ mmappend nl tail

type SubArea a = Space True a 2


-- * Constructors
makeArea ∷ Fractional a ⇒ Di a → Space False a 1
makeArea = Sarea ∘ Farea

-- XXX/natnormalise:
-- Could not deduce: CmpNat d 0 ~ 'GT arising _ a use of ‘Spc’
--      _ the context: (Num a, d ~ (m + 1), CmpNat m 0 ~ 'GT)
--        bound by a pattern with constructor:
--                   Spc :: forall (p :: Bool) a (n :: Nat) (m :: Nat).
--                          (Num a, n ~ (m + 1), CmpNat m 0 ~ 'GT) =>
--                          S 'Wrap p a -> Space p a m -> Space p a n,
--                 in an equation for ‘growSymm’
--        at /home/desktop/src/mood/.stack-work/intero/intero29544_wH.hs:567:15-21
--    • In the expression: Spc (symm $ Th δ) sp
--      In an equation for ‘growSymm’:
--          growSymm δ sp@(Spc {..}) = Spc (symm $ Th δ) sp
growSymm ∷ (Num a) ⇒ Th a → Space False a d → Space False a (d + 1)
growSymm δ sp = Spc (symm $ δ) sp
growGX ∷ (RealFrac a) ⇒ Wi a → Space False a n → Space False a (n + 1)
growGX   d sp = Spc (goldX $ d) sp
growGY ∷ (RealFrac a) ⇒ He a → Space False a n → Space False a (n + 1)
growGY   d sp = Spc (goldY $ d) sp


-- * Transformations
-- | XXX/Lensify: update the wrap of the innermost space
mapSpaceArea ∷ (S Area p u → S Area p u) → Space p u d → Space p u d
mapSpaceArea f s | End      ← s = End
                 | Sarea sa ← s = Sarea (f sa)
                 | Spc w is ← s = Spc w $ mapSpaceArea f is
--- XXX: destroys the depth information
-- sMapInnermost ∷ (Space p d a → Space p e a) → Space p f a → Space p g a
-- sMapInnermost f s
--   | Spc w End ← s = f s
--   | Spc w is  ← s = Spc w $ sMapInnermost f is


-- * Projections
-- | Compute the total allocation for a 'Space'.
--   Complexity: O(depth) for un-pinned, O(1) for pinned.
spaceDim ∷ Space p u d → Di u
spaceDim  End                           = zero
spaceDim (Sarea a)                      = dim a
spaceDim (Spc w@(Fwrap  _ _)    sInner) = dim w ^+^ spaceDim sInner
spaceDim (Spc w@(Pwrap _ _ _ _) sInner) = dim w


-- * Pinning
-- | Compute the SE point for an un-pinned 'Space', given its NW point.
--   Complexity: O(depth).
sSE  ∷ Space False a d → Po a → Po a
sSE  s@Spc{..} lt = Po $ _poV lt ^+^ _diV (spaceDim s) --  ^-^ V2 1 1

-- | Pin space to the @lt
sPin ∷ Num a ⇒ Po a → Space False a n → Space True a n
sPin lt space = loop space zero rb
  where rb = sSE space lt
        loop ∷ Space False a n → Po a → Po a → Space True a n
        loop  End                     _  _ = End
        loop (Sarea Farea{..})       lt  _ =
          Sarea (Parea { _paD   = _aD,                   _paNWp = lt })
        loop (Spc Fwrap{..} swInner) lt rb =
          Spc   (Pwrap { _pwNWd = _wNWd, _pwSEd = _wSEd, _pwNWp = lt, _pwSEp = rb })
          $   loop swInner
              (lt ^+^ (Po ∘ _diV) _wNWd)
              (rb ^-^ (Po ∘ _diV) _wSEd)

-- XXX: not sure if needed
-- sCutOutsideS2 ∷ Di a → Space False a n → Space False a (n + 1)
-- sCutOutsideS2 cut s@Spc{..} = Spc (Fwrap cut cut)                $ s { sWrap = omap (^-^ _diV cut) sWrap }
-- sCutInsideS2  ∷ Fractional a ⇒ Di a → Space False a n → Space False a (n+1)
-- sCutInsideS2  cut s@Spc{..} = Spc (omap (^-^ _diV cut) sWrap) $ s { sWrap = Fwrap cut cut }

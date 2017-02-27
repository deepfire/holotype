{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# LANGUAGE DataKinds, KindSignatures #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RankNTypes #-}
{-# LANGUAGE GADTs, TypeFamilies, TypeFamilyDependencies #-}
{-# LANGUAGE BangPatterns, MultiWayIf, RecordWildCards, StandaloneDeriving, TypeOperators #-}
{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UnicodeSyntax #-}
module Flatland where

-- Basis
import           Prelude.Unicode

-- Type-level
import           GHC.TypeLits

-- General types
import           Control.Lens
import           Data.Map (Map)
import qualified Data.Map                          as Map
import           Data.Maybe
import           Data.Semigroup
import           Data.MeasuredMonoid
import           Data.MonoTraversable
import           Data.String                              (IsString)
import qualified Data.Text                         as DT

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

-- Dirty stuff
import qualified Foreign                           as F
import qualified System.IO.Unsafe                  as UN


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

data KUnit = PU | PUI | Pt

type instance Element (Size PU)  = Double
type instance Element (Size PUI) = F.Int32
type instance Element (Size Pt)  = F.Int32

data Size (u ∷ KUnit) where
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

newtype R   a = R   { fromR   ∷     a } deriving (Eq, Functor, Num)             -- ^ Radius
newtype Th  a = Th  { fromTh  ∷     a } deriving (Eq, Fractional, Functor, Num) -- ^ Thickness
newtype He  a = He  { fromHe  ∷     a } deriving (Eq, Functor, Num)             -- ^ Height
newtype Wi  a = Wi  { fromWi  ∷     a } deriving (Eq, Functor, Num)             -- ^ Width
deriving instance Show a ⇒ Show (R  a)
deriving instance Show a ⇒ Show (Th a)
deriving instance Show a ⇒ Show (He a)
deriving instance Show a ⇒ Show (Wi a)
--instance Sizeable R (Size u); instance Sizeable Th (Size u); instance Sizeable He (Size u); instance Sizeable Wi (Size u)


-- * Pairing dimensions:
--
-- XXX: HUGE NOTE: V2/Co/Wrap mass of code below is likely silliness,
--      easily replaced with a couple of short lens operators.
--      Nevertheless, it is what it is -- because of blissful lack of education.
--      Typical.
--
goldenRatio ∷ Double
goldenRatio = 1.61803398875

v2symm ∷ a → V2 a
v2symm x = V2 x x

v2negp ∷ (Num a, Ord a) ⇒ V2 a → Bool
v2negp (V2 d0 d1) = d0 < 0 || d1 < 0

newtype  Di a =  Di { fromDi  ∷ V2 a } deriving (Additive, Applicative, Eq, Functor) -- ^ Dimensions
newtype  Po a =  Po { fromPo  ∷ V2 a } deriving (Additive, Applicative, Eq, Functor) -- ^ Coordinates
newtype SDi a = SDi { fromSDi ∷ V4 a } deriving                        (Eq, Functor) -- ^ Side-wise dimensions: N, E, S, W
newtype SPo a = SPo { fromSpo ∷ V4 a } deriving                        (Eq, Functor) -- ^ Side-wise positions:  N, E, S, W
-- deriving instance Additive Di
deriving instance Show a ⇒ Show  (Di a); deriving instance Show a ⇒ Show  (Po a); deriving instance Show a ⇒ Show (SDi a); deriving instance Show a ⇒ Show (SPo a)
--instance           Sizeable Di (Size u); instance           Sizeable Po (Size u); instance          Sizeable SDi (Size u); instance          Sizeable SPo (Size u)

newtype An  a = An  { fromAn  ∷ V2 a } deriving (Eq, Functor) -- ^ Unordered pair of angles
newtype Co  a = Co  { fromCo  ∷ V4 a } deriving (Eq, Functor) -- ^ Color
deriving instance Show a ⇒ Show (An a)
deriving instance Show a ⇒ Show (Co a)

di ∷ Wi a → He a  → Di a
di  (Wi x) (He y) = Di $ V2 x y
po ∷ a → a → Po a
po x y = Po $ V2 x y
an ∷ a → a → An a
an x y = An $ V2 x y
co ∷ a → a → a → a → Co a
co r g b a = Co $ V4 r g b a

off ∷ Num a ⇒ Po a → V2 a → Po a
off co = Po ∘ (+ fromPo co)

-- | Orientation: from north-west, clockwise to west.
data Orient = ONW | ON | ONE | OE | OSE | OS | OSW | OW
  deriving (Eq, Show)

vROri ∷ Num a ⇒ R a → Orient → (V2 a, V2 a)
vROri (R r) o | ON  ← o = (z, n) | ONE ← o = (e, n)
              | OE  ← o = (e, z) | OSE ← o = (e, s)
              | OS  ← o = (z, s) | OSW ← o = (w, s)
              | OW  ← o = (w, z) | ONW ← o = (w, n)
  where n = V2  0(-r)
        s = V2  0  r
        e = V2  r  0
        w = V2(-r) 0
        z = zero

oriCenterRChordCW ∷ Num a ⇒ Orient → Po a → R a → (Po a, Po a)
oriCenterRChordCW o c r
  | ONE ← o = (oy, ox)
  | OSE ← o = (ox, oy)
  | OSW ← o = (oy, ox)
  | ONW ← o = (ox, oy)
  where (vx, vy) = vROri r o
        (ox, oy) = (off c vx, off c vy)

di2goldX, di2goldY ∷ Double → Di Double
di2goldX x = Di $ V2  x               (x / goldenRatio)
di2goldY y = Di $ V2 (y / goldenRatio) y

poRectOppo ∷ Po a → Po a → (Po a, Po a)
poRectOppo !(Po (V2 c00 c01)) !(Po (V2 c10 c11))
  = (Po (V2 c00 c11), Po (V2 c10 c01))

spoNarrow ∷ Num a ⇒ Th a → SPo a → SPo a
spoNarrow (Th d) (SPo (V4 n e s w))
  = SPo $ V4 (n + d) (e - d) (s - d) (w + d)

thLineSet ∷ Th Double → GRCI.Render ()
thLineSet !(Th th)
  = GRC.setLineWidth th

-- | Description of rounded rectangle features -- sides and corners.
data RoundRectFeature a where
  RRCorn ∷
    { rrOri ∷ !Orient
    , rraCt ∷ !(Po a)
    , rraAn ∷ !(An a)
    , rraR  ∷ !(R a)
    } → RoundRectFeature a
  RRSide ∷
    { rrOri ∷ !Orient
    , rrsFr ∷ !(Po a)
    , rrsTo ∷ !(Po a)
    } → RoundRectFeature a
deriving instance Show a ⇒ Show (RoundRectFeature a)

poArc ∷ Po Double → Double → An Double → GRCI.Render ()
poArc !(Po (V2 x y)) !r !(An (V2 angs ange))
  = GRC.arc x y r angs ange
  where degrees = pi/180

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


-- | A 'Wrap' is a rectangular "donut", wrapping something inside it.
--   It effectively partitions space into:
--   - the __/wrap area/__, around the /wrapped area/ -- this is what 'Wrap' corresponds to,
--   - the __/wrapped area/__, that, which is inside the 'Wrap' itself.
data Wrap (pinned ∷ Bool) a where
  Wrap  ∷ Num a ⇒ -- ^ The non-positioned, dimensional-only wrap.
    { wNWd  ∷ !(Di a) -- ^ The combined offsets from the left and top sides.
    , wSEd  ∷ !(Di a) -- ^ The combined offsets from the right and bottom sides.
    } → Wrap False a
  PWrap ∷ Num a ⇒ -- ^ The /pinned/ variant -- enriched with a position.
    { pwNWd ∷ !(Di a) -- ^ ..same as above.
    , pwSEd ∷ !(Di a) -- ^ ..same as above.
    , pwNWp ∷ !(Po a) -- ^ Coordinates of the top-leftmost pixel of the wrap area.
    , pwSEp ∷ !(Po a) -- ^ Coordinates of the bottom-rightmost pixel of the wrap area.
    } → Wrap True a
deriving instance Show u ⇒ Show (Wrap p u)

wL, wR ∷ Wrap p u → Wi u
wT, wB ∷ Wrap p u → He u
wL  Wrap{..} = Wi ∘ (view _x) $ fromDi wNWd; wL PWrap{..} = Wi ∘ (view _x) $ fromDi pwNWd
wT  Wrap{..} = He ∘ (view _y) $ fromDi wNWd; wT PWrap{..} = He ∘ (view _y) $ fromDi pwNWd
wR  Wrap{..} = Wi ∘ (view _x) $ fromDi wSEd; wR PWrap{..} = Wi ∘ (view _x) $ fromDi pwSEd
wB  Wrap{..} = He ∘ (view _y) $ fromDi wSEd; wB PWrap{..} = He ∘ (view _y) $ fromDi pwSEd

pwPosition ∷ Wrap True a → SPo a
pwPosition (PWrap _ _ (Po (V2 nwx nwy)) (Po (V2 sex sey))) =
  SPo $ V4 nwy sex sey nwx

-- | Narrowing into a positioned 'Wrap'.
pwNWpi, pwSEpi ∷ Wrap True a → Po a
pwNWpi (PWrap (Di pwNWd) _ (Po pwNWp) _) = Po $ pwNWp ^+^ pwNWd
pwSEpi (PWrap _ (Di pwSEd) _ (Po pwSEp)) = Po $ pwSEp ^-^ pwSEd

pwPosIn        ∷ Wrap True a → (Po a, Po a)
pwPosIn pw = (pwNWpi pw, pwSEpi pw)

-- | Narrowing into a positioned 'Wrap', halfway.
pwNWpi'2, pwSEpi'2 ∷ Fractional a ⇒ Wrap True a → Po a
pwNWpi'2 (PWrap (Di pwNWd) _ (Po pwNWp) _) = Po $ pwNWp ^+^ pwNWd ⋅ 0.5
pwSEpi'2 (PWrap _ (Di pwSEd) _ (Po pwSEp)) = Po $ pwSEp ^-^ pwSEd ⋅ 0.5

pwPosIn'2          ∷ Fractional a ⇒ Wrap True a → (Po a, Po a)
pwPosIn'2 pw = (pwNWpi'2 pw, pwSEpi'2 pw)

--   Is there a useful meaning for a monoid?
-- instance (Num a, Monoid (V2 a)) ⇒ Monoid (Wrap a) where
--   mempty = Wrap mempty mempty
--   Wrap l0 l1 `mappend` Wrap r0 r1 = Wrap (mappend l0 r0) (mappend l1 r1)
-- instance forall a.Num a ⇒ Functor (Wrap a) where
--   fmap f Wrap{..} = Wrap (fmap f wLB) (fmap f wRT)

--- It's /clearly/ a profunctorial transform, but I don't care yet..
type instance Element (Wrap p a) = V2 a
instance MonoFunctor (Wrap p a) where
  omap f  Wrap{..} =  Wrap (f'di wNWd) (f'di wSEd)
    where f'di = Di ∘ f ∘ fromDi
  omap f PWrap{..} = PWrap (f'di pwNWd) (f'di pwSEd) (f'po pwNWp) (f'po pwSEp)
    where f'di = Di ∘ f ∘ fromDi
          f'po = Po ∘ f ∘ fromPo

wDim ∷ Wrap False a → Di a
wDim  Wrap{..} = wNWd ^+^ wSEd
pwDim ∷ Wrap True a → Di a
pwDim PWrap{..} = Di ∘ fromPo $ pwSEp ^-^ pwNWp

-- | Make pwNWp and pwSEp the top-leftmost and bottom-rightmost pixels of the 'Wrap'.
--   Warning:  no check on whether the coordinates are compatible with the dimensions is performed.
wPin ∷ Wrap False a → Po a → Po a → Wrap True a
wPin Wrap{..} pwNWp pwSEp = PWrap{..}
  where pwNWd = wNWd
        pwSEd = wSEd

-- | Smart constructors for 'Wrap'.
wArea ∷ Fractional a ⇒ Di a → Wrap False a
wArea dim = Wrap half half
  where half = dim ^/ 2.0

wSymm ∷ Num a ⇒ a → Wrap False a
wSymm d = Wrap (Di $ v2symm d) (Di $ v2symm d)

wGoldSX, wGoldSY ∷ Double → Wrap False Double
wGoldSX x = Wrap w w where w = di2goldX x
wGoldSY y = Wrap w w where w = di2goldY y

-- | Wrap rendering as a rounded rectangle.
wrapRoundedRectFeatures ∷ Floating a ⇒ Wrap True a → R a → Th a → [RoundRectFeature a]
wrapRoundedRectFeatures pw@PWrap{..} rr@(R r) th =
  let SPo (V4 n e s w) = spoNarrow ((/2) <$> th) $ pwPosition pw
      !degrees         = pi/180
  in [RRSide ON  (po (w + r)  n)      (po (e - r) n)
     ,RRCorn ONE (po (e - r) (n + r)) (an (-90 ⋅ degrees)   (0 ⋅ degrees)) rr
     ,RRSide OE  (po  e      (n + r)) (po  e     (s - r))
     ,RRCorn OSE (po (e - r) (s - r)) (an   (0 ⋅ degrees)  (90 ⋅ degrees)) rr
     ,RRSide OS  (po (w + r)  s)      (po (e - r) s)
     ,RRCorn OSW (po (w + r) (s - r)) (an  (90 ⋅ degrees) (180 ⋅ degrees)) rr
     ,RRSide OW  (po  w      (n + r)) (po  w     (s - r))
     ,RRCorn ONW (po (w + r) (n + r)) (an (180 ⋅ degrees) (270 ⋅ degrees)) rr]

executeFeature ∷ Maybe (Co Double) → Maybe (Co Double) → RoundRectFeature Double → GRC.Render ()
executeFeature !cStart !cEnd !(RRSide _ (Po (V2 sx sy)) (Po (V2 ex ey))) = do
  -- pat ← UN.unsafeInterleaveIO $ coPatternGradLinear rrsFr cStart rrsTo cEnd
  if | cStart ≢ cEnd    → error "Not implemented: sidewise gradients."
     | Nothing ← cStart → pure ()
     | Just c  ← cStart → coSetSourceColor c
  GRC.moveTo sx sy
  GRC.lineTo ex ey
executeFeature !cStart !cEnd !(RRCorn o c@(Po (V2 cx cy)) (An (V2 sa ea)) r) = do
  let (cs, ce) = oriCenterRChordCW o c r
  if | cStart ≢ cEnd    → GRC.setSource =<< (GRC.liftIO $ coPatternGradLinear cs (fromJust cStart) ce (fromJust cEnd))
     | Nothing ← cStart → pure ()
     | Just c  ← cStart → coSetSourceColor c
  GRC.arc cx cy (fromR r) sa ea

poNWSERectArcCentersCW ∷ Num a ⇒ Po a → Po a → R a → (Po a, Po a, Po a, Po a)
poNWSERectArcCentersCW !lt@(Po (V2 ltx lty)) !rb@(Po (V2 rbx rby)) (R r) =
  let (Po (V2 lbx lby), Po (V2 rtx rty)) = poRectOppo lt rb
  in (Po (V2 (ltx + r) (lty + r))
     ,Po (V2 (rtx - r) (rty + r))
     ,Po (V2 (rbx - r) (rby - r))
     ,Po (V2 (lbx + r) (lby - r)))

aRectAnglesNWCW ∷ (Fractional a, Floating a) ⇒ (An a, An a, An a, An a)
aRectAnglesNWCW
  = (An $ V2 (180 ⋅ degrees) (270 ⋅ degrees)
    ,An $ V2 (-90 ⋅ degrees)   (0 ⋅ degrees)
    ,An $ V2   (0 ⋅ degrees)  (90 ⋅ degrees)
    ,An $ V2  (90 ⋅ degrees) (180 ⋅ degrees))
  where !degrees = pi/180

aRectAnglesFromMidSWCW ∷ (Fractional a, Floating a) ⇒ (An a, An a, An a, An a, An a, An a)
aRectAnglesFromMidSWCW
  = (An $ V2 (135 ⋅ degrees) (180 ⋅ degrees)
    ,An $ V2 (180 ⋅ degrees) (270 ⋅ degrees)
    ,An $ V2 (-90 ⋅ degrees) (-45 ⋅ degrees)
    ,An $ V2 (-45 ⋅ degrees)   (0 ⋅ degrees)
    ,An $ V2   (0 ⋅ degrees)  (90 ⋅ degrees)
    ,An $ V2  (90 ⋅ degrees) (135 ⋅ degrees))
  where !degrees = pi/180


-- | Space partitioning

data Space (pinned ∷ Bool) a (n ∷ Nat) where
  End ∷ Num a ⇒                 Space p a 0
  Spc ∷
    (Num a, n ~ (m + 1)) ⇒
    { sWrap  ∷ !(Wrap p a)
    , sInner ∷  Space p a m } → Space p a n

deriving instance Show a ⇒  Show (Space p a n)

instance Num a ⇒ MeasuredMonoid (Space p a) where
  mmempty    = End
  mmappend     End         End      = End
  mmappend     End      t@(Spc _ _) = t
  mmappend  t@(Spc _ _)    End      = t
  mmappend tl@(Spc pl nl)  tr       = Spc pl $ mmappend nl tr

-- | XXX/Lensify: update the wrap of the innermost space
sMapInnermostWrap ∷ (Wrap p u → Wrap p u) → Space p u d → Space p u d
sMapInnermostWrap f s
  | Spc w End ← s = Spc (f w) End
  | Spc w is  ← s = Spc w $ sMapInnermostWrap f is

--- XXX: destroys the depth information
-- sMapInnermost ∷ (Space p d a → Space p e a) → Space p f a → Space p g a
-- sMapInnermost f s
--   | Spc w End ← s = f s
--   | Spc w is  ← s = Spc w $ sMapInnermost f is

-- | Compute the total allocation for a 'Space'.
--   Complexity: O(depth) for un-pinned, O(1) for pinned.
sDim ∷ Space p u d → Di u
sDim  End                            = zero
sDim (Spc  w@(Wrap  _ _)     sInner) =  wDim w ^+^ sDim sInner
sDim (Spc pw@(PWrap _ _ _ _) sInner) = pwDim pw

-- | Compute the SE point for an un-pinned 'Space', given its NW point.
--   Complexity: O(depth).
sSE  ∷ Space False a d → Po a → Po a
sSE  s@Spc{..} lt = Po $ fromPo lt ^+^ fromDi (sDim s) --  ^-^ V2 1 1

-- | Pin space to the @lt
sPin ∷ Num a ⇒ Po a → Space False a n → Space True a n
sPin lt space = loop space zero rb
  where rb = sSE space lt
        loop ∷ Space False a n → Po a → Po a → Space True a n
        loop  End        _       _        = End
        loop (Spc Wrap{..} swInner) lt rb =
          Spc (PWrap { pwNWd = wNWd, pwSEd = wSEd, pwNWp = lt, pwSEp = rb })
          $   loop swInner
              (lt ^+^ (Po ∘ fromDi) wNWd)
              (rb ^-^ (Po ∘ fromDi) wSEd)

-- This is the significant hack in the model: a contiguous area is represented as
-- a wrap around a zero-sized point amidst the area.
sArea ∷ Fractional a ⇒ Di a → Space False a 1
sArea dim = Spc (wArea dim) End

sGrowS ∷ Num a ⇒ a → Space False a d → Space False a (d + 1)
sGrowS delta sp = Spc (wSymm delta) sp

sGrowGX, sGrowGY ∷ Double → Space False Double n → Space False Double (n + 1)
sGrowGX d sp = Spc (wGoldSX d) sp
sGrowGY d sp = Spc (wGoldSY d) sp

sCutOutsideS2 ∷ Di a → Space False a n → Space False a (n + 1)
sCutOutsideS2 cut s@Spc{..} = Spc (Wrap cut cut)                $ s { sWrap = omap (^-^ fromDi cut) sWrap }

sCutInsideS2  ∷ Fractional a ⇒ Di a → Space False a n → Space False a (n+1)
sCutInsideS2  cut s@Spc{..} = Spc (omap (^-^ fromDi cut) sWrap) $ s { sWrap = Wrap cut cut }

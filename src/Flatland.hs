{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# LANGUAGE DataKinds, KindSignatures, TypeApplications, TypeInType #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RankNTypes, TypeSynonymInstances, UndecidableInstances #-}
{-# LANGUAGE GADTs, TypeFamilies, TypeFamilyDependencies #-}
{-# LANGUAGE BangPatterns, MultiWayIf, RecordWildCards, StandaloneDeriving, TypeOperators #-}
{-# LANGUAGE DeriveFunctor, DeriveGeneric, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
module Flatland where

-- Basis
import           Prelude.Unicode

-- Type-level
import           GHC.TypeLits

-- General types
import qualified GHC.Generics                      as GHC
import           Control.Applicative
import           Control.Lens
import           Control.Lens.TH
import           Control.Monad.Random
import           Control.Monad.State
import           Data.Function
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

newtype R   a = R   { _rV  ∷ a } deriving (Eq, Functor, Num)             -- ^ Radius
newtype Th  a = Th  { _thV ∷ a } deriving (Eq, Fractional, Functor, Num) -- ^ Thickness
newtype He  a = He  { _heV ∷ a } deriving (Eq, Functor, Num)             -- ^ Height
newtype Wi  a = Wi  { _wiV ∷ a } deriving (Eq, Functor, Num)             -- ^ Width
deriving instance GHC.Generic (R a)
deriving instance Show a ⇒ Show (R  a)
deriving instance Show a ⇒ Show (Th a)
deriving instance Show a ⇒ Show (He a)
deriving instance Show a ⇒ Show (Wi a)
deriving instance Random a ⇒ Random (R a)
deriving instance Random a ⇒ Random (Th a)
deriving instance Random a ⇒ Random (He a)
deriving instance Random a ⇒ Random (Wi a)
makeLenses ''R
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
goldenRatio ∷ Double
goldenRatio = 1.61803398875

v2symm ∷ a → V2 a
v2symm x = V2 x x

v2negp ∷ (Num a, Ord a) ⇒ V2 a → Bool
v2negp (V2 d0 d1) = d0 < 0 || d1 < 0

newtype  Di a =  Di { _diV  ∷ V2 a } deriving (Additive, Applicative, Eq, Functor) -- ^ Dimensions
newtype  Po a =  Po { _poV  ∷ V2 a } deriving (Additive, Applicative, Eq, Functor) -- ^ Coordinates
newtype SDi a = SDi { _sdiV ∷ V4 a } deriving                        (Eq, Functor) -- ^ Side-wise dimensions: N, E, S, W
-- deriving instance Additive Di
deriving instance Show a ⇒ Show  (Di a); deriving instance Show a ⇒ Show  (Po a); deriving instance Show a ⇒ Show (SDi a)

newtype An  a = An  { _anV  ∷ V2 a } deriving (Eq, Functor) -- ^ Unordered pair of angles
newtype Co  a = Co  { _coV  ∷ V4 a } deriving (Eq, Functor) -- ^ Color
deriving instance Show a ⇒ Show (An a)
deriving instance Show a ⇒ Show (Co a)
deriving instance Random a ⇒ Random (Di a)
deriving instance Random a ⇒ Random (Po a)
deriving instance Random a ⇒ Random (SDi a)
deriving instance Random a ⇒ Random (Co a)
deriving instance Random a ⇒ Random (An a)
makeLenses ''Di
makeLenses ''Po
makeLenses ''SDi
makeLenses ''An
makeLenses ''Co

di ∷ Wi a → He a  → Di a
di  (Wi x) (He y) = Di $ V2 x y
po ∷ a → a → Po a
po x y = Po $ V2 x y
an ∷ a → a → An a
an x y = An $ V2 x y
co ∷ a → a → a → a → Co a
co r g b a = Co $ V4 r g b a

poBy ∷ Num a ⇒ Po a → V2 a → Po a
poBy po = Po ∘ (+ _poV po)
poByDi ∷ Num a ⇒ Po a → Di a → Po a
poByDi po = poBy po ∘ _diV

poSub ∷ Num a ⇒ Po a → Po a → Di a
poSub (Po v) (Po v') = Di $ v ^-^ v'

-- | Orientation: _ north-west, clockwise to west.
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
  FArea  ∷ Num a ⇒ -- ^ A non-positioned, free area.
    { _aD    ∷ !(Di a)
    } → S Area False a
  PArea  ∷ Num a ⇒ -- ^ A positioned, pinned area.
    { _paD   ∷ !(Di a)
    , _paNWp ∷ !(Po a)
    } → S Area True a
  FWrap  ∷ Num a ⇒ -- ^ The non-positioned, dimensional-only, free wrap.
    { _wNWd  ∷ !(Di a) -- ^ The combined offsets _ the left and top sides.
    , _wSEd  ∷ !(Di a) -- ^ The combined offsets _ the right and bottom sides.
    } → S Wrap False a
  PWrap ∷ Num a ⇒ -- ^ The pinned variant -- enriched with a position.
    { _pwNWd ∷ !(Di a) -- ^ ..same as above.
    , _pwSEd ∷ !(Di a) -- ^ ..same as above.
    , _pwNWp ∷ !(Po a) -- ^ Coordinates of the top-leftmost pixel of the wrap area.
    , _pwSEp ∷ !(Po a) -- ^ Coordinates of the bottom-rightmost pixel of the wrap area.
    } → S Wrap True a
deriving instance Show u ⇒ Show (S k p u)
aD :: Lens' (S Area p a) (Di a)
aD f FArea{..} = FArea <$> f _aD
aD f PArea{..} = flip PArea _paNWp <$> f _paD



-- * Constructors
class Num a ⇒ AspectS a k where aspect ∷ Di a → S k False a
class Num a ⇒ SymmS   a k where symm   ∷ Th a → S k False a
class Num a ⇒ GoldXS  a k where goldX  ∷ Wi a → S k False a
class Num a ⇒ GoldYS  a k where goldY  ∷ He a → S k False a
instance Num a ⇒      AspectS a Area where aspect =      FArea
instance Num a ⇒      AspectS a Wrap where aspect = join FWrap
instance Num a ⇒      SymmS   a Area where symm   =      FArea ∘ Di ∘ join V2 ∘ _thV
instance Num a ⇒      SymmS   a Wrap where symm   = join FWrap ∘ Di ∘ join V2 ∘ _thV
instance RealFrac a ⇒ GoldXS  a Area where goldX  =      FArea ∘ goldXdi
instance RealFrac a ⇒ GoldXS  a Wrap where goldX  = join FWrap ∘ goldXdi
instance RealFrac a ⇒ GoldYS  a Area where goldY  =      FArea ∘ goldYdi
instance RealFrac a ⇒ GoldYS  a Wrap where goldY  = join FWrap ∘ goldYdi

-- mkAreaWrapPoDi ∷ Num a ⇒ Po a → Di a → S Wrap True a
-- mkAreaWrapPoDi ltpo ltdi = PWrap  ltdi         zero ltpo (off ltpo $ _Di ltdi)
-- mkAreaWrapPoPo ∷ Num a ⇒ Po a → Po a → S Wrap True a
-- mkAreaWrapPoPo lt   rb   = PWrap (poSub rb lt) zero lt    rb
-- mkAreaIntersectWrap ∷ Num a ⇒ S Wrap True a → S Wrap True a → S Wrap True a
-- mkAreaIntersectWrap (PWrap ltd rbd ltp rbp) rb   = PWrap (poSub rb lt) zero lt    rb

-- instance Random a ⇒ Random (S Area True a) where
--   random =
--     runState $ Spc <$> (mkAreaWrap <$> (state random) <*> (state random)) <*> pure End
--   randomR (V2 x y, V2 x' y') =
--     runState $ liftA2 V2 (state $ randomR (x, x')) (state $ randomR (y, y'))


-- * Projections
--
paSEp ∷ S Area True a → Po a
paSEp (PArea di po) = poByDi po di

center ∷ Fractional a ⇒ S k True a → Po a
center (PArea d p)                 = poByDi p (d ^/ 2)
center (PWrap _ _ (Po nw) (Po se)) = Po $ (se ^+^ nw) ^/ 2

dim ∷ S k p a → Di a
dim FArea{..} = _aD
dim PArea{..} = _paD
dim FWrap{..} = _wNWd ^+^ _wSEd
dim PWrap{..} = Di ∘ _poV $ _pwSEp ^-^ _pwNWp

-- | Narrowing into a positioned 'S Wrap'.
stepIntoNW, stepIntoSE ∷ S Wrap True a → Po a
stepIntoNW (PWrap (Di pwNWd) _ (Po pwNWp) _) = Po $ pwNWp ^+^ pwNWd
stepIntoSE (PWrap _ (Di pwSEd) _ (Po pwSEp)) = Po $ pwSEp ^-^ pwSEd

stepInto ∷               S Wrap True a → (Po a, Po a)
stepInto pw = (stepIntoNW pw, stepIntoNW pw)

-- | Narrowing into a positioned 'Wrap', halfway.
stepIntoNW'2, stepIntoSE'2 ∷ Fractional a ⇒ S Wrap True a → Po a
stepIntoNW'2 (PWrap (Di pwNWd) _ (Po pwNWp) _) = Po $ pwNWp ^+^ pwNWd ⋅ 0.5
stepIntoSE'2 (PWrap _ (Di pwSEd) _ (Po pwSEp)) = Po $ pwSEp ^-^ pwSEd ⋅ 0.5

stepInto'2 ∷                 Fractional a ⇒ S Wrap True a → (Po a, Po a)
stepInto'2 pw = (stepIntoNW'2 pw, stepIntoSE'2 pw)

-- * 'l', 't', 'r', 'b' -- sidewise dimensions.
--
l, r ∷ S Wrap p a → Wi a
t, b ∷ S Wrap p a → He a
l FWrap{..} = Wi ∘ (view _x) $ _diV _wNWd; l PWrap{..} = Wi ∘ (view _x) $ _diV _pwNWd
t FWrap{..} = He ∘ (view _y) $ _diV _wNWd; t PWrap{..} = He ∘ (view _y) $ _diV _pwNWd
r FWrap{..} = Wi ∘ (view _x) $ _diV _wSEd; r PWrap{..} = Wi ∘ (view _x) $ _diV _pwSEd
b FWrap{..} = He ∘ (view _y) $ _diV _wSEd; b PWrap{..} = He ∘ (view _y) $ _diV _pwSEd


-- * Combinators
--
area ∷ S k True a → S Area True a
area a@PArea{..} = a
area (PWrap _ _ (Po nw) (Po se)) =
  PArea (Di $ se ^-^ nw) (Po nw)

areaSubtract ∷ S Area p a → S Area p a → S Area p a
FArea l `areaSubtract` FArea r = FArea $ l ^-^ r

-- newtype ProS p co pr = ProS { _ProS ∷ S Area p pr }
-- instance Profunctor (ProS p) where
--   dimap con (cov ∷ ) (ProS x) = ProS $ cov x

-- ..for inspiration:
-- instance Profunctor (->) where
--   dimap con cov f = cov ∘ f ∘ con
--         con     ∷   a    ←   c
--             cov ∷     b  →     d
--   dimap con cov ∷ f a b  → f c d


-- * General transformations (parametrized combinators)
--
narrowByTh ∷ Th a → S Area p a → S Area p a
narrowByTh (Th d) (FArea (Di a))        = FArea (Di $ a ^-^ (V2 d d) ^* 2)
narrowByTh (Th d) (PArea (Di a) (Po p)) = PArea (Di $ a ^-^ (V2 d d) ^* 2) (Po $ p ^+^ (V2 d d))


-- * Pinning
-- | Make pwNWp and pwSEp the top-leftmost and bottom-rightmost pixels of the 'Wrap'.
--   Warning:  no check on whether the coordinates are compatible with the dimensions is performed.
pinArea ∷ S Area False a → Po a → S Area True a
pinArea FArea{..} _paNWp = PArea{..}
  where _paD = _aD
pinWrap ∷ S Wrap False a → Po a → Po a → S Wrap True a
pinWrap FWrap{..} _pwNWp _pwSEp = PWrap{..}
  where _pwNWd = _wNWd
        _pwSEd = _wSEd


-- * Drawing
-- | Wrap rendering as a rounded rectangle.
wrapRoundedRectFeatures ∷ Floating a ⇒ S Wrap True a → R a → Th a → [RoundRectFeature a]
wrapRoundedRectFeatures pw@PWrap{..} rr@(R r) th =
  let pa@(PArea (Di (V2 wi he)) (Po (V2 w n))) = narrowByTh ((/2) <$> th) $ area pw
      V2 e s = _poV $ paSEp pa
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
  GRC.arc cx cy (_rV r) sa ea

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

aRectAngles_MidSWCW ∷ (Fractional a, Floating a) ⇒ (An a, An a, An a, An a, An a, An a)
aRectAngles_MidSWCW
  = (An $ V2 (135 ⋅ degrees) (180 ⋅ degrees)
    ,An $ V2 (180 ⋅ degrees) (270 ⋅ degrees)
    ,An $ V2 (-90 ⋅ degrees) (-45 ⋅ degrees)
    ,An $ V2 (-45 ⋅ degrees)   (0 ⋅ degrees)
    ,An $ V2   (0 ⋅ degrees)  (90 ⋅ degrees)
    ,An $ V2  (90 ⋅ degrees) (135 ⋅ degrees))
  where !degrees = pi/180


-- | Space partitioning

data Space (pinned ∷ Bool) a (n ∷ Nat) where
  End   ∷ Num a ⇒                Space p a 0
  SArea ∷ Num a ⇒
    { sArea  ∷ !(S Area p a) } → Space p a 1
  Spc   ∷
    (Num a, n ~ (m + 1)) ⇒
    { sWrap  ∷ !(S Wrap p a)
    , sInner ∷  Space p a m }  → Space p a n

deriving instance Show a ⇒  Show (Space p a n)

instance Num a ⇒ MeasuredMonoid (Space p a) where
  mmempty    = End
  mmappend     End       End       = End
  mmappend     End     x@SArea{..} = x
  mmappend     End     x@Spc{..}   = x
  mmappend  x@SArea{..}  End       = x
  mmappend  x@Spc{..}    End       = x
  mmappend tl@(Spc pl nl) tr       = Spc pl $ mmappend nl tr

type SubArea a = Space True a 2


-- * Constructors
makeArea ∷ Fractional a ⇒ Di a → Space False a 1
makeArea = SArea ∘ FArea

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
                 | SArea sa ← s = SArea (f sa)
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
spaceDim (SArea a)                      = dim a
spaceDim (Spc w@(FWrap  _ _)    sInner) = dim w ^+^ spaceDim sInner
spaceDim (Spc w@(PWrap _ _ _ _) sInner) = dim w


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
        loop (SArea FArea{..})       lt  _ =
          SArea (PArea { _paD   = _aD,                   _paNWp = lt })
        loop (Spc FWrap{..} swInner) lt rb =
          Spc   (PWrap { _pwNWd = _wNWd, _pwSEd = _wSEd, _pwNWp = lt, _pwSEp = rb })
          $   loop swInner
              (lt ^+^ (Po ∘ _diV) _wNWd)
              (rb ^-^ (Po ∘ _diV) _wSEd)

-- XXX: not sure if needed
-- sCutOutsideS2 ∷ Di a → Space False a n → Space False a (n + 1)
-- sCutOutsideS2 cut s@Spc{..} = Spc (FWrap cut cut)                $ s { sWrap = omap (^-^ _diV cut) sWrap }
-- sCutInsideS2  ∷ Fractional a ⇒ Di a → Space False a n → Space False a (n+1)
-- sCutInsideS2  cut s@Spc{..} = Spc (omap (^-^ _diV cut) sWrap) $ s { sWrap = FWrap cut cut }

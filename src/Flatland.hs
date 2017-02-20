{-# LANGUAGE DataKinds, KindSignatures #-}
{-# LANGUAGE TypeFamilies, GADTs #-}
{-# LANGUAGE BangPatterns, MultiWayIf, RecordWildCards, StandaloneDeriving, TypeOperators #-}
{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}
module Flatland where

-- Generic types
import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans.Reader               (ReaderT(..))
import qualified Data.ByteString.Char8             as SB
import           Data.Map (Map)
import qualified Data.Map                          as Map
import           Data.Maybe
import           Data.MonoTraversable
import qualified Data.Text                         as DT
import qualified Data.Vector                       as V
import           GHC.TypeLits

-- Algebra
import           Linear

-- Misc
import           Text.Printf                              (printf)
import           Text.Show.Pretty                         (ppShow)

-- Cairo and Pango
import qualified Graphics.Rendering.Cairo          as GRC -- (moveTo, Render, Format(..))
import qualified Graphics.Rendering.Cairo.Internal as GRC (Render(..), create, imageSurfaceCreate)
import qualified Graphics.Rendering.Cairo.Types    as GRC -- (Cairo(..))
import qualified Graphics.Rendering.Cairo.Internal as GRCI-- (Cairo(..))

import qualified Data.GI.Base                      as GI
import qualified GI.Cairo                          as GIC
import qualified GI.Cairo.Structs.Context          as GIC -- (Context(..))
import qualified GI.Pango                          as GIP
import qualified GI.PangoCairo.Interfaces.FontMap  as GIPC -- (fontMapGetDefault)
import qualified GI.PangoCairo.Functions           as GIPC -- (createLayout, showLayout)

-- Dirty stuff
import qualified Foreign.C.Types                   as F
import qualified Foreign                           as F
import qualified Foreign.Ptr                       as F
import qualified System.IO.Unsafe                  as UN

-- LambdaCube
import           Graphics.GL.Core33
import qualified LambdaCube.GL                     as GL
import qualified LambdaCube.GL.Mesh                as GL
import qualified LambdaCube.GL.Type                as GL
import qualified LambdaCube.Linear                 as LCLin
import           LambdaCube.Mesh

---
--- XXX: HUGE NOTE: V2/Co/Wrap mass of code below is likely silliness,
---      easily replaced with a couple of short lens operators.
---      Nevertheless, it is what it is -- because of blissful lack of education.
---      Typical.
---

goldenRatio :: Double
goldenRatio = 1.61803398875

v2symm :: a -> V2 a
v2symm x = V2 x x

v2negp :: (Num a, Ord a) => V2 a -> Bool
v2negp (V2 d0 d1) = d0 < 0 || d1 < 0


-- | Type-safe flat space types.
newtype Di  a = Di  { fromDi  :: V2 a } deriving (Additive, Eq, Functor) -- ^ Dimensions
newtype Po  a = Po  { fromPo  :: V2 a } deriving (Additive, Eq, Functor) -- ^ Coordinates
newtype SDi a = SDi { fromSDi :: V4 a } deriving           (Eq)          -- ^ Side-wise dimensions: north, east, south, west
newtype SPo a = SPo { fromSpo :: V4 a } deriving           (Eq)          -- ^ Side-wise positions:  north, east, south, west
newtype An  a = An  { fromAn  :: V2 a } deriving (Additive, Eq, Functor) -- ^ Angles
newtype Co  a = Co  { fromCo  :: V4 a } deriving           (Eq, Functor) -- ^ Color
newtype R   a = R   { fromR   ::    a } deriving           (Eq, Functor, Num) -- ^ Radius
newtype Th  a = Th  { fromTh  ::    a } deriving           (Eq, Fractional, Functor, Num) -- ^ Thickness
deriving instance Show a => Show (Di a)
deriving instance Show a => Show (Po a)
deriving instance Show a => Show (SDi a)
deriving instance Show a => Show (SPo a)
deriving instance Show a => Show (An a)
deriving instance Show a => Show (Co a)
deriving instance Show a => Show (R a)
deriving instance Show a => Show (Th a)

di :: a -> a -> Di a
di x y = Di $ V2 x y
po :: a -> a -> Po a
po x y = Po $ V2 x y
an :: a -> a -> An a
an x y = An $ V2 x y
co :: a -> a -> a -> a -> Co a
co r g b a = Co $ V4 r g b a

off :: Num a => Po a -> V2 a -> Po a
off co = Po . (+ fromPo co)

-- | Orientation: from north-west, clockwise to west.
data Orient = ONW | ON | ONE | OE | OSE | OS | OSW | OW
  deriving (Eq, Show)

vROri :: Num a => R a -> Orient -> (V2 a, V2 a)
vROri (R r) o | ON  <- o = (z, n) | ONE <- o = (e, n)
              | OE  <- o = (e, z) | OSE <- o = (e, s)
              | OS  <- o = (z, s) | OSW <- o = (w, s)
              | OW  <- o = (w, z) | ONW <- o = (w, n)
  where n = V2  0(-r)
        s = V2  0  r
        e = V2  r  0
        w = V2(-r) 0
        z = zero

oriCenterRChordCW :: Num a => Orient -> Po a -> R a -> (Po a, Po a)
oriCenterRChordCW o c r
  | ONE <- o = (oy, ox)
  | OSE <- o = (ox, oy)
  | OSW <- o = (oy, ox)
  | ONW <- o = (ox, oy)
  where (vx, vy) = vROri r o
        (ox, oy) = (off c vx, off c vy)

di2goldX, di2goldY :: Double -> Di Double
di2goldX x = Di $ V2  x               (x / goldenRatio)
di2goldY y = Di $ V2 (y / goldenRatio) y

poRectOppo :: Po a -> Po a -> (Po a, Po a)
poRectOppo !(Po (V2 c00 c01)) !(Po (V2 c10 c11))
  = (Po (V2 c00 c11), Po (V2 c10 c01))

spoNarrow :: Num a => Th a -> SPo a -> SPo a
spoNarrow (Th d) (SPo (V4 n e s w))
  = SPo $ V4 (n + d) (e - d) (s - d) (w + d)

thLineSet :: Th Double -> GRCI.Render ()
thLineSet !(Th th)
  = GRC.setLineWidth th

-- | Description of rounded rectangle features -- sides and corners.
data RoundRectFeature a where
  RRCorn ::
    { rrOri :: !Orient
    , rraCt :: !(Po a)
    , rraAn :: !(An a)
    , rraR  :: !(R a)
    } -> RoundRectFeature a
  RRSide ::
    { rrOri :: !Orient
    , rrsFr :: !(Po a)
    , rrsTo :: !(Po a)
    } -> RoundRectFeature a
deriving instance Show a => Show (RoundRectFeature a)

poArc :: Po Double -> Double -> An Double -> GRCI.Render ()
poArc !(Po (V2 x y)) !r !(An (V2 angs ange))
  = GRC.arc x y r angs ange
  where degrees = pi/180

coOpaq :: Num a => a -> a -> a -> Co a
coOpaq r g b = Co $ V4 r g b 1

coGray :: a -> a -> Co a
coGray x a = Co $ V4 x x x a

coMult :: Num a => a -> Co a -> Co a
coMult x (Co (V4 r g b a)) = Co $ V4 (r*x) (g*x) (b*x) a

coSetSourceColor :: Co Double -> GRCI.Render ()
coSetSourceColor !(Co (V4 r g b a))
  = GRC.setSourceRGBA r g b a

coGradientSet :: Co Double -> Co Double -> GRC.Pattern -> IO ()
coGradientSet !(Co (V4 rs gs bs as)) !(Co (V4 re ge be ae)) pat = do
  GRCI.patternAddColorStopRGBA pat 0 rs gs bs as
  GRCI.patternAddColorStopRGBA pat 1 re ge be ae

coPatternGradLinear :: Po Double -> Co Double -> Po Double -> Co Double -> IO GRC.Pattern
coPatternGradLinear !(Po (V2 xs ys)) !sco !(Po (V2 xe ye)) !eco = do
  p <- GRCI.patternCreateLinear xs ys xe ye
  coGradientSet sco eco p
  pure p

coPatternGradRadial :: Po Double -> Double -> Co Double -> Po Double -> Double -> Co Double -> IO GRC.Pattern
coPatternGradRadial !(Po (V2 xi yi)) !ir !ico !(Po (V2 xo yo)) !or !oco = do
  p <- GRCI.patternCreateRadial xi yi ir xo yo or
  coGradientSet ico oco p
  pure p


-- | A 'Wrap' is a rectangular "donut", wrapping something inside it.
--   It effectively partitions space into:
--   - the __/wrap area/__, around the /wrapped area/ -- this is what 'Wrap' corresponds to,
--   - the __/wrapped area/__, inside the 'Wrap' itself.
data Wrap (pinned :: Bool) a where
  Wrap  :: Num a => -- ^ The non-positioned, dimensional-only wrap.
    { wNWd  :: !(Di a) -- ^ The combined offsets from the left and top sides.
    , wSEd  :: !(Di a) -- ^ The combined offsets from the right and bottom sides.
    } -> Wrap False a
  PWrap :: Num a => -- ^ The /pinned/ variant -- enriched with a position.
    { pwNWd :: !(Di a) -- ^ ..same as above.
    , pwSEd :: !(Di a) -- ^ ..same as above.
    , pwNWp :: !(Po a) -- ^ Coordinates of the top-leftmost pixel of the wrap area.
    , pwSEp :: !(Po a) -- ^ Coordinates of the bottom-rightmost pixel of the wrap area.
    } -> Wrap True a
deriving instance Show a => Show (Wrap p a)

wL, wT, wR, wB :: Wrap p a -> a
wL  Wrap{..} = (view _x) $ fromDi wNWd; wL PWrap{..} = (view _x) $ fromDi pwNWd
wT  Wrap{..} = (view _y) $ fromDi wNWd; wT PWrap{..} = (view _y) $ fromDi pwNWd
wR  Wrap{..} = (view _x) $ fromDi wSEd; wR PWrap{..} = (view _x) $ fromDi pwSEd
wB  Wrap{..} = (view _y) $ fromDi wSEd; wB PWrap{..} = (view _y) $ fromDi pwSEd

pwPosition :: Wrap True a -> SPo a
pwPosition (PWrap _ _ (Po (V2 nwx nwy)) (Po (V2 sex sey))) =
  SPo $ V4 nwy sex sey nwx

-- | Narrowing into a positioned 'Wrap'.
pwNWpi, pwSEpi :: Wrap True a -> Po a
pwNWpi (PWrap (Di pwNWd) _ (Po pwNWp) _) = Po $ pwNWp ^+^ pwNWd
pwSEpi (PWrap _ (Di pwSEd) _ (Po pwSEp)) = Po $ pwSEp ^-^ pwSEd

pwPosIn        :: Wrap True a -> (Po a, Po a)
pwPosIn pw = (pwNWpi pw, pwSEpi pw)

-- | Narrowing into a positioned 'Wrap', halfway.
pwNWpi'2, pwSEpi'2 :: Fractional a => Wrap True a -> Po a
pwNWpi'2 (PWrap (Di pwNWd) _ (Po pwNWp) _) = Po $ pwNWp ^+^ pwNWd * 0.5
pwSEpi'2 (PWrap _ (Di pwSEd) _ (Po pwSEp)) = Po $ pwSEp ^-^ pwSEd * 0.5

pwPosIn'2          :: Fractional a => Wrap True a -> (Po a, Po a)
pwPosIn'2 pw = (pwNWpi'2 pw, pwSEpi'2 pw)

--   Is there a useful meaning for a monoid?
-- instance (Num a, Monoid (V2 a)) => Monoid (Wrap a) where
--   mempty = Wrap mempty mempty
--   Wrap l0 l1 `mappend` Wrap r0 r1 = Wrap (mappend l0 r0) (mappend l1 r1)
-- instance forall a.Num a => Functor (Wrap a) where
--   fmap f Wrap{..} = Wrap (fmap f wLB) (fmap f wRT)

--- It's /clearly/ a profunctorial transform, but I don't care yet..
type instance Element (Wrap p a) = V2 a
instance MonoFunctor (Wrap p a) where
  omap f  Wrap{..} =  Wrap (f'di wNWd) (f'di wSEd)
    where f'di = Di . f . fromDi
  omap f PWrap{..} = PWrap (f'di pwNWd) (f'di pwSEd) (f'po pwNWp) (f'po pwSEp)
    where f'di = Di . f . fromDi
          f'po = Po . f . fromPo

wDim :: Wrap s a -> Di a
wDim  Wrap{..} = wNWd ^+^ wSEd
wDim PWrap{..} = pwNWd ^+^ pwSEd

-- | Make pwNWp and pwSEp the top-leftmost and bottom-rightmost pixels of the 'Wrap'.
--   Warning:  no check on whether the coordinates are compatible with the dimensions is performed.
wPin :: Wrap False a -> Po a -> Po a -> Wrap True a
wPin Wrap{..} pwNWp pwSEp = PWrap{..}
  where pwNWd = wNWd
        pwSEd = wSEd

-- | Smart constructors for 'Wrap'.
wSymm :: Num a => a -> Wrap False a
wSymm d = Wrap (Di $ v2symm d) (Di $ v2symm d)

wGoldSX, wGoldSY :: Double -> Wrap False Double
wGoldSX x = Wrap w w where w = di2goldX x
wGoldSY y = Wrap w w where w = di2goldY y

-- | Wrap rendering as a rounder rectangle.
wrapRoundedRectFeatures :: Floating a => Wrap True a -> R a -> Th a -> [RoundRectFeature a]
wrapRoundedRectFeatures pw@PWrap{..} rr@(R r) th =
  let SPo (V4 n e s w) = spoNarrow ((/2) <$> th) $ pwPosition pw
      !degrees         = pi/180
  in [RRSide ON  (po (w + r)  n)      (po (e - r) n)
     ,RRCorn ONE (po (e - r) (n + r)) (an (-90 * degrees)   (0 * degrees)) rr
     ,RRSide OE  (po  e      (n + r)) (po  e     (s - r))
     ,RRCorn OSE (po (e - r) (s - r)) (an   (0 * degrees)  (90 * degrees)) rr
     ,RRSide OS  (po (w + r)  s)      (po (e - r) s)
     ,RRCorn OSW (po (w + r) (s - r)) (an  (90 * degrees) (180 * degrees)) rr
     ,RRSide OW  (po  w      (n + r)) (po  w     (s - r))
     ,RRCorn ONW (po (w + r) (n + r)) (an (180 * degrees) (270 * degrees)) rr]

executeFeature :: Maybe (Co Double) -> Maybe (Co Double) -> RoundRectFeature Double -> GRC.Render ()
executeFeature !cStart !cEnd !(RRSide _ (Po (V2 sx sy)) (Po (V2 ex ey))) = do
  -- pat <- UN.unsafeInterleaveIO $ coPatternGradLinear rrsFr cStart rrsTo cEnd
  if | cStart /= cEnd    -> error "Not implemented: sidewise gradients."
     | Nothing <- cStart -> pure ()
     | Just c  <- cStart -> coSetSourceColor c
  GRC.moveTo sx sy
  GRC.lineTo ex ey
executeFeature !cStart !cEnd !(RRCorn o c@(Po (V2 cx cy)) (An (V2 sa ea)) r) = do
  let (cs, ce) = oriCenterRChordCW o c r
  if | cStart /= cEnd    -> GRC.setSource =<< (GRC.liftIO $ coPatternGradLinear cs (fromJust cStart) ce (fromJust cEnd))
     | Nothing <- cStart -> pure ()
     | Just c  <- cStart -> coSetSourceColor c
  GRC.arc cx cy (fromR r) sa ea

poNWSERectArcCentersCW :: Num a => Po a -> Po a -> R a -> (Po a, Po a, Po a, Po a)
poNWSERectArcCentersCW !lt@(Po (V2 ltx lty)) !rb@(Po (V2 rbx rby)) (R r) =
  let (Po (V2 lbx lby), Po (V2 rtx rty)) = poRectOppo lt rb
  in (Po (V2 (ltx + r) (lty + r))
     ,Po (V2 (rtx - r) (rty + r))
     ,Po (V2 (rbx - r) (rby - r))
     ,Po (V2 (lbx + r) (lby - r)))

aRectAnglesNWCW :: (Fractional a, Floating a) => (An a, An a, An a, An a)
aRectAnglesNWCW
  = (An $ V2 (180 * degrees) (270 * degrees)
    ,An $ V2 (-90 * degrees)   (0 * degrees)
    ,An $ V2   (0 * degrees)  (90 * degrees)
    ,An $ V2  (90 * degrees) (180 * degrees))
  where !degrees = pi/180

aRectAnglesFromMidSWCW :: (Fractional a, Floating a) => (An a, An a, An a, An a, An a, An a)
aRectAnglesFromMidSWCW
  = (An $ V2 (135 * degrees) (180 * degrees)
    ,An $ V2 (180 * degrees) (270 * degrees)
    ,An $ V2 (-90 * degrees) (-45 * degrees)
    ,An $ V2 (-45 * degrees)   (0 * degrees)
    ,An $ V2   (0 * degrees)  (90 * degrees)
    ,An $ V2  (90 * degrees) (135 * degrees))
  where !degrees = pi/180


-- * Space partitioning

data Space (pinned :: Bool) (depth :: Nat) a where
  End :: Num a =>                 Space p  0    a
  Spc :: Num a =>
    { sWrap  :: !(Wrap p   a)
    , sInner ::  Space p m a } -> Space p (n+1) a

-- | Compute the total allocation for an un-pinned 'Space'.
--   Complexity: O(depth).
sDim :: Space False d a -> Di a
sDim s@Spc{..} = wDim sWrap ^+^ sDim sInner
sDim   End     = zero

-- | Compute the SE point for an un-pinned 'Space', given its NW point.
--   Complexity: O(depth).
sSE  :: Space False d a -> Po a -> Po a
sSE  s@Spc{..} lt = Po $ fromPo lt ^+^ fromDi (sDim s) --  ^-^ V2 1 1

-- | Pin space to the @lt
sPin :: Num a => Space False n a -> Po a -> Space True n a
sPin space lt = loop space zero rb
  where rb = sSE space lt
        loop :: Space False n a -> Po a -> Po a -> Space True n a
        loop  End        _       _        = End
        loop (Spc Wrap{..} swInner) lt rb =
          Spc (PWrap { pwNWd = wNWd, pwSEd = wSEd, pwNWp = lt, pwSEp = rb })
          $   loop swInner
              (lt ^+^ (Po . fromDi) wNWd)
              (rb ^-^ (Po . fromDi) wSEd)

-- This is the significant hack in the model: a contiguous area is represented as
-- a wrap around a zero-sized point amidst the area.
sArea :: Fractional a => Di a -> Space False 1 a
sArea dim = Spc (Wrap half half) End
            where half = dim ^/ 2.0

sGrowS :: Num a => a -> Space False d a -> Space False (d + 1) a
sGrowS delta sp = Spc (wSymm delta) sp

sGrowGX, sGrowGY :: Double -> Space False n Double -> Space False (n + 1) Double
sGrowGX d sp = Spc (wGoldSX d) sp
sGrowGY d sp = Spc (wGoldSY d) sp

sCutOutsideS2 :: Di a -> Space False n a -> Space False (n+1) a
sCutOutsideS2 cut s@Spc{..} = Spc (Wrap cut cut)                $ s { sWrap = omap (^-^ fromDi cut) sWrap }

sCutInsideS2  :: Fractional a => Di a -> Space False n a -> Space False (n+1) a
sCutInsideS2  cut s@Spc{..} = Spc (omap (^-^ fromDi cut) sWrap) $ s { sWrap = Wrap cut cut }


-- * Cairo/Pango toolkit

grcToGIC :: GRC.Cairo -> IO GIC.Context
grcToGIC grc = GIC.Context <$> GI.newManagedPtr (F.castPtr $ GRC.unCairo grc) (return ())

defaultFontDesc, terminusFontDesc, aurulentFontDesc :: GIP.FontDescription
aurulentFontDesc = UN.unsafePerformIO $ fontDescriptionFromArgs "Aurulent Sans" GIP.StyleNormal 12288
terminusFontDesc = UN.unsafePerformIO $ fontDescriptionFromArgs "Terminus" GIP.StyleNormal 12288
defaultFontDesc = aurulentFontDesc --terminusFontDesc

fontDescriptionFromArgs :: String -> GIP.Style -> Int -> IO GIP.FontDescription
fontDescriptionFromArgs family style pus = do
  fd <- GIP.fontDescriptionNew
  GIP.fontDescriptionSetFamily fd $ DT.pack family
  GIP.fontDescriptionSetStyle  fd style
  GIP.fontDescriptionSetSize   fd $ fromIntegral pus
  pure fd

  -- fmap        <- GIPC.fontMapGetDefault
  -- ffam        <- fromJust <$> (tryFontMapFamily  fmap $ DT.pack fontfamily)
  -- fface       <- fromJust <$> (tryFontFamilyFace ffam $ DT.pack fontface)
  -- fcsizes     <- fromJust <$> GIP.fontFaceListSizes fface
  -- unless (elem (fromIntegral fontpts) fcsizes) $
  --   error $ printf "No font size %dPU for font %s-%s.\nAvailable sizes: %s." fontpts fontfamily fontface (show fcsizes)
  -- printf "------------ got: %s-%s-%s\n" (DT.unpack $ UN.unsafePerformIO $ GIP.fontFamilyGetName ffam) (DT.unpack $ UN.unsafePerformIO $ GIP.fontFaceGetFaceName fface) (show fcsizes)

tryFontMapFamily :: GIP.FontMap -> DT.Text -> IO (Maybe GIP.FontFamily)
tryFontMapFamily fm req = loop =<< GIP.fontMapListFamilies fm
  where loop []     = pure Nothing
        loop (f:fs) = do
          name <- GIP.fontFamilyGetName f
          if name == req
          then pure $ Just f
          else loop fs
tryFontFamilyFace :: GIP.FontFamily -> DT.Text -> IO (Maybe GIP.FontFace)
tryFontFamilyFace fa req = loop =<< GIP.fontFamilyListFaces fa
  where loop []     = pure Nothing
        loop (f:fs) = do
          name <- GIP.fontFaceGetFaceName f
          if name == req
          then pure $ Just f
          else loop fs


-- * Toolkit

uploadTexture2DToGPU'''' :: Bool -> Bool -> Bool -> Bool -> (Int, Int, GLenum, F.Ptr F.CUChar) -> IO GL.TextureData
uploadTexture2DToGPU'''' isFiltered isSRGB isMip isClamped (w, h, format, ptr) = do
    glPixelStorei GL_UNPACK_ALIGNMENT 1
    to <- F.alloca $! \pto -> glGenTextures 1 pto >> F.peek pto
    glBindTexture GL_TEXTURE_2D to
    let texFilter = if isFiltered then GL_LINEAR else GL_NEAREST
        wrapMode = case isClamped of
            True    -> GL_CLAMP_TO_EDGE
            False   -> GL_REPEAT
        (minFilter,maxLevel) = case isFiltered && isMip of
            False   -> (texFilter,0)
            True    -> (GL_LINEAR_MIPMAP_LINEAR, floor $ log (fromIntegral $ max w h) / log 2)
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S $ fromIntegral wrapMode
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T $ fromIntegral wrapMode
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER $ fromIntegral minFilter
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER $ fromIntegral texFilter
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_BASE_LEVEL 0
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAX_LEVEL $ fromIntegral maxLevel
    let internalFormat  = fromIntegral $ if isSRGB then GL_SRGB8_ALPHA8 else GL_RGBA8
    glTexImage2D GL_TEXTURE_2D 0 internalFormat (fromIntegral w) (fromIntegral h) 0 format GL_UNSIGNED_BYTE $ F.castPtr ptr
    when isMip $ glGenerateMipmap GL_TEXTURE_2D
    return $ GL.TextureData to


-- * Canvas

data CanvasRequest where
  CanvasRequest    ::
    { crSpace      :: Space False 5 Double -- 5 = outer bezel, border, inner bezel, padding, drawing area
    , crText       :: DT.Text
    , crCFore      :: Co Double
    , crCBack      :: Co Double
    , crCLBez      :: Co Double
    , crCBord      :: Co Double
    , crCDBez      :: Co Double
    , crFontDesc   :: GIP.FontDescription
    } -> CanvasRequest

data Canvas where
  Canvas ::
    { cvGPU      :: GL.Object
    , cvMatSlot  :: GL.GLUniformName
    , cvTexture  :: GL.TextureData
    , cvGRCr     :: GRC.Cairo
    , cvGRCSurf  :: GRC.Surface
    , cvGICtx    :: GIC.Context
    , cvGIPLay   :: GIP.Layout
    } -> Canvas

newtype UniformName = UniformName { fromUN :: String } deriving (Show)

cvstream   = "canvas"
cvmaterial = "canvas"

renderCanvasInitial :: GL.GLStorage -> String ->  GL.GLUniformName -> CanvasRequest -> IO Canvas
renderCanvasInitial storage objstream mtlUniform
  (CanvasRequest space@(Spc _ (Spc _ (Spc _ (Spc _ (Spc canvas End)))))
   text fgColor bgColor lBezColor bordColor dBezColor fontdesc) = do
  let total@(V2 totalw totalh) = ceiling <$> fromDi (sDim space)
      V2 canvw canvh = fromDi $ wDim canvas

  grcSurface  <- GRC.createImageSurface GRC.FormatARGB32 totalw totalh
  strideBytes <- GRC.imageSurfaceGetStride grcSurface
  let stridePxs = strideBytes `div` 4
  grc         <- GRC.create grcSurface
  gic         <- grcToGIC grc
  gip         <- GIPC.createLayout gic
  GIP.layoutSetFontDescription gip (Just fontdesc)
  GIP.layoutSetWrap            gip GIP.WrapModeWord
  GIP.layoutSetEllipsize       gip GIP.EllipsizeModeEnd
  GIP.layoutSetWidth           gip =<< (GIP.unitsFromDouble canvw)
  GIP.layoutSetHeight          gip =<< (GIP.unitsFromDouble canvh)
  (`runReaderT` grc) $ GRC.runRender $ do
    -- ((layw, layh), ellipsized) <-
    (case sPin space (Po $ V2 0 0) of
      Spc obez (Spc bord (Spc ibez (Spc pad (Spc canv End)))) -> do
        let d (Po (V2 x y)) (Co (V4 r g b a)) = GRC.setSourceRGBA r g b a >> GRC.rectangle (x) (y) 1 1 >> GRC.fill -- GRC.rectangle (x-1) (y-1) 3 3 >> GRC.fill
            dCorn (RRCorn _ pos _ _) col = d pos col
            ths@[oth, bth, ith, pth]
                          = fmap (Th . wL) [obez, bord, ibez, pad]
            totpadx       = sum ths
            or            = R . fromTh $ totpadx - oth/2
            br            = or - (R . fromTh $ (oth+bth)*0.6)
            ir            = br - (R . fromTh $ (bth+ith)/2)
        -- coSetSourceColor (co 0 1 0 1) >> GRC.paint
        -- background & border arcs
        let bfeats@[n, ne, _, se, _, sw, _, nw] = wrapRoundedRectFeatures bord br bth
        GRC.newPath >> thLineSet bth
        forM_ [n, ne, se, sw, nw] $ executeFeature Nothing Nothing
        coSetSourceColor bgColor >>
          GRC.fillPreserve
        coSetSourceColor bordColor >>
          GRC.stroke

        -- border bezels: light outer TL, dark outer SE
        thLineSet oth
        let ofeats@[n, ne, e, se, s, sw, w, nw] = wrapRoundedRectFeatures obez or oth
        GRC.newPath
        (coSetSourceColor $ lBezColor) >>
          (forM_ [w, nw, n] $ executeFeature Nothing Nothing) >> GRC.stroke
        (coSetSourceColor $ dBezColor) >>
          (forM_ [e, se, s] $ executeFeature Nothing Nothing) >> GRC.stroke
        (coSetSourceColor $ bordColor) >>
          GRC.newPath >> (executeFeature (Just dBezColor) (Just lBezColor) sw) >> GRC.stroke >>
          GRC.newPath >> (executeFeature (Just lBezColor) (Just dBezColor) ne) >> GRC.stroke

        -- border bezels: dark inner TL, light inner SE
        thLineSet ith
        let [n, ne, e, se, s, sw, w, nw] = wrapRoundedRectFeatures ibez ir ith
        GRC.newPath
        (coSetSourceColor $ dBezColor) >>
          (forM_ [w, nw, n] $ executeFeature Nothing Nothing) >> GRC.stroke
        (coSetSourceColor $ lBezColor) >>
          (forM_ [e, se, s] $ executeFeature Nothing Nothing) >> GRC.stroke
        (coSetSourceColor $ bordColor) >>
          GRC.newPath >> (executeFeature (Just lBezColor) (Just dBezColor) sw) >> GRC.stroke >>
          GRC.newPath >> (executeFeature (Just dBezColor) (Just lBezColor) ne) >> GRC.stroke

        -- text
        let Po (V2 cvx cvy) = pwNWp canv
        GRC.moveTo cvx cvy
        coSetSourceColor fgColor
        --
        let text = DT.pack $ printf
                   (unlines
                    ["total=%d,%d  space=%s"
                    ,"totpadx=%f  oth=%f  bth=%f  ith=%f"
                    ,"or=%f  br=%f  ir=%f"
                    ,"%s"
                    ,"%s"
                    ,"%s"
                    ,"%s"
                    ,""])
                   totalw totalh (show $ sDim space)
                   (fromTh totpadx) (fromTh oth) (fromTh bth) (fromTh ith)
                   (fromR or) (fromR br) (fromR ir)
                   (ppShow obez) (ppShow $ ofeats !! 7)
                   (ppShow bord) (ppShow $ bfeats !! 7)
            --(bc, oc) = (r, y) -- XXX/GHC: an apparent type checker bug
        GIP.layoutSetText      gip text (-1)
        GIPC.showLayout gic gip
        ) :: GRCI.Render () -- XXX/GHC: an apparent type checker bug
        -- ellipsized <- GIP.layoutIsEllipsized gip
        -- (, ellipsized) <$> GIP.layoutGetPixelSize gip

  pixels      <- GRCI.imageSurfaceGetData grcSurface
  cvTexture   <- uploadTexture2DToGPU'''' False False False False $ (stridePxs, totalh, GL_BGRA, pixels)

  let (dx, dy)       = (fromIntegral totalw, fromIntegral (-totalh)) --(fromIntegral reqw, -fromIntegral reqh)
      n              = (1) -- the normal
      position = V.fromList [ LCLin.V3  0 dy 0, LCLin.V3  0  0 0, LCLin.V3 dx  0 0, LCLin.V3  0 dy 0, LCLin.V3 dx  0 0, LCLin.V3 dx dy 0 ]
      normal   = V.fromList [ LCLin.V3  0  0 n, LCLin.V3  0  0 n, LCLin.V3  0  0 n, LCLin.V3  0  0 n, LCLin.V3  0  0 n, LCLin.V3  0  0 n ]
      texcoord = V.fromList [ LCLin.V2  0  1,   LCLin.V2  0  0,   LCLin.V2  1  0,   LCLin.V2  0  1,   LCLin.V2  1  0,   LCLin.V2  1  1 ]
      cvMesh   = LambdaCube.Mesh.Mesh { mPrimitive  = P_Triangles
                                      , mAttributes = Map.fromList [ ("position",  A_V3F position)
                                                                   , ("normal",    A_V3F normal)
                                                                   , ("diffuseUV", A_V2F texcoord) ] }
  cvGPUMesh <- GL.uploadMeshToGPU cvMesh
  cvGPU <- GL.addMeshToObjectArray storage objstream [SB.unpack mtlUniform, "viewProj"] cvGPUMesh
  GL.updateObjectUniforms cvGPU $ do
    mtlUniform GL.@= return cvTexture

  GL.uniformFTexture2D mtlUniform (GL.uniformSetter storage) cvTexture

  pure Canvas { cvGPU      = cvGPU
              , cvTexture  = cvTexture
              , cvMatSlot  = mtlUniform
              , cvGRCr     = grc
              , cvGRCSurf  = grcSurface
              , cvGICtx    = gic
              , cvGIPLay   = gip }

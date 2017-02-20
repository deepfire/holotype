{-# LANGUAGE LambdaCase, ViewPatterns, RecordWildCards, TupleSections, PackageImports, OverloadedStrings #-}
{-# LANGUAGE BangPatterns, MultiWayIf #-}
{-# LANGUAGE ExplicitForAll, FlexibleContexts, NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving, StandaloneDeriving #-}
{-# LANGUAGE DataKinds, GADTs, TypeFamilies, TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Engine
  ( loadPK3
  , createLoadingScreen
  , drawLoadingScreen
  , engineInit
  , setupStorage
  , updateRenderInput
  -- temp
  , loadQuake3Graphics
  , compileQuake3Graphics
  , compileQuake3GraphicsCached
  , getSpawnPoints
  , getBSP
  , getModelIndexFromBrushIndex
  , getTeleportFun
  , getMusicFile
  --
  , EngineContent, EngineGraphics, CanvasGPU
  ) where

import GHC.Stack
import GHC.TypeLits

import Control.Monad
import Control.Lens
import Data.Char
import Data.Digest.CRC32
import Data.HashSet (HashSet)
import qualified Data.ByteString.Char8 as SB8
import qualified Data.HashSet as HashSet
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import Data.List (delete, isPrefixOf, partition, isInfixOf, elemIndex)
import Data.Maybe
import Data.Vect
import Data.Set (Set)
import Data.Map (Map)
import Data.Vector (Vector)
import qualified Data.Text as DT
import Foreign
import System.FilePath
import System.Directory
import qualified System.IO.Unsafe as UN

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.HashSet as HashSet

import qualified Data.ByteString.Lazy as LB
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as SB

import Data.Binary (encodeFile,decodeFile)

import Data.MonoTraversable

import Linear
import qualified Linear as L
import Numeric.Extra

import Text.Printf (printf)
import Text.Show.Pretty (ppShow)

import qualified Graphics.Rendering.Cairo          as GRC -- (moveTo, Render, Format(..))
import qualified Graphics.Rendering.Cairo.Internal as GRC (Render(..), create, imageSurfaceCreate)
import qualified Graphics.Rendering.Cairo.Types    as GRC -- (Cairo(..))
import qualified Graphics.Rendering.Cairo.Internal as GRCI-- (Cairo(..))

import qualified Data.GI.Base                      as GI
import qualified GI.Cairo                          as GIC
import qualified GI.Cairo.Structs.Context          as GIC -- (Context(..))
import qualified GI.Pango                          as GIP
       -- (AttrList, GL.Attribute, attrListInsert, attrListNew, Layout,
       --  layoutSetWidth, layoutNew, layoutSetAttributes, layoutSetText,
       --  layoutSetWrap)
import qualified GI.PangoCairo.Interfaces.FontMap  as GIPC -- (fontMapGetDefault)
import qualified GI.PangoCairo.Functions           as GIPC -- (createLayout, showLayout)

import Foreign.ForeignPtr (withForeignPtr)
import Control.Monad.Trans.Reader (ReaderT(..))
import Foreign.Ptr (castPtr)
import Foreign.C.Types (CUChar)
import Control.Monad.IO.Class (MonadIO(..))

import Codec.Picture
import Codec.Picture.Types
import Data.Vector.Mutable

import qualified LambdaCube.GL     as GL
import qualified LambdaCube.Linear as LCLin
import LambdaCube.Mesh
import LambdaCube.GL.Mesh
import LambdaCube.GL.Type (TextureData(..))
import Graphics.GL.Core33

import GameEngine.Data.BSP
import GameEngine.Data.MD3
import GameEngine.Data.GameCharacter
import GameEngine.Data.Material hiding (Vec3,Entity)
import qualified GameEngine.Data.Material as GE
import GameEngine.Content
import GameEngine.Graphics.Culling
import GameEngine.Graphics.Frustum
import GameEngine.Graphics.Storage
import GameEngine.Graphics.BSP
import GameEngine.Graphics.MD3
import GameEngine.Loader.MD3
import GameEngine.Loader.BSP
import GameEngine.Loader.Zip
import GameEngine.Utils
import qualified GameEngine.Data.MD3 as MD3
import qualified GameEngine.Loader.Entity as E

import Entity
import Content

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
        z = L.zero

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
  pat <- liftIO $ UN.unsafeInterleaveIO $ coPatternGradLinear cs (fromJust cStart) ce (fromJust cEnd)
  if | cStart /= cEnd    -> GRC.setSource pat
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


----
---- Space partitioning
----
data Space (pinned :: Bool) (depth :: Nat) a where
  End :: Num a =>                 Space p  0    a
  Spc :: Num a =>
    { sWrap  :: !(Wrap p   a)
    , sInner ::  Space p m a } -> Space p (n+1) a

-- | Compute the total allocation for an un-pinned 'Space'.
--   Complexity: O(depth).
sDim :: Space False d a -> Di a
sDim s@Spc{..} = wDim sWrap ^+^ sDim sInner
sDim   End     = L.zero

-- | Compute the SE point for an un-pinned 'Space', given its NW point.
--   Complexity: O(depth).
sSE  :: Space False d a -> Po a -> Po a
sSE  s@Spc{..} lt = Po $ fromPo lt ^+^ fromDi (sDim s) --  ^-^ V2 1 1

-- | Pin space to the @lt
sPin :: Num a => Space False n a -> Po a -> Space True n a
sPin space lt = loop space L.zero rb
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


type EngineContent =
  ( BSPLevel
  , Map ByteString MD3.MD3Model
  , [(Proj4, (Map String String, String))]
  , [[(Proj4, (Map String String, [Char]))]]
  , [Character]
  , Map String CommonAttrs
  , [Vec3]
  , V.Vector Int
  , ([Entity], Map ByteString Entity)
  , Maybe String
  )

type EngineGraphics a =
  ( GL.GLStorage
  , [(Proj4, MD3Instance)]
  , [Character]
  , [(Proj4, (MD3.MD3Model, MD3Instance), (MD3.MD3Model, MD3Instance),(MD3.MD3Model, MD3Instance))]
  , V.Vector [GL.Object]
  , BSPLevel
  , MD3Instance, Canvas a
  , [(Float, GL.SetterFun TextureData, V.Vector TextureData)]
  )

loremIpsum = "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna\ \ aliqua. Ut enim ad minim veniam, quis nostrud exercitation\ \ ullamco laboris nisi ut aliquip ex ea commodo consequat.\ \ Duis aute irure dolor in reprehenderit in voluptate\ \ velit esse cillum dolore eu fugiat nulla pariatur.\ \ Excepteur sint occaecat cupidatat non proident, sunt in culpa\ \ qui officia deserunt mollit anim id est laborum."

cvstream   = "canvas"
cvmaterial = "canvas"

createMoodRenderInfo :: Map FilePath CommonAttrs -> HashSet FilePath -> HashSet FilePath -> (GL.PipelineSchema, Map FilePath CommonAttrs)
createMoodRenderInfo shMap' levelMaterials modelMaterials = (inputSchema,shMapTexSlot) where
  mkShader hasLightmap n = case Map.lookup n shMap' of
    Just s -> (n,s)
    Nothing -> let alias = dropExtension n in case Map.lookup alias shMap' of
      Just s -> (alias,s)
      Nothing -> (n,imageShader hasLightmap n)

  imageShader hasLightmap txName = defaultCommonAttrs {caStages = sa:if hasLightmap then saLM:[] else []} where
    sa = defaultStageAttrs
        { saTexture     = ST_Map txName
        , saBlend       = Nothing
        , saTCGen       = TG_Base
        , saDepthWrite  = True
        , saRGBGen      = RGB_IdentityLighting
        }
    saLM = defaultStageAttrs
        { saTexture = ST_Lightmap
        , saBlend   = Just (B_DstColor,B_Zero)
        , saTCGen   = TG_Lightmap
        , saRGBGen  = RGB_IdentityLighting
        }

  shMap = Map.fromList [mkShader True n | n <- HashSet.toList levelMaterials]  `Map.union`
          Map.fromList [mkShader False n | n <- HashSet.toList modelMaterials]
          `Map.union`
          Map.fromList [(cvmaterial
                        ,defaultCommonAttrs
                         { caSort   = 10.0
                         , caStages = [defaultStageAttrs
                                        { saTexture     = ST_ClampMap cvmaterial
                                        , saBlend       = Just ( B_SrcAlpha , B_OneMinusSrcAlpha )
                                        , saTCGen       = TG_Base
                                        , saDepthWrite  = True
                                        , saRGBGen      = RGB_IdentityLighting
                                        }]})]

  shMapTexSlot = mangleCA <$> shMap
    where
      mangleStageTex stageTex = "Tex_" ++ show (crc32 $ SB.pack $ show stageTex)
      mangleCA ca = ca {caStages = mangleSA <$> caStages ca}
      mangleSA sa = sa {saTextureUniform = mangleStageTex sa}

  textureUniforms = Set.toList . Set.fromList . concat . map name . concat . map caStages $ Map.elems shMapTexSlot
    where
      name s = [saTextureUniform s]
      {-
      name s = case saTexture s of
        ST_Map n        -> [n]
        ST_ClampMap n   -> [n]
        ST_AnimMap _ n  -> [head n]
        ST_Lightmap     -> ["LightMap"]
        ST_WhiteImage   -> []

      -}
  debugSlotSchema =
    GL.ObjectArraySchema GL.Triangles $ Map.fromList
      [ ("position",    GL.Attribute_V3F)
      , ("color",       GL.Attribute_V4F)
      ]
  canvasSchema =
    GL.ObjectArraySchema GL.Triangles $ Map.fromList
      [ ("diffuseUV",   GL.Attribute_V2F)
      , ("normal",      GL.Attribute_V3F)
      , ("position",    GL.Attribute_V3F)
      ]
  quake3SlotSchema =
    GL.ObjectArraySchema GL.Triangles $ Map.fromList
      [ ("color",       GL.Attribute_V4F)
      , ("diffuseUV",   GL.Attribute_V2F)
      , ("normal",      GL.Attribute_V3F)
      , ("position",    GL.Attribute_V3F)
      , ("lightmapUV",  GL.Attribute_V2F)
      ]
  quakeObjectStreamNames = delete cvstream $ Map.keys shMap
  inputSchema = {-TODO-}
    GL.PipelineSchema
    { objectArrays = Map.fromList $
      ("CollisionShape",debugSlotSchema)
      : (cvstream, canvasSchema)
      : zip ("LightMapOnly":"missing shader": quakeObjectStreamNames) (repeat quake3SlotSchema)
    , uniforms = Map.fromList $ [ ("viewProj",      GL.M44F)
                                , ("worldMat",      GL.M44F)
                                , ("viewMat",       GL.M44F)
                                , ("orientation",   GL.M44F)
                                , ("viewOrigin",    GL.V3F)
                                , ("entityRGB",     GL.V3F)
                                , ("entityAlpha",   GL.Float)
                                , ("identityLight", GL.Float)
                                , ("time",          GL.Float)
                                , ("LightMap",      GL.FTexture2D)
                                , ("Noise",                GL.FTexture2D)
                                , ("SinTable",             GL.FTexture2D)
                                , ("SquareTable",          GL.FTexture2D)
                                , ("SawToothTable",        GL.FTexture2D)
                                , ("InverseSawToothTable", GL.FTexture2D)
                                , ("TriangleTable",        GL.FTexture2D)
                                , ("origin",    GL.V3F)
                                ] ++ zip textureUniforms (repeat GL.FTexture2D)
    }

-- TODO
engineInit :: Map String Entry -> FilePath -> IO (GL.PipelineSchema, EngineContent)
engineInit pk3Data fullBSPName = do
    let bspName = takeBaseName fullBSPName
        bspEntry = case Map.lookup fullBSPName pk3Data of
            Nothing -> error "You need to put pk3 file into your current directory"
            Just bspd -> bspd

    putStrLn $ "loading: " ++ show bspName
    -- load bsp data
    bsp <- readBSP . LB.fromStrict <$> readEntry bspEntry

    createDirectoryIfMissing True lc_q3_cache -- create cache

    SB.writeFile (lc_q3_cache </> bspName ++ ".entities") $ blEntities bsp

    -- extract spawn points
    let ents = case E.parseEntities bspName $ blEntities bsp of
            Left err -> error err
            Right x -> x
        spawnPoint E.EntityData{..}
          | classname `elem` [ "info_player_deathmatch"
                             , "info_player_start"
                             , "team_CTF_bluespawn"
                             , "team_CTF_redspawn"
                             , "team_CTF_blueplayer"
                             , "team_CTF_redplayer"
                             ] = [origin]
          | otherwise = []
        spawnPoints = concatMap spawnPoint ents
        p0 = head spawnPoints
        teleportData = loadTeleports ents
        music = (head . words) <$> (E.music $ head ents)

    -- MD3 related code
    (characterSkinMaterials,characterObjs,characters) <- readCharacters pk3Data p0
    (md3Materials,md3Map,md3Objs) <- readMD3Objects characterObjs ents pk3Data
    printf "---------\nMD3 materials:\n%s\n" $ show md3Materials
    printf "---------\nBSP materials:\n%s\n" $ show $ map (SB.unpack . GameEngine.Data.BSP.shName) $ V.toList $ blShaders bsp
    --putStrLn $ "level materials"
    --mapM_ SB.putStrLn $ map shName $ V.toList $ blShaders bsp
    shMap <- do
      let q3shader_cache = "foo.shc"
      hasShaderCache <- doesFileExist q3shader_cache
      case hasShaderCache of
        True -> putStrLn "load shader cache" >> decodeFile q3shader_cache
        False -> do
                  putStrLn "create shader cache"
                  sm <- loadShaderMap pk3Data
                  encodeFile q3shader_cache sm
                  return sm
    let
        maxMaterial = 20 -- TODO: remove if we will have fast reducer
        shNames = Set.fromList $ {-Prelude.take maxMaterial $ -}selectedMaterials ++ ignoredMaterials
        allShName = map GameEngine.Data.BSP.shName $ V.toList $ blShaders bsp
        (selectedMaterials,ignoredMaterials) = partition (\n -> or $ [SB.isInfixOf k n | k <- ["floor","wall","door","trim","block"]]) allShName

    let levelMaterials = HashSet.fromList . Set.toList $ Set.map SB.unpack shNames
        modelMaterials = HashSet.fromList . Set.toList $ Set.map SB.unpack (md3Materials `Set.union` characterSkinMaterials)
        (inputSchema,shMapTexSlot) = createMoodRenderInfo shMap levelMaterials modelMaterials
    --putStrLn $ "all materials:  " ++ show (Map.size shMap')
    --putStrLn $ "used materials: " ++ show (Map.size shMap)
    --putStrLn $ "texture uniforms: \n" ++ ppShow textureUniforms
    --putStrLn $ "used materials: " ++ show (Map.size shMapTexSlot)
    --putStrLn $ "ignored materials: " ++ show (length ignoredMaterials)
    writeSampleMaterial shMapTexSlot
    --SB.putStrLn $ SB.unlines ignoredMaterials

    let brushModelMapping = V.replicate (V.length $ blBrushes bsp) (-1) V.//
          (concat $ V.toList $ V.imap (\i Model{..} -> [(n,i) | n <- [mdFirstBrush..mdFirstBrush+mdNumBrushes-1]]) (blModels bsp))
    putStrLn $ "bsp model count: " ++ show (V.length $ blModels bsp)
    -- print brushModelMapping
    -- print teleportData
    return (inputSchema,(bsp,md3Map,md3Objs,characterObjs,characters,shMapTexSlot,spawnPoints,brushModelMapping,teleportData,music))

getMusicFile (bsp,md3Map,md3Objs,characterObjs,characters,shMapTexSlot,spawnPoints,brushModelMapping,teleportData,music) = music
getModelIndexFromBrushIndex (bsp,md3Map,md3Objs,characterObjs,characters,shMapTexSlot,spawnPoints,brushModelMapping,teleportData,music) brushIndex = brushModelMapping V.! brushIndex
getBSP (bsp,md3Map,md3Objs,characterObjs,characters,shMapTexSlot,spawnPoints,brushModelMapping,teleportData,music) = bsp
getSpawnPoints (bsp,md3Map,md3Objs,characterObjs,characters,shMapTexSlot,spawnPoints,brushModelMapping,teleportData,music) = spawnPoints
getTeleportFun levelData@(bsp,md3Map,md3Objs,characterObjs,characters,shMapTexSlot,spawnPoints,brushModelMapping,(teleport,teleportTarget),music) brushIndex p =
  let models = map (getModelIndexFromBrushIndex levelData) brushIndex
      hitModels = [tp | TriggerTeleport target model <- teleport, model `elem` models, TargetPosition _ tp <- maybeToList $ Map.lookup target teleportTarget]
  --in head $ trace (show ("hitModels",hitModels,models)) hitModels ++ [p]
  in head $ hitModels ++ [p]

--- What do we lose?
-- 1. buffer per surface vs. per model
-- 2. frame lossage is ok, since we don't intend to have frames
-- 3. shader lossage is ok, since, well, no common shaders, yay, ok, no?
--
-- Frame {frMins = Vec3 (-10.765625) (-10.921875) (-5.84375), frMaxs = Vec3 10.828125 10.671875 15.75, frOrigin = Vec3 0.0 0.0 0.0, frRadius = 22.01359, frName = "(from ase)"}
uploadMD3Surface :: MD3.Surface -> MD3.Frame -> IO GPUMD3S
uploadMD3Surface surface@MD3.Surface{..} frame = do
  let cvtSurface :: MD3.Surface -> (GL.Array, GL.Array, V.Vector (GL.Array, GL.Array))
      cvtSurface MD3.Surface{..} =
        ( GL.Array GL.ArrWord32 (SV.length srTriangles) (withV srTriangles)
        , GL.Array GL.ArrFloat (2 * SV.length srTexCoords) (withV srTexCoords)
        , V.map cvtPosNorm srXyzNormal
        )
        where
          withV a f = SV.unsafeWith a (\p -> f $ castPtr p)
          cvtPosNorm (p,n) = (f p, f n) where f sv = GL.Array GL.ArrFloat (3 * SV.length sv) $ withV sv

      addSurface sf (il,tl,pl,nl,pnl) = (i:il,t:tl,p:pl,n:nl,pn:pnl) where
        (i,t,pn) = cvtSurface sf
        (p,n)    = V.head pn

      (il,tl,pl,nl,pnl) = addSurface surface ([],[],[],[],[])

  buffer <- GL.compileBuffer (concat [il,tl,pl,nl])

  let nMdFrames   = 1
      numSurfaces = 1
      surfaceData idx MD3.Surface{..} = (index,attributes) where
        index = GL.IndexStream buffer idx 0 (SV.length srTriangles)
        countV = SV.length srTexCoords
        attributes = Map.fromList $
          [ ("diffuseUV",   GL.Stream GL.Attribute_V2F buffer (1 * numSurfaces + idx) 0 countV)
          , ("position",    GL.Stream GL.Attribute_V3F buffer (2 * numSurfaces + idx) 0 countV)
          , ("normal",      GL.Stream GL.Attribute_V3F buffer (3 * numSurfaces + idx) 0 countV)
          , ("color",       GL.ConstV4F (GL.V4 1 1 1 1))
          , ("lightmapUV",  GL.ConstV2F (GL.V2 0 0))
          ]
      frames :: Data.Vector.Vector [(Int, GL.Array)]
      frames = foldr addSurfaceFrames emptyFrame $ zip [0..] pnl where
        emptyFrame = V.replicate nMdFrames []
        addSurfaceFrames (idx,pn) f = V.zipWith (\l (p,n) -> (2 * numSurfaces + idx,p):(3 * numSurfaces + idx,n):l) f pn

  return $ GPUMD3S
    { gpumd3sBuffer    = buffer
    , gpumd3sStreams   = surfaceData 0 surface
    , gpumd3sFrames    = frames
    , gpumd3sShaders   = HashSet.fromList $ map (SB8.unpack . MD3.shName) $ V.toList srShaders
    , gpumd3sFrame     = frame
    , gpumd3sSurface   = surface
    }

data GPUMD3S
  = GPUMD3S
  { gpumd3sBuffer    :: GL.Buffer
  , gpumd3sStreams   :: (GL.IndexStream GL.Buffer,Map String (GL.Stream GL.Buffer)) -- index stream, attribute streams
  , gpumd3sFrames    :: V.Vector [(Int, GL.Array)]
  , gpumd3sShaders   :: HashSet String
  , gpumd3sSurface   :: MD3.Surface
  , gpumd3sFrame     :: MD3.Frame
  }

data MD3SInstance
  = MD3SInstance
  { md3sinstanceObject  :: [GL.Object]
  , md3sinstanceBuffer  :: GL.Buffer
  , md3sinstanceFrames  :: V.Vector [(Int, GL.Array)]
  , md3sinstanceSurface :: MD3.Surface
  }

type MD3Skin = Map String String

addGPUMD3Surface :: GL.GLStorage -> GPUMD3S -> MD3Skin -> [String] -> IO MD3SInstance
addGPUMD3Surface r GPUMD3S{..} skin unis = do
    let sf@MD3.Surface{..} = gpumd3sSurface
    let (index, attrs) = gpumd3sStreams
    objs <- do
        let materialName s = case Map.lookup (SB8.unpack $ srName) skin of
              Nothing -> SB8.unpack $ MD3.shName s
              Just a  -> a
        objList <- concat <$> forM (V.toList $ srShaders) (\s -> do
          printf "addObjectWithmaterial:\n  %s\n  %s\n\n" (show $ materialName s) (show unis) -- (ppShow attrs)
          a <- addObjectWithMaterial r (materialName s) GL.TriangleList (Just index) attrs $ setNub $ "worldMat":unis
          b <- GL.addObject          r "LightMapOnly"   GL.TriangleList (Just index) attrs $ setNub $ "worldMat":unis
          return [a,b])

        -- add collision geometry
        let Frame{..} = gpumd3sFrame
        collisionObjs <- do
            sphereObj <- uploadMeshToGPU (sphere (GL.V4 1 0 0 1) 4 frRadius) >>= addMeshToObjectArray r "CollisionShape" (setNub $ ["worldMat","origin"] ++ unis)
            boxObj <- uploadMeshToGPU (bbox (GL.V4 0 0 1 1) frMins frMaxs) >>= addMeshToObjectArray r "CollisionShape" (setNub $ ["worldMat","origin"] ++ unis)
            --when (frOrigin /= zero) $ putStrLn $ "frOrigin: " ++ show frOrigin
            return [sphereObj,boxObj]

        return $ objList ++ collisionObjs
    -- question: how will be the referred shaders loaded?
    --           general problem: should the gfx network contain all passes (every possible materials)?
    return $ MD3SInstance
        { md3sinstanceObject  = objs
        , md3sinstanceBuffer  = gpumd3sBuffer
        , md3sinstanceFrames  = gpumd3sFrames
        , md3sinstanceSurface = sf
        }

addMD3Surface :: GL.GLStorage -> MD3.Surface -> MD3.Frame -> MD3Skin -> [String] -> IO MD3SInstance
addMD3Surface r surface frame skin unis = do
    gpuMD3 <- uploadMD3Surface surface frame
    addGPUMD3Surface r gpuMD3 skin unis

grcToGIC :: GRC.Cairo -> IO GIC.Context
grcToGIC grc = GIC.Context <$> GI.newManagedPtr (castPtr $ GRC.unCairo grc) (return ())

{-# INLINE doRange #-}
doRange :: Int -> Int -> (Int -> IO ()) -> IO ()
doRange from to action =
  let loop n | n >= to   = return ()
             | otherwise = do action n
                              loop (n+1)
   in loop from

setupStorage :: Map String Entry -> EngineContent -> GL.GLStorage -> IO (EngineGraphics CanvasGPU)
setupStorage pk3Data (bsp,md3Map,md3Objs,characterObjs,characters,shMapTexSlot,_,_,_,_) storage = do
    let slotU           = GL.uniformSetter storage
        entityRGB       = GL.uniformV3F "entityRGB" slotU
        entityAlpha     = GL.uniformFloat "entityAlpha" slotU
        identityLight   = GL.uniformFloat "identityLight" slotU
        worldMat        = GL.uniformM44F "worldMat" slotU
        overbrightBits  = 0
        idmtx = GL.V4 (GL.V4 1 0 0 0) (GL.V4 0 1 0 0) (GL.V4 0 0 1 0) (GL.V4 0 0 0 1)
    worldMat idmtx
    entityRGB $ GL.V3 1 1 1
    entityAlpha 1
    identityLight $ 1 / (2 ^ overbrightBits)
    initTableTextures >>= setupTableTextures slotU

    -- default texture
    let redBitmap x y = let v = if (x+y) `mod` 2 == 0 then 255 else 0 in PixelRGB8 v v 0
    defaultTexture <- GL.uploadTexture2DToGPU' False False False False $ ImageRGB8 $ generateImage redBitmap 2 2

    canvas <- renderCanvasInitial storage shMapTexSlot
              (CanvasRequest (sGrowS 2 $ sGrowS 5 $ sGrowS 2 $ sGrowS 16 $ sArea $ fromIntegral . ceiling <$> di2goldX 256)
                loremIpsum (coGray 0.8 1) (coOpaq 0.1 0.1 0.5) (coGray 1 1) (coGray 0.5 1) (coGray 0.1 0.5) terminusFontDesc)

    putStrLn "loading textures:"
    -- load textures
    animTex <- fmap concat $ forM (Set.toList $ Set.fromList $ concatMap (\(shName,sh) -> [(shName,saTexture sa,saTextureUniform sa,caNoMipMaps sh) | sa <- caStages sh]) $ Map.toList shMapTexSlot) $
      \(shName,stageTex,texSlotName,noMip) -> do -- texSlotName :: Approx "Tex_3913048198"
        let texSetter = GL.uniformFTexture2D (SB.pack texSlotName) slotU
            setTex isClamped img = if img == cvMaterial canvas -- don't touch our canvas..
                                   then return []
                                   else (texSetter =<< loadQ3Texture (not noMip) isClamped defaultTexture pk3Data shName img) >> return []
        case stageTex of
            ST_Map img          -> setTex False img
            ST_ClampMap img     -> setTex True  img
            ST_AnimMap freq imgs   -> do
                txList <- mapM (loadQ3Texture (not noMip) False defaultTexture pk3Data shName) imgs
                let txVector = V.fromList txList
                return [(fromIntegral (V.length txVector) / freq,texSetter,txVector)]
            _ -> return []

    putStrLn "add bsp to storage"
    surfaceObjs <- bspinstanceSurfaces <$> addBSP (Map.keysSet shMapTexSlot) storage bsp

    -- add entities
    let addMD3Obj (mat,(skin,name)) = case Map.lookup (SB.pack name) md3Map of
          Nothing -> return []
          Just md3 -> do
                    putStrLn ("add model: " ++ name)
                    lcmd3 <- addMD3 storage md3 skin ["worldMat"]
                    return [(mat,lcmd3)]

    lcMD3Objs <- pure [] --concat <$> forM md3Objs addMD3Obj

    -- weapon
    chunk <- loadMD3 "./chunk.md3"
    let weapon_model = chunk -- (fromJust $ Map.lookup (SB.pack handWeapon) md3Map)
    -- lcMD3Weapon <- addMD3 storage weapon_model mempty ["worldMat","viewProj"]
    lcMD3Weapon <- addMD3 storage (fromJust $ Map.lookup (SB.pack handWeapon) md3Map) mempty ["worldMat","viewProj"]

    -- add characters
    lcCharacterObjs <- pure mempty -- forM characterObjs
      (\[(mat,(hSkin,hName)),(_,(uSkin,uName)),(_,(lSkin,lName))] -> do
        let Just hMD3 = Map.lookup (SB.pack hName) md3Map
            Just uMD3 = Map.lookup (SB.pack uName) md3Map
            Just lMD3 = Map.lookup (SB.pack lName) md3Map
        hLC <- addMD3 storage hMD3 hSkin ["worldMat"]
        uLC <- addMD3 storage uMD3 uSkin ["worldMat"]
        lLC <- addMD3 storage lMD3 lSkin ["worldMat"]
        return (mat,(hMD3,hLC),(uMD3,uLC),(lMD3,lLC))
      )
    return (storage,lcMD3Objs,characters,lcCharacterObjs,surfaceObjs,bsp,lcMD3Weapon,canvas,animTex)

uploadTexture2DToGPU''' :: Bool -> (Int, Int, Ptr Foreign.C.Types.CUChar) -> IO TextureData
uploadTexture2DToGPU''' isMip (w, h, ptr) = do
    glPixelStorei GL_UNPACK_ALIGNMENT 1
    to <- alloca $! \pto -> glGenTextures 1 pto >> peek pto
    glBindTexture GL_TEXTURE_2D to
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S $ fromIntegral GL_REPEAT
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T $ fromIntegral GL_REPEAT
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER $ fromIntegral GL_NEAREST
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER $ fromIntegral GL_NEAREST
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_BASE_LEVEL 0
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAX_LEVEL $ fromIntegral 0
    let internalFormat  = fromIntegral $ GL_RGBA8
        dataFormat      = fromIntegral $ GL_RGBA8
    glTexImage2D GL_TEXTURE_2D 0 internalFormat (fromIntegral w) (fromIntegral h) 0 dataFormat GL_UNSIGNED_BYTE $ castPtr ptr
    when isMip $ glGenerateMipmap GL_TEXTURE_2D
    return $ TextureData to

uploadTexture2DToGPU'''' :: Bool -> Bool -> Bool -> Bool -> (Int, Int, GLenum, Ptr Foreign.C.Types.CUChar) -> IO TextureData
uploadTexture2DToGPU'''' isFiltered isSRGB isMip isClamped (w, h, format, ptr) = do
    glPixelStorei GL_UNPACK_ALIGNMENT 1
    to <- alloca $! \pto -> glGenTextures 1 pto >> peek pto
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
    glTexImage2D GL_TEXTURE_2D 0 internalFormat (fromIntegral w) (fromIntegral h) 0 format GL_UNSIGNED_BYTE $ castPtr ptr
    when isMip $ glGenerateMipmap GL_TEXTURE_2D
    return $ TextureData to

uploadTexture2DToGPU'' :: Bool -> Bool -> TextureData -> DynamicImage -> IO ()
uploadTexture2DToGPU'' isSRGB isMip (TextureData to) bitmap' = do
    let bitmap = case bitmap' of
          ImageRGB8 i@(Image w h _)   -> bitmap'
          ImageRGBA8 i@(Image w h _)  -> bitmap'
          ImageYCbCr8 i@(Image w h _) -> error "unsupported texture pixel format!" -- ImageRGB8 $ convertImage i
          di -> ImageRGBA8 $ convertRGBA8 di
        withBitmap (ImageRGB8 (Image w h v)) f = SV.unsafeWith v $ f (w,h) 3 0
        withBitmap (ImageRGBA8 (Image w h v)) f = SV.unsafeWith v $ f (w,h) 4 0
        withBitmap _ _ = error "unsupported image type :("
    glBindTexture GL_TEXTURE_2D to
    withBitmap bitmap $ \(w,h) nchn 0 ptr -> do
        let internalFormat  = fromIntegral $ if isSRGB then (if nchn == 3 then GL_SRGB8 else GL_SRGB8_ALPHA8) else (if nchn == 3 then GL_RGB8 else GL_RGBA8)
            dataFormat      = fromIntegral $ case nchn of
                3   -> GL_RGB
                4   -> GL_RGBA
                _   -> error "unsupported texture format!"
        glTexImage2D GL_TEXTURE_2D 0 internalFormat (fromIntegral w) (fromIntegral h) 0 dataFormat GL_UNSIGNED_BYTE $ castPtr ptr
    when isMip $ glGenerateMipmap GL_TEXTURE_2D
    return ()

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

data Canvas a where
  Canvas ::
    { cvGPU      :: a
    , cvMaterial :: String
    , cvMatSlot  :: GL.GLUniformName
    , cvTexture  :: TextureData
    , cvGRCr     :: GRC.Cairo
    , cvGRCSurf  :: GRC.Surface
    , cvGICtx    :: GIC.Context
    , cvGIPLay   :: GIP.Layout
    } -> Canvas a

-- grcStrokeCanvas :: GRC.Context -> CanvasRequest ->

renderCanvasInitial :: GL.GLStorage -> Map String CommonAttrs -> CanvasRequest -> IO (Canvas CanvasGPU)
renderCanvasInitial storage shMapTexSlot
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
            -- roundedBoxArcs :: Wrap True Double -> Double -> [Orient]
            --                -> GRCI.Render (Po Double, Po Double, Po Double, Po Double)
            -- roundedBoxArcs wrap@PWrap{..} r orients = do
            --   let (lti, rbi)           = pwPosIn'2 wrap
            --       (plt, prt, prb, plb) = poNWSERectArcCentersCW lti rbi r
            --       (alt, art, arb, alb) = aRectAnglesNWCW
            --   when drawNW $ poArc plt r alt
            --   when drawRT $ poArc prt r art
            --   when drawSE $ poArc prb r arb
            --   when drawLB $ poArc plb r alb
            --   pure (plt, prt, prb, plb)
            ths@[oth, bth, ith, pth]
                          = fmap (Th . wL) [obez, bord, ibez, pad]
            totpadx       = sum ths
            or            = R . fromTh $ totpadx - oth/2
            br            = or - (R . fromTh $ (oth+bth)*0.6)
            ir            = br - (R . fromTh $ (bth+ith)/2)
        -- coSetSourceColor (co 0 1 0 1) >>
        --   GRC.paint
        -- background & border arcs
        let bfeats@[n, ne, _, se, _, sw, _, nw] = wrapRoundedRectFeatures bord br bth
        GRC.newPath >> thLineSet bth
        forM_ [n, ne, se, sw, nw] $ executeFeature Nothing Nothing
        coSetSourceColor bgColor >>
          GRC.fillPreserve
        coSetSourceColor bordColor >>
          GRC.stroke
        -- (dCorn nw $ co 1 0 0 1) >> (dCorn ne $ co 1 0 0 1) >> (dCorn sw $ co 1 0 0 1) >> (dCorn se $ co 1 0 0 1)

        -- border bezels: light outer TL, dark outer SE
        thLineSet oth
        let ofeats@[n, ne, e, se, s, sw, w, nw] = wrapRoundedRectFeatures obez or oth
        --
        GRC.newPath
        (coSetSourceColor $ lBezColor) >>
          (forM_ [w, nw, n] $ executeFeature Nothing Nothing) >> GRC.stroke
        (coSetSourceColor $ dBezColor) >>
          (forM_ [e, se, s] $ executeFeature Nothing Nothing) >> GRC.stroke
        (coSetSourceColor $ bordColor) >>
          GRC.newPath >> (executeFeature (Just dBezColor) (Just lBezColor) sw) >> GRC.stroke >>
          GRC.newPath >> (executeFeature (Just lBezColor) (Just dBezColor) ne) >> GRC.stroke
        -- (dCorn ne $ co 0 1 0 1) >> (dCorn sw $ co 0 1 0 1) >> (dCorn nw $ co 0 1 0 1) >> (dCorn se $ co 0 1 0 1)

        -- border bezels: dark inner TL, light inner SE
        thLineSet ith
        let [n, ne, e, se, s, sw, w, nw] = wrapRoundedRectFeatures ibez ir ith
        --
        GRC.newPath
        (coSetSourceColor $ dBezColor) >>
          (forM_ [w, nw, n] $ executeFeature Nothing Nothing) >> GRC.stroke
        (coSetSourceColor $ lBezColor) >>
          (forM_ [e, se, s] $ executeFeature Nothing Nothing) >> GRC.stroke
        (coSetSourceColor $ bordColor) >>
          GRC.newPath >> (executeFeature (Just lBezColor) (Just dBezColor) sw) >> GRC.stroke >>
          GRC.newPath >> (executeFeature (Just dBezColor) (Just lBezColor) ne) >> GRC.stroke
        -- (dCorn ne $ co 0 0 0 1) >> (dCorn sw $ co 0 0 0 1) >> (dCorn nw $ co 0 0 0 1) >> (dCorn se $ co 0 0 0 1)

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
                    -- ,"%s   %s"
                    -- ,"%s   %s"
                    -- ,"or=%f  oth=%f"
                    -- ,"%s"
                    -- ,"%s   %s"
                    -- ,"%s   %s"
                    -- ,"br=%f  bth=%f"
                    ,""])
                   totalw totalh (show $ sDim space)
                   (fromTh totpadx) (fromTh oth) (fromTh bth) (fromTh ith)
                   (fromR or) (fromR br) (fromR ir)
                   -- (ppShow bfeats)
                   (ppShow obez) (ppShow $ ofeats !! 7)
                   -- (show olt) (show ort) (show olb) (show orb)
                   -- (fromR or) oth
                   (ppShow bord) (ppShow $ bfeats !! 7)
                   -- (show blt) (show brt) (show blb) (show brb)
                   -- (fromR br) bth
                   --(show $ orb ^+^ (V2 or or)) or
            r = V4 1 0 0 1; y = V4 1 1 0 1; b = V4 0 0 1 1
            --r = V4 1 0 0 1; y = V4 1 1 0 1; b = V4 0 0 1 1
            --(bc, oc) = (r, y) -- XXX/GHC: an apparent type checker bug
        GIP.layoutSetText      gip text (-1)
        GIPC.showLayout gic gip
        -- d blt borColor; d olt r
        -- d brb borColor; d orb r
        -- d (pwNWp bord) b; d (pwNWp obez) y;
        -- d (pwSEp bord) b; d (pwSEp obez) y;
        -- dpx (V2 0 0) (V3 0 0 1)
        ) :: GRCI.Render () -- XXX/GHC: an apparent type checker bug
        -- pure ((0,0),False)
        -- ellipsized <- GIP.layoutIsEllipsized gip
        -- (, ellipsized) <$> GIP.layoutGetPixelSize gip

  pixels      <- GRCI.imageSurfaceGetData grcSurface
  cvTexture   <- uploadTexture2DToGPU'''' False False False False $ (stridePxs, totalh, GL_BGRA, pixels)

  let cvMatSlot  = head $ concat $ concatMap (\(shName,sh) -> [if shName == cvmaterial then [saTextureUniform sa] else [] | sa <- caStages sh]) $ Map.toList shMapTexSlot
      -- widthOfStride = fromIntegral totalw / fromIntegral stridePxs
      (dx, dy)       = (fromIntegral totalw, fromIntegral (-totalh)) --(fromIntegral reqw, -fromIntegral reqh)
      n              = (1) -- the normal
      cvSurface  = MD3.Surface
                   { srName      = "canvasSurface"
                   , srShaders   = V.fromList  [MD3.Shader {shIndex = 0, shName = SB.pack cvmaterial }]
                   , srTriangles = SV.fromList [0,2,1,0,1,3]
                   , srTexCoords = SV.fromList [ Vec2 0 1, Vec2 1 0
                                               , Vec2 0 0, Vec2 1 1]
                   , srXyzNormal = V.singleton ( SV.fromList [Vec3 0 dy 0, Vec3 dx 0 0, Vec3 0 0 0, Vec3 dx dy 0]
                                               , SV.fromList [Vec3 0  0 n, Vec3  0 0 n, Vec3 0 0 n, Vec3  0  0 n] ) }
      cvFrame = MD3.Frame { frMins   = Vec3 0 0 0
                          , frMaxs   = Vec3 1 1 0
                          , frOrigin = Vec3 0 0 0
                          , frRadius = 1.42
                          , frName   = "baseframe" }
  cvGPU <- addMD3Surface storage cvSurface cvFrame mempty ["viewProj"]

  GL.uniformFTexture2D (SB.pack cvMatSlot) (GL.uniformSetter storage) cvTexture

  pure Canvas { cvMaterial = cvmaterial
              , cvGPU      = cvGPU
              , cvTexture  = cvTexture
              , cvMatSlot  = SB.pack cvMatSlot
              , cvGRCr     = grc
              , cvGRCSurf  = grcSurface
              , cvGICtx    = gic
              , cvGIPLay   = gip }

-- type CanvasGPU = GL.Object
type CanvasGPU = MD3SInstance

-- * To screen space conversion matrix.  From hell knows what, yes.
screenM :: Int -> Int -> Mat4
screenM w h =
  Mat4 (Vec4 (1/fw)  0     0 0)
       (Vec4  0     (1/fh) 0 0)
       (Vec4  0      0     1 0)
       (Vec4  0      0     0 0.5) -- where the f..k does that 0.5 factor COMEFROM?
  where (fw, fh) = (fromIntegral w, fromIntegral h)

-- TODO
updateRenderInput :: (EngineGraphics CanvasGPU)
                  -> (Vec3, Vec3, Vec3)
                  -> Int -> Int -> Float -> (Vec3, Vec3) -> IO ()
updateRenderInput (storage, lcMD3Objs, characters, lcCharacterObjs, surfaceObjs, bsp, lcMD3Weapon, canvas@Canvas{..}, animTex)
                  (camPos@(Vec3 cx cy cz), camTarget, camUp)
                  w h time (Vec3 cvx cvy cvz, cvpos) = do
            let slotU = GL.uniformSetter storage

            let matSetter   = GL.uniformM44F "viewProj" slotU
                viewOrigin  = GL.uniformV3F "viewOrigin" slotU
                orientation = GL.uniformM44F "orientation" slotU
                viewMat     = GL.uniformM44F "viewMat" slotU
                timeSetter  = GL.uniformFloat "time" slotU

            let cm = fromProjective (lookat camPos camTarget camUp)                             -- camera orientation transform
                pm = GameEngine.Utils.perspective near far (fovDeg / 180 * pi) (fromIntegral w / fromIntegral h) -- perspective matrix
                sm = fromProjective (scaling $ Vec3 s s s)                                      -- scale matrix
                s  = 0.005
                near = 0.00001/s
                far  = 100/s
                fovDeg = 60
                frust = GameEngine.Graphics.Frustum.frustum fovDeg (fromIntegral w / fromIntegral h) near far camPos camTarget camUp
                cullObject obj p = GL.enableObject obj (pointInFrustum p frust)

            let idM44F = mat4ToM44F $ idmtx -- inverse cm .*. (fromProjective $ translation (Vec3 0 (0) (-30)))
            let --cvrot = fromProjective $ orthogonal $ toOrthoUnsafe $ rotMatrixZ cvz .*. rotMatrixY cvy .*. rotMatrixX cvx
                cvrot = fromProjective $ orthogonal $ toOrthoUnsafe $ rotMatrixZ cvz .*. rotMatrixY 100 .*. rotMatrixX 100
                -- (vpw,vph) = (fromIntegral w/2.0, fromIntegral h/2.0)
                -- om    = Engine.ortho (doubleToFloat (-vpw)) (doubleToFloat vpw) (doubleToFloat (-vph)) (doubleToFloat vph) (-1) 1
                toScreen = screenM w h
                -- (fw, fh) = (fromIntegral w, fromIntegral h)
                -- aspectM44F = GL.V4 (GL.V4 (1/fw)  0     0 0)
                --                    (GL.V4  0     (1/fh) 0 0)
                --                    (GL.V4  0      0     1 0)
                --                    (GL.V4  0      0     0 0.5)
            -- uploadTexture2DToGPU'' False False cvTexture $ ImageRGBA8 $ generateImage gen 256 256
            -- updateUniforms storage $ do
            --   cvMatSlot @= return cvTexture
            -- let obj = cvGPU
            -- GL.uniformM44F "viewProj" (GL.objectUniformSetter obj) $ mat4ToM44F $! toScreen .*. (fromProjective $! Data.Vect.translation cvpos)
            forM_ (md3sinstanceObject cvGPU) $ \obj -> do
              GL.uniformM44F "viewProj" (GL.objectUniformSetter obj) $ mat4ToM44F $! toScreen .*. (fromProjective $! Data.Vect.translation cvpos)
              GL.uniformM44F "worldMat" (GL.objectUniformSetter obj) idM44F

            -- set uniforms
            timeSetter $ time / 1
            viewOrigin $ GL.V3 cx cy cz
            viewMat $ mat4ToM44F cm
            matSetter $! mat4ToM44F $! cm .*. sm .*. pm

            forM_ lcMD3Objs $ \(mat,lcmd3) -> do
              forM_ (md3instanceObject lcmd3) $ \obj -> do
                let m = mat4ToM44F $ fromProjective $ (rotationEuler (Vec3 time 0 0) .*. mat)
                    p = trim . Data.Vect._4 $ fromProjective mat
                GL.uniformM44F "worldMat" (GL.objectUniformSetter obj) m
                cullObject obj p

            forM_ (zip characters lcCharacterObjs) $ \(Character{..},(mat,(hMD3,hLC),(uMD3,uLC),(lMD3,lLC))) -> do
              {-
typedef struct {
	vec3_t		origin;
	vec3_t		axis[3];
} orientation_t;

void _VectorCopy( const vec3_t in, vec3_t out );
void _VectorMA( const vec3_t veca, float scale, const vec3_t vecb, vec3_t vecc );
  = vecc[i] = veca[i] + scale*vecb[i]; i={0,1,2}
void MatrixMultiply(float in1[3][3], float in2[3][3], float out[3][3]);

                -- entity, parent, parentModel, parent_tag_name
                CG_PositionRotatedEntityOnTag( &torso, &legs, ci->legsModel, "tag_torso");
                CG_PositionRotatedEntityOnTag( &head, &torso, ci->torsoModel, "tag_head");
              -}
              -- minBound, maxBound :: a
              -- fromEnum :: a -> Int
              -- toEnum :: Int -> a

              let bothAnim = [BOTH_DEATH1, BOTH_DEAD1, BOTH_DEATH2, BOTH_DEAD2, BOTH_DEATH3, BOTH_DEAD3]
                  anims = V.fromList $
                    [ (TORSO_GESTURE,LEGS_IDLE)
                    , (TORSO_ATTACK,LEGS_IDLE)
                    , (TORSO_ATTACK2,LEGS_IDLE)
                    , (TORSO_DROP,LEGS_IDLE)
                    , (TORSO_RAISE,LEGS_IDLE)
                    , (TORSO_STAND,LEGS_IDLE)
                    , (TORSO_STAND2,LEGS_IDLE)
                    , (TORSO_GETFLAG,LEGS_IDLE)
                    , (TORSO_GUARDBASE,LEGS_IDLE)
                    , (TORSO_PATROL,LEGS_IDLE)
                    , (TORSO_FOLLOWME,LEGS_IDLE)
                    , (TORSO_AFFIRMATIVE,LEGS_IDLE)
                    , (TORSO_NEGATIVE,LEGS_IDLE)

                    , (TORSO_STAND,LEGS_WALKCR)
                    , (TORSO_STAND,LEGS_WALK)
                    , (TORSO_STAND,LEGS_RUN)
                    , (TORSO_STAND,LEGS_BACK)
                    , (TORSO_STAND,LEGS_SWIM)

                    , (TORSO_STAND,LEGS_JUMP)
                    , (TORSO_STAND,LEGS_LAND)

                    , (TORSO_STAND,LEGS_JUMPB)
                    , (TORSO_STAND,LEGS_LANDB)

                    , (TORSO_STAND,LEGS_IDLE)
                    , (TORSO_STAND,LEGS_IDLECR)

                    , (TORSO_STAND,LEGS_TURN)


                    , (TORSO_STAND,LEGS_BACKCR)
                    , (TORSO_STAND,LEGS_BACKWALK)
                    ] ++ zip bothAnim bothAnim

              let t100 = floor $ time / 4
                  (torsoAnimType,legAnimType) = anims V.! (t100 `mod` V.length anims)

              -- torso = upper
              --  transform torso to legs
              --  transform head to torso (and legs)
              let t = floor $ time * 15
                  legAnim = animationMap HashMap.! legAnimType
                  legFrame = aFirstFrame legAnim + t `mod` aNumFrames legAnim
                  torsoAnim = animationMap HashMap.! torsoAnimType
                  torsoFrame = aFirstFrame torsoAnim + t `mod` aNumFrames torsoAnim

                  tagToMat4 MD3.Tag{..} = translateAfter4 tgOrigin (orthogonal . toOrthoUnsafe $ Mat3 tgAxisX tgAxisY tgAxisZ)
                  hMat = (tagToMat4 $ (MD3.mdTags uMD3 V.! torsoFrame) HashMap.! "tag_head") .*. uMat
                  uMat = (tagToMat4 $ (MD3.mdTags lMD3 V.! legFrame) HashMap.! "tag_torso")
                  lMat = one :: Proj4
                  lcMat m = mat4ToM44F . fromProjective $ m .*. rotationEuler (Vec3 (time/5) 0 0) .*. mat
                  p = trim . Data.Vect._4 $ fromProjective mat
                  setup m obj = do
                    GL.uniformM44F "worldMat" (GL.objectUniformSetter obj) $ lcMat m
                    cullObject obj p
              forM_ (md3instanceObject hLC) $ setup hMat
              forM_ (md3instanceObject uLC) $ setup uMat
              forM_ (md3instanceObject lLC) $ setup lMat
              --setMD3Frame hLC frame
              setMD3Frame uLC torsoFrame
              setMD3Frame lLC legFrame

            forM_ animTex $ \(animTime,texSetter,v) -> do
              let (_,i) = properFraction (time / animTime)
                  idx = floor $ i * fromIntegral (V.length v)
              texSetter $ v V.! idx
            GL.setScreenSize storage (fromIntegral w) (fromIntegral h)
            -- TODO
            let idmtx = GL.V4 (GL.V4 1 0 0 0) (GL.V4 0 1 0 0) (GL.V4 0 0 1 0) (GL.V4 0 0 0 1)
            -- ???: why is this needed?
            V.forM_ surfaceObjs $ \objs -> forM_ objs $ \obj -> GL.uniformM44F "worldMat" (GL.objectUniformSetter obj) idmtx
            -- case noBSPCull of
            --   True  -> V.forM_ surfaceObjs $ \objs -> forM_ objs $ \obj -> enableObject obj True
            --   False -> cullSurfaces bsp camPos frust surfaceObjs
            cullSurfaces bsp camPos frust surfaceObjs
            return ()

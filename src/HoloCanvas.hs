{-# LANGUAGE DataKinds, GADTs, KindSignatures, TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitForAll, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RankNTypes #-}
{-# LANGUAGE LambdaCase, OverloadedStrings, RecordWildCards, ScopedTypeVariables, TupleSections, TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}

module HoloCanvas where

-- Basis
import           Prelude                           hiding ((.))
import           Prelude.Unicode
import           Control.Category
import           Control.Lens

-- Type-level
import           GHC.TypeLits                      hiding (Text)

-- Types
import           Control.Arrow                            ((***))
import           Control.Monad                            (join, unless, when, forM_, filterM)
import           Control.Monad.Trans.Reader               (ReaderT(..))
import qualified Data.ByteString.Char8             as SB
import qualified Data.ByteString.Lazy              as LB
import           Data.Map                                 (Map)
import qualified Data.Map                          as Map
import           Data.Maybe                               (fromMaybe)
import           Data.MeasuredMonoid
import           Data.String                              (IsString)
import qualified Data.Text                         as T
import qualified Data.Vector                       as V
import qualified Data.Vect                         as Vc
import           Data.Vect                                (Mat4(..), Vec3(..), Vec4(..))
import           Numeric.Extra                            (floatToDouble, doubleToFloat)

-- Algebra
import           Linear

-- Serialization
import qualified Data.Aeson                        as AE

-- Misc
import           System.FilePath                          ((</>))
import qualified System.Directory                  as FS
import           Text.Show.Pretty                         (ppShow)
import           Text.Printf                              (printf)

-- Manually-bound Cairo
import qualified Graphics.Rendering.Cairo          as GRC
import qualified Graphics.Rendering.Cairo.Internal as GRC (Render(..), create, imageSurfaceCreate)
import qualified Graphics.Rendering.Cairo.Types    as GRC
import qualified Graphics.Rendering.Cairo.Internal as GRCI

-- glib-introspection -based Cairo and Pango
import qualified Data.GI.Base                      as GI
import qualified GI.Cairo                          as GIC
import qualified GI.Cairo.Structs.Context          as GIC
import qualified GI.Pango                          as GIP
import qualified GI.PangoCairo.Interfaces.FontMap  as GIPC
import qualified GI.PangoCairo.Functions           as GIPC

-- Dirty stuff
import qualified Foreign.C.Types                   as F
import qualified Foreign                           as F
import qualified Foreign.Ptr                       as F
import qualified System.IO.Unsafe                  as UN

-- …
import           Graphics.GL.Core33                as GL

-- LambdaCube
import qualified LambdaCube.Compiler               as LCC
import qualified LambdaCube.GL                     as GL
import qualified LambdaCube.GL.Mesh                as GL
import qualified LambdaCube.GL.Type                as GL
import qualified LambdaCube.Linear                 as LCLin
import           LambdaCube.Mesh                   as LC

-- LambdaCube Quake III
import           GameEngine.Data.Material          as Q3
import           GameEngine.Utils                  as Q3

-- Local imports
import Flatland
import HoloFont


-- | Render context for all objects with the same GL pipeline.
data ObjectStream where
  ObjectStream ∷
    { osStorage  ∷ GL.GLStorage
    , osObjArray ∷ ObjArrayNameS
    , osUniform  ∷ UniformNameS
    } → ObjectStream

-- | A GL/Cairo drawable.
data Canvas where
  Canvas ∷
    { cObjectStream ∷ ObjectStream
    , cDi           ∷ Di Int
    , cSurface      ∷ GRC.Surface
    , cStrideBytes  ∷ Wi Int
    , cStridePixels ∷ Wi Int
    , cGRC          ∷ GRC.Cairo
    , cGIC          ∷ GIC.Context
    -- , dTexture      ∷ GL.TextureData
    -- , dGPU          ∷ GL.Object
    } → Canvas

makeCanvas ∷ ObjectStream → Di Double → IO Canvas
makeCanvas cObjectStream cDi' = do
  let cDi@(Di (V2 w h)) = fmap ceiling cDi'
  cSurface      ← GRC.createImageSurface GRC.FormatARGB32 w h
  cStrideBytes  ← Wi <$> GRC.imageSurfaceGetStride cSurface
  let cStridePixels     = (`div` 4) <$> cStrideBytes
  -- let stridePxs = dStrideBytes `div` 4
  cGRC          ← GRC.create cSurface
  cGIC          ← grcToGIC cGRC
  -- XXX: uninitialized: dTexture
  pure Canvas{..}

canvasUpdateGPUSide ∷ Canvas → IO ()
canvasUpdateGPUSide Canvas{..} = do
  let (Di (V2 w h)) = cDi
  pixels   ← GRCI.imageSurfaceGetData cSurface -- XXX/eff: convert to imageSurfaceGetPixels
  cTexture ← uploadTexture2DToGPU'''' False False False False $ (fromWi cStridePixels, h, GL_BGRA, pixels)
  pure ()


-- * Very early generic widget code.
type CanvasSpace p d = Space p Double d
type WidgetSpace   d = CanvasSpace False d

-- | Lifecycle phase of a 'Widget'.
data Phase
  = Shell      -- ^ A fully-parametrized widget, with, however, no content or canvas,
               --   and no dimension or position information at all.
  | Contentful -- ^ Content available, dimension requirements specified, unpositioned.
  | Bound      -- ^ Canvas available, position and dimensions fully specified.

class Widget  (w ∷ Phase → * → *) where
  type FillArg w ∷ *
  type BindArg w ∷ *
  type   Depth w ∷ Nat
  fill         ∷ w Shell a      → a         → FillArg w → IO (w Contentful a)
  spaceRequest ∷ w Contentful a → CanvasSpace False (Depth w)
  bind         ∷ w Contentful a → Canvas    → CanvasSpace True (Depth w) → BindArg w → IO (w Bound a)
  render       ∷ w Bound a      → IO ()


-- * Void: null widget
data Void (p ∷ Phase) a where
  Void  ∷ Void Shell a
  CVoid ∷ Void Contentful a
  PVoid ∷ Void Bound a

instance Widget Void where
  type  FillArg Void = ()
  type  BindArg Void = ()
  type    Depth Void = 0
  fill Void _ ()      = pure CVoid
  spaceRequest _      = End
  bind CVoid _ End () = pure PVoid
  render PVoid        = pure ()


-- * Text: end-point widget
data Text u (p ∷ Phase) a where
  Text ∷ a ~ T.Text ⇒
    { tSettings     ∷ TextSettings TSProto u
    , tColor        ∷ Co Double
    } → Text u Shell a
  CText ∷ a ~ T.Text ⇒
    { tShell        ∷ Text u Shell a
    , tInitialText  ∷ a
    , tUSpace       ∷ CanvasSpace False 1
    } → Text u Contentful a
  PText ∷ a ~ T.Text ⇒
    { tContent      ∷ Text u Contentful a
    , tPSpace       ∷ CanvasSpace True 1
    , tLayout       ∷ GIP.Layout
    , tText         ∷ T.Text
    , tCanvas       ∷ Canvas
    } → Text u Bound a

-- let rrtSpace = sPin (po 0 0) $

instance Widget (Text u) where
  type  FillArg (Text u) = Wi (Size PU)
  type  BindArg (Text u) = TextSettings TSPhys u
  type    Depth (Text u) = 1
  fill tShell@(Text TextSettings{..} _) tInitialText maxWi = do
    di ∷ Di (Size PU) ← layRunTextForSize tsLayout tsDΠ maxWi tInitialText -- XXX/GHC/inference: weak
    let tUSpace = sArea $ fromPU ∘ fromSz tsDΠ <$> di
    pure CText{..}
  spaceRequest = tUSpace

  bind tContent@(CText Text{..} tInitialText _) tCanvas@Canvas{..} tPSpace tc = do
    let tText = tInitialText
    tLayout ← makeTextLayout tc
    pure PText{..}
  render (PText (CText Text{..} _ _)
                (Spc (PWrap _ _ (Po (V2 cvx cvy)) _) End) lay text
                 Canvas{..}) = do
    (`runReaderT` cGRC) $ GRC.runRender $ do
      GRC.moveTo cvx cvy
      coSetSourceColor tColor
      GIP.layoutSetText lay text (-1)
      GIPC.showLayout cGIC lay


-- * Rounded rectangle: widget component
data RRect (p ∷ Phase) a where
  RRect ∷
    { rrCLBezel, rrCBorder, rrCDBezel, rrCBG   ∷ Co Double
    , rrThBez,   rrThBord,             rrThPad ∷ Th Double
    } → RRect Shell a
  CRRect ∷
    { rrShell        ∷ RRect Shell a
    , rrInside       ∷ a
    } → RRect Contentful a
  PRRect ∷
    { rrContent      ∷ RRect Contentful a
    , rrPSpace       ∷ CanvasSpace True 4
    , rrCanvas       ∷ Canvas
    } → RRect Bound a

-- let rrtSpace = sPin (po 0 0) $
--                rrsWSpace <> (sArea $ fromPU ∘ fromSz dπ <$> di)

instance Widget RRect where
  type  FillArg RRect = ()
  type  BindArg RRect = ()
  type    Depth RRect = 4
  fill rrShell rrInside _ =
    pure CRRect{..}
  spaceRequest (CRRect RRect{..} _) =
    sGrowS (fromTh rrThBez) $ sGrowS (fromTh rrThBord) $ sGrowS (fromTh rrThBez) $ sGrowS (fromTh rrThPad) End

  bind rrContent rrCanvas rrPSpace () =
    pure PRRect{..}
  render (PRRect (CRRect RRect{..} _)
                 (Spc obez (Spc bord (Spc ibez (Spc pad _))))
                 (Canvas _ _ _ _ _ cGRC cGIC)) = do
    (`runReaderT` cGRC) $ GRC.runRender $ do
      -- ((layw, layh), ellipsized) ←
      let d (Po (V2 x y)) (Co (V4 r g b a)) = GRC.setSourceRGBA r g b a >> GRC.rectangle (x) (y) 1 1 >> GRC.fill -- GRC.rectangle (x-1) (y-1) 3 3 >> GRC.fill
          dCorn (RRCorn _ pos _ _) col = d pos col
          ths@[oth, bth, ith, pth]
                        = fmap (Th ∘ fromWi ∘ wL) [obez, bord, ibez, pad]
          totpadx       = sum ths
          or            =       R ∘ fromTh $ (totpadx - oth/2)
          br            = or - (R ∘ fromTh $ (oth+bth)*0.6)
          ir            = br - (R ∘ fromTh $ (bth+ith)/2)
      -- coSetSourceColor (co 0 1 0 1) >> GRC.paint
      -- background & border arcs
      let bfeats@[n, ne, _, se, _, sw, _, nw] = wrapRoundedRectFeatures bord br bth
      GRC.newPath >> thLineSet bth
      forM_ [n, ne, se, sw, nw] $ executeFeature Nothing Nothing
      coSetSourceColor rrCBG >>
        GRC.fillPreserve
      coSetSourceColor rrCBorder >>
        GRC.stroke

      thLineSet oth -- border bezels: light outer TL, dark outer SE
      let ofeats@[n, ne, e, se, s, sw, w, nw] = wrapRoundedRectFeatures obez or oth
      GRC.newPath
      (coSetSourceColor $ rrCLBezel) >>
        (forM_ [w, nw, n] $ executeFeature Nothing Nothing) >> GRC.stroke
      (coSetSourceColor $ rrCDBezel) >>
        (forM_ [e, se, s] $ executeFeature Nothing Nothing) >> GRC.stroke
      (coSetSourceColor $ rrCBorder) >>
        GRC.newPath >> (executeFeature (Just rrCDBezel) (Just rrCLBezel) sw) >> GRC.stroke >>
        GRC.newPath >> (executeFeature (Just rrCLBezel) (Just rrCDBezel) ne) >> GRC.stroke

      thLineSet ith -- border bezels: dark inner TL, light inner SE
      let [n, ne, e, se, s, sw, w, nw] = wrapRoundedRectFeatures ibez ir ith
      GRC.newPath
      (coSetSourceColor $ rrCDBezel) >>
        (forM_ [w, nw, n] $ executeFeature Nothing Nothing) >> GRC.stroke
      (coSetSourceColor $ rrCLBezel) >>
        (forM_ [e, se, s] $ executeFeature Nothing Nothing) >> GRC.stroke
      (coSetSourceColor $ rrCBorder) >>
        GRC.newPath >> (executeFeature (Just rrCLBezel) (Just rrCDBezel) sw) >> GRC.stroke >>
        GRC.newPath >> (executeFeature (Just rrCDBezel) (Just rrCLBezel) ne) >> GRC.stroke

       ∷ GRCI.Render () -- XXX/GHC: an apparent type checker bug
      -- ellipsized ← GIP.layoutIsEllipsized gip
      -- (, ellipsized) <$> GIP.layoutGetPixelSize gip


-- * Shader attributery

canvasCommonAttrs ∷ UniformNameS → CommonAttrs
canvasCommonAttrs uname =
  defaultCommonAttrs
  { caSort   = 10.0
  , caStages =
    [ defaultStageAttrs
      { saTexture        = ST_ClampMap ∘ SB.unpack ∘ fromUNS $ uname
      , saTextureUniform = SB.unpack $ fromUNS uname
      , saBlend          = Just ( B_SrcAlpha , B_OneMinusSrcAlpha )
      , saTCGen          = TG_Base
      , saDepthWrite     = True
      , saRGBGen         = RGB_IdentityLighting
      }]}


-- | Cairo contextuary
grcToGIC ∷ GRC.Cairo → IO GIC.Context
grcToGIC grc = GIC.Context <$> GI.newManagedPtr (F.castPtr $ GRC.unCairo grc) (return ())


-- renderCanvasInitial ∷ ObjectStream → RRText → IO (Canvas d)
-- renderCanvasInitial ObjectStream{..} RRText{..} = do

--   pixels      ← GRCI.imageSurfaceGetData grcSurface
--   cvTexture   ← uploadTexture2DToGPU'''' False False False False $ (stridePxs, ceiling $ fromHe cvHe, GL_BGRA, pixels)

--   let (dx, dy)       = (fromIntegral totalw, doubleToFloat $ -fromHe cvHe) --(fromIntegral reqw, -fromIntegral reqh)
--       -- position = V.fromList [ LCLin.V3  0 dy 0, LCLin.V3  0  0 0, LCLin.V3 dx  0 0, LCLin.V3  0 dy 0, LCLin.V3 dx  0 0, LCLin.V3 dx dy 0 ]
--       position = V.fromList [ LCLin.V2  0 dy,   LCLin.V2  0  0,   LCLin.V2 dx  0,   LCLin.V2  0 dy,   LCLin.V2 dx  0,   LCLin.V2 dx dy ]
--       texcoord = V.fromList [ LCLin.V2  0  1,   LCLin.V2  0  0,   LCLin.V2  1  0,   LCLin.V2  0  1,   LCLin.V2  1  0,   LCLin.V2  1  1 ]
--       cvMesh   = LC.Mesh { mPrimitive  = P_Triangles
--                          , mAttributes = Map.fromList [ ("position",  A_V2F position)
--                                                       , ("uv",        A_V2F texcoord) ] }
--   cvGPUMesh ← GL.uploadMeshToGPU cvMesh
--   cvGPU ← GL.addMeshToObjectArray storage (fromOANS objStream) [unameStr mtlUniform, "viewProj"] cvGPUMesh
--   GL.updateObjectUniforms cvGPU $ do
--     fromUNS mtlUniform GL.@= return cvTexture

--   GL.uniformFTexture2D (fromUNS mtlUniform) (GL.uniformSetter storage) cvTexture

--   pure ()


-- * Pipelinistan

newtype UniformNameS  = UniformNameS  { fromUNS  ∷ SB.ByteString } deriving (IsString, Show)
newtype ObjArrayNameS = ObjArrayNameS { fromOANS ∷ String }        deriving (IsString, Show)

unameStr ∷ UniformNameS → String
unameStr = SB.unpack ∘ fromUNS

pipelineSchema ∷ ObjArrayNameS → UniformNameS → GL.PipelineSchema
pipelineSchema cvObjStream cvTexture =
  GL.PipelineSchema
  { objectArrays =
      Map.singleton (fromOANS cvObjStream) $
      GL.ObjectArraySchema GL.Triangles $ Map.fromList
      [ ("position",       GL.Attribute_V2F)
      , ("uv",             GL.Attribute_V2F) ]
  , uniforms =
      Map.fromList $
      [ ("viewProj",         GL.M44F)
      , ("worldMat",         GL.M44F)
      , ("entityRGB",        GL.V3F)
      , ("entityAlpha",      GL.Float)
      , ("identityLight",    GL.Float)
      , ("time",             GL.Float)
      , (unameStr cvTexture, GL.FTexture2D) ]
  }

compilePipeline ∷ FilePath → IO Bool
compilePipeline jsonOutput = Q3.printTimeDiff "-- compiling graphics pipeline... " $ do
  let pipelineSrc = "Holotype.lc"
  LCC.compileMain ["lc"] LCC.OpenGL33 pipelineSrc >>= \case
    Left  err → printf "-- error compiling %s:\n%s\n" pipelineSrc (ppShow err) >> return False
    Right ppl → LB.writeFile jsonOutput (AE.encode ppl)                   >> return True

bindPipeline ∷ GL.GLStorage → String → IO (Maybe GL.GLRenderer)
bindPipeline storage pipelineJSON = do
    putStrLn $ "-- reading GPU pipeline from " ++ pipelineJSON
    let paths = [pipelineJSON]
    validPaths ← filterM FS.doesFileExist paths
    when (Prelude.null validPaths) $
      fail $ "GPU pipeline " ++ pipelineJSON ++ " couldn't be found in " ++ show paths
    renderer ← printTimeDiff "-- allocating GPU pipeline (GL.allocRenderer)... " $ do
      AE.eitherDecode <$> LB.readFile (Prelude.head validPaths) >>= \case
        Left err  → fail err
        Right ppl → GL.allocRenderer ppl
    printTimeDiff "-- binding GPU pipeline to GL storage (GL.setStorage)... " $ GL.setStorage renderer storage
    return $ Just renderer


-- * GL Toolkit
uploadTexture2DToGPU'''' ∷ Bool → Bool → Bool → Bool → (Int, Int, GL.GLenum, F.Ptr F.CUChar) → IO GL.TextureData
uploadTexture2DToGPU'''' isFiltered isSRGB isMip isClamped (w, h, format, ptr) = do
    glPixelStorei GL_UNPACK_ALIGNMENT 1
    to ← F.alloca $! \pto → glGenTextures 1 pto >> F.peek pto
    glBindTexture GL_TEXTURE_2D to
    let texFilter = if isFiltered then GL_LINEAR else GL_NEAREST
        wrapMode = case isClamped of
            True    → GL_CLAMP_TO_EDGE
            False   → GL_REPEAT
        (minFilter,maxLevel) = case isFiltered && isMip of
            False   → (texFilter,0)
            True    → (GL_LINEAR_MIPMAP_LINEAR, floor $ log (fromIntegral $ max w h) / log 2)
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

-- | To screen space conversion matrix.
screenM :: Int → Int → Mat4
screenM w h =
  Mat4 (Vec4 (1/fw)  0     0 0)
       (Vec4  0     (1/fh) 0 0)
       (Vec4  0      0     1 0)
       (Vec4  0      0     0 0.5) -- where does that 0.5 factor COMEFROM?
  where (fw, fh) = (fromIntegral w, fromIntegral h)

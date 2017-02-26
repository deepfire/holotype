{-# LANGUAGE DataKinds, GADTs, KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase, OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}

module HoloCanvas where

-- Basis
import           Prelude                           hiding ((.))
import           Prelude.Unicode
import           Control.Category
import           Control.Lens

-- Types
import           Control.Monad                            (when, forM_, filterM)
import           Control.Monad.Trans.Reader               (ReaderT(..))
import qualified Data.ByteString.Char8             as SB
import qualified Data.ByteString.Lazy              as LB
import           Data.Map                                 (Map)
import qualified Data.Map                          as Map
import           Data.Maybe                               (fromMaybe)
import           Data.String                              (IsString)
import           Data.Text                         as T
import qualified Data.Vector                       as V
import qualified Data.Vect                         as Vc
import           Data.Vect                                (Mat4(..), Vec3(..), Vec4(..))

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


newtype UniformNameS  = UniformNameS  { fromUNS  ∷ SB.ByteString } deriving (IsString, Show)
newtype ObjArrayNameS = ObjArrayNameS { fromOANS ∷ String }        deriving (IsString, Show)

unameStr ∷ UniformNameS → String
unameStr = SB.unpack ∘ fromUNS


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


-- | Cairo/Pango toolkit

grcToGIC ∷ GRC.Cairo → IO GIC.Context
grcToGIC grc = GIC.Context <$> GI.newManagedPtr (F.castPtr $ GRC.unCairo grc) (return ())

-- | Toolkit

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

data CanvasRequest where
  CanvasRequest ∷
    { crSpace    ∷ Space False 5 Double -- 5 = outer bezel, border, inner bezel, padding, drawing area
    , crText     ∷ Text
    , crCFore    ∷ Co Double
    , crCBack    ∷ Co Double
    , crCLBez    ∷ Co Double
    , crCBord    ∷ Co Double
    , crCDBez    ∷ Co Double
    , crFontDesc ∷ GIP.FontDescription
    } → CanvasRequest

data Canvas where
  Canvas ∷
    { cvGRCSurf  ∷ GRC.Surface
    , cvGRCr     ∷ GRC.Cairo
    , cvGICtx    ∷ GIC.Context
    , cvGIPLay   ∷ GIP.Layout
    , cvMatSlot  ∷ UniformNameS
    , cvTexture  ∷ GL.TextureData
    , cvGPU      ∷ GL.Object
    } → Canvas

renderCanvasInitial ∷ GL.GLStorage → ObjArrayNameS → UniformNameS → CanvasRequest → IO Canvas
renderCanvasInitial storage objStream mtlUniform
  (CanvasRequest space'@(Spc _ (Spc _ (Spc _ (Spc _ (Spc canvas End)))))
   text fgColor bgColor lBezColor bordColor dBezColor fontdesc) = do

  -- Determine actual height
  pcd  ← makePangoContextDetached fontdesc
  let V2 cvWi cvMaxHe = fromDi $ wDim canvas
  cvHe ← pcdRunTextForHeight pcd (Wi cvWi) text
  -- Update the space
  let space = sMapInnermostWrap (const $ wArea $ Di $ V2 cvWi (fromHe cvHe)) space'

  -- Compute total dimension (XXX: this work replicated later during sPin)
  let total@(V2 totalw totalh) = ceiling <$> fromDi (sDim space)
  -- Allocate physical drawables
  grcSurface  ← GRC.createImageSurface GRC.FormatARGB32 totalw totalh
  strideBytes ← GRC.imageSurfaceGetStride grcSurface
  let stridePxs = strideBytes `div` 4
  grc         ← GRC.create grcSurface
  gic         ← grcToGIC grc
  --
  gipc        ← GIPC.createContext gic
  gip         ← GIP.layoutNew gipc
  gipSetup gip
  -- GIP.layoutSetFontDescription gip (Just fontdesc)
  GIP.layoutSetText            gip text (-1)
  GIP.layoutSetHeight          gip =<< (GIP.unitsFromDouble $ fromHe cvHe)

  (`runReaderT` grc) $ GRC.runRender $ do
    -- ((layw, layh), ellipsized) ←
    (case sPin space (Po $ V2 0 0) of
      Spc obez (Spc bord (Spc ibez (Spc pad (Spc canv End)))) → do
        let d (Po (V2 x y)) (Co (V4 r g b a)) = GRC.setSourceRGBA r g b a >> GRC.rectangle (x) (y) 1 1 >> GRC.fill -- GRC.rectangle (x-1) (y-1) 3 3 >> GRC.fill
            dCorn (RRCorn _ pos _ _) col = d pos col
            ths@[oth, bth, ith, pth]
                          = fmap (Th ∘ wL) [obez, bord, ibez, pad]
            totpadx       = sum ths
            or            = R ∘ fromTh $ totpadx - oth/2
            br            = or - (R ∘ fromTh $ (oth+bth)*0.6)
            ir            = br - (R ∘ fromTh $ (bth+ith)/2)
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
        GIPC.showLayout gic gip
        ) ∷ GRCI.Render () -- XXX/GHC: an apparent type checker bug
        -- ellipsized ← GIP.layoutIsEllipsized gip
        -- (, ellipsized) <$> GIP.layoutGetPixelSize gip

  pixels      ← GRCI.imageSurfaceGetData grcSurface
  cvTexture   ← uploadTexture2DToGPU'''' False False False False $ (stridePxs, maxh, GL_BGRA, pixels)

  let (dx, dy)       = (fromIntegral totalw, fromIntegral (-maxh)) --(fromIntegral reqw, -fromIntegral reqh)
      -- position = V.fromList [ LCLin.V3  0 dy 0, LCLin.V3  0  0 0, LCLin.V3 dx  0 0, LCLin.V3  0 dy 0, LCLin.V3 dx  0 0, LCLin.V3 dx dy 0 ]
      position = V.fromList [ LCLin.V2  0 dy,   LCLin.V2  0  0,   LCLin.V2 dx  0,   LCLin.V2  0 dy,   LCLin.V2 dx  0,   LCLin.V2 dx dy ]
      texcoord = V.fromList [ LCLin.V2  0  1,   LCLin.V2  0  0,   LCLin.V2  1  0,   LCLin.V2  0  1,   LCLin.V2  1  0,   LCLin.V2  1  1 ]
      cvMesh   = LC.Mesh { mPrimitive  = P_Triangles
                         , mAttributes = Map.fromList [ ("position",  A_V2F position)
                                                      , ("uv",        A_V2F texcoord) ] }
  cvGPUMesh ← GL.uploadMeshToGPU cvMesh
  cvGPU ← GL.addMeshToObjectArray storage (fromOANS objStream) [unameStr mtlUniform, "viewProj"] cvGPUMesh
  GL.updateObjectUniforms cvGPU $ do
    fromUNS mtlUniform GL.@= return cvTexture

  GL.uniformFTexture2D (fromUNS mtlUniform) (GL.uniformSetter storage) cvTexture

  pure Canvas { cvGPU      = cvGPU
              , cvTexture  = cvTexture
              , cvMatSlot  = mtlUniform
              , cvGRCr     = grc
              , cvGRCSurf  = grcSurface
              , cvGICtx    = gic
              , cvGIPLay   = gip }

-- | To screen space conversion matrix.
screenM :: Int → Int → Mat4
screenM w h =
  Mat4 (Vec4 (1/fw)  0     0 0)
       (Vec4  0     (1/fh) 0 0)
       (Vec4  0      0     1 0)
       (Vec4  0      0     0 0.5) -- where does that 0.5 factor COMEFROM?
  where (fw, fh) = (fromIntegral w, fromIntegral h)

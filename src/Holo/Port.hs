{-# OPTIONS_GHC -Weverything #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
{-# OPTIONS_GHC -Wno-implicit-prelude #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-monomorphism-restriction #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unsafe #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-} -- XXX: due to HasCallStack
module Holo.Port where

import           Control.Monad
import           Control.Newtype.Generics
import           Data.Maybe
import           Data.Proxy
import           Data.Typeable
import           Data.Vect                                (Mat4(..), Vec3(..), Vec4(..))
import           GHC.Stack
import           GHC.Types
import           Graphics.GL.Core33                as GL
import           LambdaCube.Mesh                   as LC
import           Linear                            hiding (Trace, V3, V4)
import           Reflex                            hiding (Query, Query(..))
import           Reflex.GLFW                              (RGLFW)
import "GLFW-b"  Graphics.UI.GLFW                  as GL
import qualified Codec.Picture                     as Juicy
import qualified Control.Concurrent.STM            as STM
import qualified Data.Aeson.Encode.Pretty          as AE
import qualified Data.ByteString                   as B
import qualified Data.ByteString.Char8             as SB
import qualified Data.ByteString.Lazy              as LB
import qualified Data.IORef                        as IO
import qualified Data.List                         as L
import qualified Data.IntMap.Strict                as IntMap
import qualified Data.Map.Strict                   as Map
import qualified Data.Text                         as T
import qualified Data.TypeMap.Dynamic              as TM
import qualified Data.IntUnique                    as U
import qualified Data.Vect                         as Vc
import qualified Data.Vector                       as V
import qualified Data.Vector.Storable.ByteString   as B
import qualified Foreign                           as F
import qualified Foreign.C.Types                   as F
import qualified GHC.Generics                      as GHC
import qualified GI.Cairo                          as GIC
import qualified GI.Pango                          as GIP
import qualified Graphics.Rendering.Cairo          as GRC
import qualified Graphics.Rendering.Cairo.Internal as GRCI
import qualified LambdaCube.Compiler               as GL
import qualified LambdaCube.GL                     as GL
import qualified LambdaCube.GL.Mesh                as GL
import qualified LambdaCube.GL.Type                as GL
import qualified LambdaCube.Linear                 as LCLin
import qualified System.IO.Unsafe                  as IO
import qualified Unsafe.Coerce                     as Co

-- Local imports
import           Graphics.Flatland
import           Graphics.Cairo                       (FKind(..))
import qualified Graphics.Cairo                    as Cr
import           Holo.Prelude
import           Tracer()


-- | Usher Cairo + Pango -enabled surfaces onto a GL Window,
--   according to user-controlled Settings.

-- | Usher Cairo + Pango -enabled surfaces onto a GL Window,
--   according to user-controlled Settings.
data Port f where
  Port ∷
    { portSettings        ∷ Settings

    , portFontmap         ∷ Cr.FontMap PU
    , portWindow          ∷ GL.Window

    , portVisualTracker   ∷ TyIdMap f

    , portGLStorage       ∷ GL.GLStorage
    , portObjectStream    ∷ ObjectStream

    , portPipelines       ∷ Map.Map PipeName GL.GLRenderer
    } → Port f
    deriving (GHC.Generic)

data Drawable where
  Drawable ∷
    { dObjectStream       ∷ ObjectStream
    , dDi                 ∷ Di Int
    , dSurface            ∷ GRCI.Surface
    , dSurfaceData        ∷ (F.Ptr F.CUChar, V2 Int)
    , dCairo              ∷ Cr.Cairo
    , dGIC                ∷ GIC.Context
    --
    , dMesh               ∷ LC.Mesh
    , dGPUMesh            ∷ GL.GPUMesh
    , dGLObject           ∷ GL.Object
    , dTexId              ∷ GLuint
    } → Drawable

data PipeName
  = PipeDraw
  | PipePick
  deriving (Bounded, Enum, Eq, Ord, Show)

-- | A GL 'Frame'.
data Frame where
  Frame ∷
    { fDim ∷ Di Int
    } → Frame

-- | Render context for all objects with the same GL store.
data ObjectStream where
  ObjectStream ∷
    { osStorage  ∷ GL.GLStorage
    , osObjArray ∷ ObjArrayNameS
    , osUniform  ∷ UniformNameS
    } → ObjectStream

newtype UniformNameS  = UniformNameS  { fromUNS  ∷ SB.ByteString } deriving (Eq, IsString, Ord, Show)
newtype ObjArrayNameS = ObjArrayNameS { fromOANS ∷ String }        deriving (Eq, IsString, Ord, Show)

portDΠ ∷ Port f → DΠ
portDΠ = sttsDΠ ∘ portSettings

portSetVSync ∷ (MonadIO m) ⇒ WaitVSync → m ()
portSetVSync x = liftIO $ GL.swapInterval $ case x of
                                              WaitVSync True  → 1
                                              WaitVSync False → 0

data ScreenMode
  = FullScreen
  | Windowed
  deriving (Eq, GHC.Generic, Read, Show)

newtype ScreenDim a = ScreenDim a
  deriving (Eq, Newtype, Show)

newtype WaitVSync = WaitVSync Bool deriving (Eq, Show)

data Settings where
  Settings ∷
    { sttsDΠ              ∷ DΠ
    , sttsFontPreferences ∷ Cr.FontPreferences
    , sttsScreenMode      ∷ ScreenMode
    , sttsScreenDim       ∷ ScreenDim (Di Int)
    , sttsWaitVSync       ∷ WaitVSync
    } → Settings
    deriving (Eq, GHC.Generic, Show)

defaultSettings ∷ Settings
defaultSettings =
  let sttsDΠ ∷ DΠ         = 96
      sttsFontPreferences = Cr.FontPreferences
        [ ("default",     Left $ Cr.Alias "defaultMono" )
        , ("defaultSans", Right $ [ Cr.FontSpec "Bitstream Charter" "Regular" $ Cr.Outline (UnitPU $ PUs 16)
                                  , Cr.FontSpec "Aurulent Sans"     "Regular" $ Cr.Outline (UnitPU $ PUs 16) ])
        , ("defaultMono", Right $ [ Cr.FontSpec "Terminus"          "Regular" $ Cr.Bitmap  (UnitPU $ PUs 15) LT ])
        ]
      sttsScreenMode      = Windowed
      sttsScreenDim       = ScreenDim $ di 800 600
      sttsWaitVSync       = WaitVSync False
  in Settings{..}

data ESettings t where
  ESettings ∷
    { esDΠFontPrefs       ∷ Event t (DΠ, Cr.FontPreferences)
    , esScreenModeDim     ∷ Event t (ScreenMode, ScreenDim (Di Int))
    , esWaitVSync         ∷ Event t WaitVSync
    } → ESettings t

(⋈) ∷ Reflex t ⇒ Dynamic t a → Dynamic t b → Dynamic t (a, b)
(⋈) = zipDynWith (,)

portCreate ∷ (RGLFW t m, MonadTrace r m) ⇒ Dynamic t GL.Window → ESettings t → m (Dynamic t (Maybe (Port f)))
portCreate winD ESettings{..} = do
  blankIdToken'setup
  portVisualTracker ← mkTIMap

  let (,) osName uniName = ("portStream", "portMtl")
      schema             = pipelineSchema [(osName, uniName)]
  portGLStorage ← liftIO $ GL.allocStorage schema
  let portObjectStream   = ObjectStream portGLStorage osName uniName
  pipeDraw      ← buildPipelineForStorage portGLStorage "PipeDraw.lc"

  fontmapE      ← performEvent $ esDΠFontPrefs <&>
    \(dπ, fontPrefs) → do
      portFontmap ← Cr.makeFontMap dπ Cr.fmDefault fontPrefs
      liftIO $ putStrLn $ printf "%s" (show portFontmap)
      pure (dπ, fontPrefs, portFontmap)
  fontmapD      ← foldDyn (\e _→ Just e) Nothing fontmapE

  pipelinesE    ← performEvent $ esScreenModeDim <&>
    \(scrMode, scrDim@(ScreenDim dim@(Di (V2 w h)))) → do
      case scrMode of
        Windowed   → liftIO $ printf "window size: %dx%d\n" w h--GL.setWindowSize win w h
        FullScreen → error "XXX: implement fullscreen"
      liftIO $ SB.writeFile "lc/PipePick.lc" $ mkPipePickText osName dim
      pipePick      ← buildPipelineForStorage portGLStorage "PipePick.lc"
      pure $ (,,) scrMode scrDim
                  (Map.fromList [(PipePick, pipePick)
                                ,(PipeDraw, pipeDraw)])
  pipelinesD    ← foldDyn (\e _→ Just e) Nothing pipelinesE

  vsyncE        ← performEvent $ esWaitVSync <&>
    \waitVSync → do
      portSetVSync waitVSync
      pure waitVSync
  vsyncD        ← foldDyn (\e _→ Just e) Nothing vsyncE

  let composeD   = winD ⋈ fontmapD ⋈ pipelinesD ⋈ vsyncD
      filteredE  = fmapMaybe (\case
                                 (((a, Just b), Just c), Just d) → Just (a, b, c, d)
                                 _ → Nothing)
                   (updated composeD)

  portE ← performEvent $ filteredE <&>
    \(portWindow
     ,(sttsDΠ, sttsFontPreferences, portFontmap)
     ,(sttsScreenMode, sttsScreenDim, portPipelines)
     ,sttsWaitVSync) → do
      let portSettings = Settings{..}
      pure $ Just Port{..}

  holdDyn Nothing portE

portShutdown ∷ (MonadIO m) ⇒ Port f → m ()
portShutdown Port{..} = liftIO $ do
  _ ← traverse GL.disposeRenderer portPipelines
  GL.destroyWindow portWindow

mkPipePickText ∷ ObjArrayNameS → Di Int → SB.ByteString
mkPipePickText oans (Di di) = SB.unlines
  [ "type FB = FrameBuffer 1 '[ 'Color (Vec 4 Int)]"
  , ""
  , "intV4I :: Int -> Vec 4 Int"
  , "intV4I x = V4"
  , "  ((x)            % 256) -- XXX: this is suboptimal, but we can't use shifts"
  , "  ((x / 256)      % 256) --      because of the shifts not supported by WebGL 1"
  , "  ((x / 65536)    % 256)"
  , "  ((x / 16777216) % 256)"
  , ""
  , "scene :: String -> FB -> FB"
  , "scene name prevFB ="
  , "  Accumulate    ((ColorOp NoBlending (one :: Vec 4 Bool)))"
  , "  (mapFragments (\\(uv, rgba) -> ((rgba)))"
  , "   $ rasterizePrimitives (TriangleCtx CullFront PolygonFill NoOffset LastVertex) (Flat, Flat)"
  , "   $ mapPrimitives"
  , "    (\\(pos, _, id)->"
  , "      ( (Uniform \"viewProj\" :: Mat 4 4 Float) *. (V4 pos%x pos%y 0 1)"
  , "      , V2 0.0 0.0"
  , "      , intV4I id))"
  , "    $ fetch name ( Attribute \"position\"   :: Vec 3 Float"
  , "                 , Attribute \"uv\"         :: Vec 2 Float"
  , "                 , Attribute \"id\"         :: Int))"
  , "  prevFB"
  , ""
  , "main :: Output"
  , "main = TextureOut ("<> SB.pack (show di) <>") $"
  , "       scene \""<> SB.pack (fromOANS oans) <>"\" $"
  , "       FrameBuffer ((colorImage1 (V4 0 0 0 0)))"
  ]


-- * Frames / framebuffers
--
portDrawFrame ∷ (MonadIO m) ⇒ Port f → PipeName → m ()
portDrawFrame Port{..} name = liftIO $ do
  GL.renderFrame ∘ Data.Maybe.fromJust $ Map.lookup name portPipelines

portWindowSize ∷ (MonadIO m) ⇒ GL.Window → m (Di Int)
portWindowSize win = liftIO (GL.getFramebufferSize win)
                     <&> uncurry (Di .: V2)

portSetupFrame ∷ (MonadIO m) ⇒ Port f → m Frame
portSetupFrame Port{..} = liftIO $ do
  let slotU           = GL.uniformSetter portGLStorage
      overbrightBits  = 0
  GL.uniformFloat "identityLight" slotU $ 1 / (2 ^ (overbrightBits ∷ Int)) -- used by lc:mkColor
  -- XXX: shadow size computation
  dim@(Di (V2 screenW screenH)) ← portWindowSize portWindow
  GL.setScreenSize portGLStorage (fromIntegral screenW) (fromIntegral screenH)
  pure $ Frame dim

portNextFrame ∷ (MonadIO m) ⇒ Port f → m Frame
portNextFrame port@Port{..} = do
  portDrawFrame  port PipeDraw
  liftIO $ GL.swapBuffers portWindow
  portSetupFrame port

-- | Picking is a perverse form of drawing.
portPick ∷ MonadIO m ⇒ Port f → Po Int → m IdToken
portPick port@Port{portSettings=Settings{sttsScreenDim=ScreenDim dim}} pos = do
  -- liftIO $ B.writeFile "screenshot.png" =<< Juicy.imageToPng <$> snapFrameBuffer (di 800 600)
  GL.glDisable GL.GL_FRAMEBUFFER_SRGB
  portDrawFrame port PipePick
  let glRenderer = fromJust $ portPipeline port PipePick
      (fromIntegral → fb)
        = case GL.glOutputs glRenderer of
            [GL.GLOutputRenderTexture fbo _rendTex] → fbo
            outs → error $ "Unexpected outputs: " <> show outs
  raw ← liftIO $ pickFrameBuffer fb dim pos
  GL.glEnable GL.GL_FRAMEBUFFER_SRGB
  let decoded ∷ Integer = fromIntegral raw
  -- XXX: we're breaking the Unique-ness here!
  -- TODO: fix this by maintaining a map from (hashUnique → k) to Unique
  pure $ IdToken $ Co.unsafeCoerce decoded

-- The next couple of functions mostly stolen from lambdacube-gl/testclient.hs
snapFrameBuffer ∷ (MonadIO m) ⇒ Di Int → m Juicy.DynamicImage
snapFrameBuffer (Di (V2 w h)) = do
  glFinish
  glBindFramebuffer GL_READ_FRAMEBUFFER 0
  -- glReadBuffer GL_FRONT
  -- glBlitFramebuffer 0 0 (fromIntegral w) (fromIntegral h) 0 (fromIntegral h) (fromIntegral w) 0 GL_COLOR_BUFFER_BIT GL_NEAREST
  glReadBuffer GL_BACK
  bs ← liftIO $ withFrameBuffer w GL_RGBA 0 0 w h $ \p -> B.packCStringLen (F.castPtr p,w*h*4)
  let v = B.byteStringToVector bs
  pure $ Juicy.ImageRGBA8 $ Juicy.Image w h v

pickFrameBuffer ∷ (MonadIO m)
  ⇒ GLint       -- ^ framebuffer
  → Di Int      -- ^ FB dimensions
  → Po Int      -- ^ pick coordinates
  → m F.Word32  -- ^ resultant pixel value
pickFrameBuffer fb (Di (V2 w h)) (Po (V2 x y)) = do
  glFinish
  glBindFramebuffer GL_READ_FRAMEBUFFER $ fromIntegral fb
  let (fbmode, format) =
        if fb == 0
        then (GL_BACK_LEFT,         GL_RGBA)
        else (GL_COLOR_ATTACHMENT0, GL_RGBA_INTEGER)
  glReadBuffer fbmode
  liftIO $ withFrameBuffer w format x (h - y - 1) 1 1 $ \p -> F.peek (F.castPtr p ∷ F.Ptr F.Word32)

withFrameBuffer ∷ Int → GLenum → Int → Int → Int → Int → (F.Ptr F.Word8 → IO a) → IO a
withFrameBuffer rowLen format x y w h fn = F.allocaBytes (w*h*4) $ \p → do
  glPixelStorei GL_UNPACK_LSB_FIRST    0
  glPixelStorei GL_UNPACK_SWAP_BYTES   0
  glPixelStorei GL_UNPACK_ROW_LENGTH   $ fromIntegral rowLen
  glPixelStorei GL_UNPACK_IMAGE_HEIGHT 0
  glPixelStorei GL_UNPACK_SKIP_ROWS    0
  glPixelStorei GL_UNPACK_SKIP_PIXELS  0
  glPixelStorei GL_UNPACK_SKIP_IMAGES  0
  glPixelStorei GL_UNPACK_ALIGNMENT    1
  glReadPixels (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h) format GL_UNSIGNED_BYTE $ F.castPtr p
  glPixelStorei GL_UNPACK_ROW_LENGTH   0
  fn p

deriving instance Show (GL.GLOutput)
deriving instance Show (GL.GLTexture)


-- * Pipelinistan
--
portPipeline ∷ Port f → PipeName → Maybe GL.GLRenderer
portPipeline Port{..} = flip Map.lookup portPipelines

pipelineSchema ∷ [(ObjArrayNameS, UniformNameS)] → GL.PipelineSchema
pipelineSchema schemaPairs =
  let arrays   = fromOANS ∘ view _1            <$> schemaPairs
      textures = SB.unpack ∘ fromUNS ∘ view _2 <$> schemaPairs
      simplePosUVSchema =
        GL.ObjectArraySchema GL.Triangles $ Map.fromList
        [ ("position",       GL.Attribute_V2F)
        , ("uv",             GL.Attribute_V2F)
        , ("id",             GL.Attribute_Int) ]
  in GL.PipelineSchema
  { objectArrays = Map.fromList $ zip arrays $ repeat simplePosUVSchema
  , uniforms =
      Map.fromList $
      [ ("viewProj",         GL.M44F)
      , ("worldMat",         GL.M44F)
      , ("entityRGB",        GL.V3F)
      , ("entityAlpha",      GL.Float)
      , ("identityLight",    GL.Float)
      , ("time",             GL.Float) ]
      ++ zip textures (repeat GL.FTexture2D)
  }

buildPipelineForStorage ∷ (HasCallStack, MonadIO m) ⇒ GL.GLStorage → String → m GL.GLRenderer
buildPipelineForStorage storage pipelineSrc = liftIO $ do
  (printTimeDiff "-- compiling graphics pipeline... " $
    GL.compileMain ["lc"] GL.OpenGL33 pipelineSrc) >>= \case
    Left  err → error $ printf "-- error compiling %s:\n%s\n" pipelineSrc (GL.ppShow err)
    Right ppl → do
      renderer ← GL.allocRenderer ppl
      LB.writeFile ("run/" <> pipelineSrc <> ".json") (AE.encodePretty ppl)
      _ ← printTimeDiff "-- binding GPU pipeline to GL storage (GL.setStorage)... " $
        GL.setStorage renderer storage
      pure renderer


-- * A Pango/Cairo-capable 'Drawable' with an artificial identity.
--
clearDrawable ∷ (MonadIO m) ⇒ Drawable → m ()
clearDrawable Drawable{..} = do
  Cr.runCairo dCairo $ do
    GRC.save
    GRC.setOperator GRCI.OperatorSource
    Cr.crColor (co 0 0 0 0)
    GRC.paint
    GRC.restore

imageSurfaceGetPixels' ∷ HasCallStack ⇒ GRC.Surface → IO (F.Ptr F.CUChar, V2 Int)
imageSurfaceGetPixels' pb = do
  pixPtr ← GRCI.imageSurfaceGetData pb
  when (pixPtr ≡ F.nullPtr) $ do
    error "imageSurfaceGetPixels: image surface not available"
  h ← GRC.imageSurfaceGetHeight pb
  r ← GRC.imageSurfaceGetStride pb
  return (pixPtr, V2 r h)

makeDrawable ∷ (HasCallStack, MonadTrace r m) ⇒ ObjectStream → IdToken → Di Double → m Drawable
makeDrawable dObjectStream@ObjectStream{..} tok dDi' = do
  let dDi@(Di (V2 w h)) = fmap ceiling dDi'
  unless (w > 0 ∧ h > 0) $
    error $ printf "makeDrawable: non-positive dimensions are not acceptable (token=0x%x): %s" (tokenHash tok) (show $ dDi'^.di'v)
  dSurface      ← liftIO $ GRC.createImageSurface GRC.FormatARGB32 w h
  dCairo        ← liftIO $ Cr.cairoCreate  dSurface
  dGIC          ← liftIO $ Cr.cairoToGICairo dCairo

  let (dx, dy) = (fromIntegral w, fromIntegral $ -h)
      -- position = V.fromList [ LCLin.V3  0 dy 0, LCLin.V3  0  0 0, LCLin.V3 dx  0 0, LCLin.V3  0 dy 0, LCLin.V3 dx  0 0, LCLin.V3 dx dy 0 ]
      position = V.fromList [ LCLin.V2  0 dy,   LCLin.V2  0  0,   LCLin.V2 dx  0,   LCLin.V2  0 dy,   LCLin.V2 dx  0,   LCLin.V2 dx dy ]
      texcoord = V.fromList [ LCLin.V2  0  1,   LCLin.V2  0  0,   LCLin.V2  1  0,   LCLin.V2  0  1,   LCLin.V2  1  0,   LCLin.V2  1  1 ]
      ids      = V.fromList $ L.replicate 6 $ fromIntegral (tokenHash tok)
      dMesh    = LC.Mesh { mPrimitive  = P_Triangles
                         , mAttributes = Map.fromList [ ("position",  A_V2F position)
                                                      , ("uv",        A_V2F texcoord)
                                                      , ("id",        A_Int ids)] }
  dGPUMesh      ← liftIO $ GL.uploadMeshToGPU dMesh
  -- XXX: this is leaky -- has to be done manually
  -- SMem.addFinalizer dGPUMesh $ do
  --   GL.disposeMesh dGPUMesh
  dGLObject     ← liftIO $ GL.addMeshToObjectArray osStorage (fromOANS osObjArray) [SB.unpack $ fromUNS osUniform, "viewProj"] dGPUMesh

  dSurfaceData  ← liftIO $ imageSurfaceGetPixels' dSurface
  dTexId        ← liftIO $ F.alloca $! \pto → glGenTextures 1 pto >> F.peek pto
  logDebug "TEX ALLOC %s %d" (show $ dDi^.di'v, dTexId)
  -- traceNamedObject tr (LP (LogValue "tex" (PureD (dDi^.di'v))))
  -- trev ALLOC TEX (dDi^.di'v) (fromIntegral dTexId)

  -- dTexture      ← uploadTexture2DToGPU'''' False False False False $ (fromWi dStridePixels, h, GL_BGRA, pixels)
  pure Drawable{..}

disposeDrawable ∷ (HasCallStack, MonadTrace r m) ⇒ ObjectStream → Drawable → m ()
disposeDrawable ObjectStream{..} Drawable{..} =
  -- see experiment in LCstress
  logDebug "TEX FREE %s %d" (show $ dDi^.di'v, dTexId) >>
  do liftIO $ do
       F.withArray [dTexId] (glDeleteTextures 1) -- release tex id
       GL.removeObject osStorage dGLObject
       GL.disposeMesh  dGPUMesh
       -- dCairo ← cairoCreate is auto-managed
       GRCI.surfaceFinish dSurface -- undo createImageSurface

drawableContentToGPU ∷ (MonadIO m) ⇒ Drawable → m ()
drawableContentToGPU Drawable{..} = liftIO $ do
  let ObjectStream{..} = dObjectStream

  let (pixels, V2 strideBytes pixelrows) = dSurfaceData
  cTexture ← uploadTexture2DToGPU'''' False False False False (strideBytes `div` 4, pixelrows, GL_BGRA, pixels) dTexId

  GL.updateObjectUniforms dGLObject $ do
    fromUNS osUniform GL.@= return cTexture

uploadTexture2DToGPU'''' ∷ (MonadIO m) ⇒ Bool → Bool → Bool → Bool → (Int, Int, GL.GLenum, F.Ptr F.CUChar) → GLuint → m GL.TextureData
uploadTexture2DToGPU'''' isFiltered isSRGB isMip isClamped (w, h, format, ptr) to' = liftIO $ do
    glPixelStorei GL_UNPACK_ALIGNMENT 1
    glBindTexture GL_TEXTURE_2D to'
    let texFilter = if isFiltered then GL_LINEAR else GL_NEAREST
        wrapMode = case isClamped of
            True    → GL_CLAMP_TO_EDGE
            False   → GL_REPEAT
        (minFilter,maxLevel) = case isFiltered && isMip of
            False   → (texFilter,0)
            True    → (GL_LINEAR_MIPMAP_LINEAR, (floor $ (log (fromIntegral $ max w h) ∷ Float) / log 2) ∷ Int)
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S $ fromIntegral wrapMode
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T $ fromIntegral wrapMode
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER $ fromIntegral minFilter
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER $ fromIntegral texFilter
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_BASE_LEVEL 0
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAX_LEVEL $ fromIntegral maxLevel
    let internalFormat  = fromIntegral $ if isSRGB then GL_SRGB8_ALPHA8 else GL_RGBA8
    glTexImage2D GL_TEXTURE_2D 0 internalFormat (fromIntegral w) (fromIntegral h) 0 format GL_UNSIGNED_BYTE $ F.castPtr ptr
    when isMip $ glGenerateMipmap GL_TEXTURE_2D
    return $ GL.TextureData to'

framePutDrawable ∷ (MonadIO m) ⇒ Frame → Drawable → Po Float → m ()
framePutDrawable (Frame (Di (V2 screenW screenH))) Drawable{..} (Po (V2 x y)) = do
  let cvpos     = Vec3 x (-y) 0
      toScreen  = screenM screenW screenH
  liftIO $ GL.uniformM44F "viewProj" (GL.objectUniformSetter $ dGLObject) $
    mat4ToM44F $! (Vc.fromProjective $! Vc.translation cvpos) Vc..*. toScreen


-- * Math, some of it from lambdacube-quake3
--
vec4ToV4F ∷ Vec4 → LCLin.V4F
vec4ToV4F (Vc.Vec4 x y z w) = LCLin.V4 x y z w

vec3ToV3F ∷ Vec3 → LCLin.V3F
vec3ToV3F (Vec3 x y z) = LCLin.V3 x y z

mat4ToM44F ∷ Mat4 → LCLin.M44F
mat4ToM44F (Mat4 a b c d) = LCLin.V4 (vec4ToV4F a) (vec4ToV4F b) (vec4ToV4F c) (vec4ToV4F d)

rotationEuler ∷ Vec3 → Vc.Proj4
rotationEuler (Vec3 a b c) = Vc.orthogonal $ Vc.toOrthoUnsafe $ Vc.rotMatrixZ a Vc..*. Vc.rotMatrixX b Vc..*. Vc.rotMatrixY (-c)

-- This one, while canonical, murders the projection.
ortho ∷ Int → Int → Int → Int → Int → Int → Mat4
ortho l' r' t' b' n' f' =
  Vc.Mat4 (Vc.Vec4 (2/r-l) 0      0         (-(r+l)/(r-l)))
          (Vc.Vec4  0     (2/t-b) 0         (-(t+b)/(t-b)))
          (Vc.Vec4  0      0     ((-2)/f-n) (-(f+n)/(f-n)))
          (Vc.Vec4  0      0      0         (1))
  where (,,,,,) l r t b n f =
          (,,,,,) (fromIntegral l') (fromIntegral r') (fromIntegral t') (fromIntegral b') (fromIntegral n') (fromIntegral f')

orthoLU, orthoLB ∷ Int → Int → Mat4
orthoLU w h = Holo.Port.ortho 0 w h 0 1 (-1)
orthoLB w h = Holo.Port.ortho 0 w 0 h 1 (-1)

-- | To screen space conversion matrix.
screenM :: Int → Int → Mat4
screenM w h = scaleM -- Vc..*. flipM
  where (fw, fh) = (fromIntegral w, fromIntegral h)
        scaleM = Vc.Mat4 (Vc.Vec4 (1/fw)  0     0 0)
                         (Vc.Vec4  0     (1/fh) 0 0)
                         (Vc.Vec4  0      0     1 0)
                         (Vc.Vec4  0      0     0 0.5) -- where does that 0.5 factor COMEFROM?


-- * Drawable identity support
--
newtype IdToken = IdToken { fromIdToken' ∷ U.Unique } deriving (Eq, Ord)
instance Show IdToken where
  show (IdToken u) = printf "id=x%x" (U.hashUnique u)

fromIdToken ∷ IdToken → U.Unique
fromIdToken = fromIdToken'
{-# INLINE fromIdToken #-}

newId ∷ (HasCallStack, MonadTrace r m) ⇒ T.Text → m IdToken
newId desc = do
  tok ← liftIO $ U.newUnique
  logDebug "TOK ALLOC %s %s" (desc, U.hashUnique tok)
  -- trev ALLOC TOK desc (U.hashUnique tok)
  pure $ IdToken tok

blankIdToken'      ∷ IO.IORef IdToken
blankIdToken'      = IO.unsafePerformIO $ IO.newIORef  undefined
blankIdToken'setup ∷ MonadTrace r m ⇒ m ()
blankIdToken'setup = newId "blank" >>= (liftIO ∘ IO.writeIORef blankIdToken')
blankIdToken       ∷ IdToken
blankIdToken       = IO.unsafePerformIO $ IO.readIORef blankIdToken'
{-# NOINLINE blankIdToken #-}

tokenHash ∷ IdToken → Int
tokenHash = U.hashUnique ∘ fromIdToken
{-# INLINE tokenHash #-}

data IdTokenMap f

data CTIMap f a where
  CTIMap ∷ c a ⇒
    { ctimCstr ∷ Proxy (c ∷ Type → Constraint)
    , ctimMap  ∷ IntMap.IntMap (f a)
    } → CTIMap f a

type instance TM.Item (IdTokenMap f) a = CTIMap f a

data TyIdMap (f ∷ Type → Type) where
  TyIdMap ∷ (STM.TVar (TM.TypeMap (IdTokenMap f))) → TyIdMap f

mkTIMap ∷ (MonadIO m) ⇒ m (TyIdMap f)
mkTIMap = TyIdMap
  <$> (liftIO $ STM.newTVarIO $ TM.empty)

tiMapAccess ∷ (MonadIO m) ⇒ TyIdMap f → m (TM.TypeMap (IdTokenMap f))
tiMapAccess (TyIdMap m) = liftIO $ STM.readTVarIO m

tiMapAdd ∷ (Typeable a, MonadIO m, c a)
  ⇒ Proxy (c ∷ Type → Constraint)
  → Proxy (a ∷ Type)
  → IdToken
  → f a
  → TyIdMap f
  → m ()
tiMapAdd pC pA k v (TyIdMap tm) = liftIO $ STM.atomically $ STM.modifyTVar' tm $
  \tm → TM.insert pA (case TM.lookup pA tm of
                        Nothing             → CTIMap pC $ IntMap.singleton (tokenHash k) v
                        Just (CTIMap _ idm) → CTIMap pC $ IntMap.insert    (tokenHash k) v idm)
        tm

tiMapReplace ∷ (MonadIO m) ⇒ TyIdMap f → TM.TypeMap (IdTokenMap f) → m ()
tiMapReplace (TyIdMap m) tm = liftIO $ STM.atomically $ STM.writeTVar m tm

-- viomapHas ∷ (MonadIO m, Typeable a) ⇒ TyIdMap f → Proxy (a ∷ Type) → IdToken → m Bool
-- viomapHas iomap p k = isJust ∘ join ∘ (Map.lookup k <$>) ∘ TM.lookup p <$> viomapAccess iomap


-- * Look up the visual and, if present, check compatibility with
--   new requirements: style generation and size.
--   XXX: violates abstraction:
--     - Holo a ⇒ createVisual
--     - portVisualTracker/viomapAccess/viomapAdd
--     - mkVisual/freeVisualOf
--     - newDrawable/disposeDrawable
--   XXX: not thread-safe

portEnsureVisual ∷ (HasCallStack, MonadTrace r m, PortVisual f, Typeable a, c a)
  ⇒ Port f
  → Di Double
  → Proxy (c ∷ Type → Constraint)
  → IdToken
  → Proxy a
  → (f a → Bool)
  → (Drawable → m (f a))
  → m (f a)
portEnsureVisual Port{..} newDim@(Di (V2 newW newH)) hiC hitok pHi keepTest hif =
  case portVisualTracker of
    TyIdMap _ → do
      tm ← tiMapAccess portVisualTracker
      (vis ∷ f x, updated ∷ Bool)
        ← case join $ IntMap.lookup (tokenHash hitok) ∘ ctimMap <$> TM.lookup pHi tm of
            Nothing → do
              logDebug "VIS MISSALLOC %s %d" (show (newW, newH), tokenHash hitok)
              -- trev MISSALLOC VIS (newW, newH) (tokenHash hitok)
              (,True) <$> (hif =<< makeDrawable portObjectStream hitok newDim)
            Just (pv ∷ f x) → do
              let pvDrw  = pvDrawable pv
              if not (keepTest pv) ∨ (dDi pvDrw ≢ (ceiling <$> newDim))
              then do
                logDebug "VIS REALLOC %s %d" (show (newW, newH), tokenHash hitok)
                -- trev REALLOC VIS (newW, newH) (tokenHash hitok)
                pvFree hiC Proxy pv
                disposeDrawable portObjectStream pvDrw
                (,True) <$> (hif =<< makeDrawable portObjectStream hitok newDim)
              else do
                logDebug "VIS REUSE %s %d" (show (newW, newH), tokenHash hitok)
                -- trev REUSE   VIS (newW, newH) (tokenHash hitok)
                pure $ (pv, False)
      when updated $
        tiMapAdd hiC pHi hitok vis portVisualTracker
      pure vis

class PortVisual f where
  pvDrawable    ∷ f a → Drawable
  pvFree        ∷ (MonadIO m, c a) ⇒ Proxy c → Proxy a → f a → m ()

portGarbageCollectVisuals ∷ ∀ r m f a. (MonadTrace r m, PortVisual f) ⇒ Port f → IntMap.IntMap a → m ()
portGarbageCollectVisuals Port{..} validLeaves = do
  case portVisualTracker of
    TyIdMap _ → do
      tm ← tiMapAccess portVisualTracker
      let gcTM ∷ Proxy b -> CTIMap f b -> m (CTIMap f b)
          gcTM _pA (CTIMap pC vismap) = do
            let used   = IntMap.intersection vismap validLeaves
                unused = IntMap.difference   vismap used
            _ ← flip IntMap.traverseWithKey unused $ \_tok pv→ do
              pvFree pC Proxy pv
              -- flip (trev FREE VIS (tokenHash k)) $
              --   case vDrawable of
              --     Nothing → (0, 0)
              --     Just vDrawable → (dDi vDrawable^.di'w, dDi vDrawable^.di'h)
              disposeDrawable portObjectStream $ pvDrawable pv
            pure $ CTIMap pC used
      tm' ← TM.traverse gcTM tm
      tiMapReplace portVisualTracker tm'


-- * Fonts
--
portFont  ∷ Port f → Cr.FontKey → Maybe (Cr.Font Found PU)
portFont Port{..} = Cr.lookupFont portFontmap

portFont' ∷ Port f → Cr.FontKey → Cr.Font Found PU
portFont' po fk = portFont po fk
  & fromMaybe (Cr.errorMissingFontkey fk)

drawableBindFontLayout ∷ (MonadIO m, FromUnit u, FromUnit v) ⇒
  DΠ → Drawable → Cr.Font Found u → Di (Unit v) → Cr.TextSizeSpec v → m (Cr.WFont Bound, GIP.Layout)
drawableBindFontLayout dπ Drawable{..} = Cr.bindWFontLayout dπ dGIC

drawableDrawText ∷ (MonadIO m) ⇒ Drawable → GIP.Layout → Co Double → Po Double → T.Text → m ()
drawableDrawText Drawable{..} layout color shift text = do
  Cr.layDrawText dCairo dGIC layout shift color text

{-# LANGUAGE GADTs, TypeFamilies, TypeFamilyDependencies, TypeInType #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitForAll, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RankNTypes, UndecidableInstances #-}
{-# LANGUAGE LambdaCase, OverloadedStrings, PackageImports, PartialTypeSignatures, RecordWildCards, ScopedTypeVariables, TupleSections, TypeOperators, ViewPatterns #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Weverything #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors -Wno-missing-import-lists -Wno-implicit-prelude #-}
{-# OPTIONS_GHC -Wno-monomorphism-restriction -Wno-name-shadowing -Wno-all-missed-specialisations #-}
{-# OPTIONS_GHC -Wno-unsafe -Wno-missing-export-lists -Wno-type-defaults -Wno-unused-do-bind #-}

module HoloPort where


import           Control.Monad
import           Data.Maybe
import           Data.Proxy
import           Data.Typeable
import           Data.Vect                                (Mat4(..), Vec3(..), Vec4(..))
import           GHC.Stack
import           Graphics.GL.Core33                as GL
import           HoloPrelude
import           LambdaCube.Mesh                   as LC
import           Linear                            hiding (V3, V4)
import "GLFW-b"  Graphics.UI.GLFW                  as GL
import qualified Codec.Picture                     as Juicy
import qualified Control.Concurrent.STM            as STM
import qualified Data.Aeson.Encode.Pretty          as AE
import qualified Data.ByteString                   as B
import qualified Data.ByteString.Char8             as SB
import qualified Data.ByteString.Lazy              as LB
import qualified Data.IORef                        as IO
import qualified Data.List                         as L
import qualified Data.Map.Strict                   as Map
import qualified Data.Text                         as T
import qualified Data.TypeMap.Dynamic              as TM
import qualified Data.Unique                       as U
import qualified Data.Vect                         as Vc
import qualified Data.Vector                       as V
import qualified Data.Vector.Storable.ByteString   as B
import qualified Foreign                           as F
import qualified Foreign.C.Types                   as F
import qualified GI.Pango                          as GIP
import qualified Graphics.Rendering.Cairo          as GRC
import qualified Graphics.Rendering.Cairo.Internal as GRCI
import qualified LambdaCube.GL                     as GL
import qualified LambdaCube.Compiler               as GL
import qualified LambdaCube.GL.Mesh                as GL
import qualified LambdaCube.GL.Type                as GL
import qualified LambdaCube.Linear                 as LCLin
import           Reflex.GLFW                              (ReflexGLFW, ReflexGLFWCtx, ReflexGLFWGuest, InputU(..))
import qualified System.IO.Unsafe                  as IO

-- Local imports
import           HoloTypes

import           Flatland
import qualified HoloCairo                         as Cr


-- * Drawable identity support

newId ∷ (HasCallStack, MonadIO m) ⇒ m IdToken
newId = liftIO $ do
  tok ← U.newUnique
  trev ALLOC TOK (U.hashUnique tok) (U.hashUnique tok)
  pure $ IdToken tok

blankIdToken'      ∷ IO.IORef IdToken
blankIdToken'      = IO.unsafePerformIO $ IO.newIORef  undefined
blankIdToken'setup ∷ IO ()
blankIdToken'setup = IO.writeIORef blankIdToken' =<< newId
blankIdToken       ∷ IdToken
blankIdToken       = IO.unsafePerformIO $ IO.readIORef blankIdToken'
{-# NOINLINE blankIdToken #-}

tokenHash ∷ IdToken → Int
tokenHash = U.hashUnique ∘ fromIdToken

tokenDesc ∷ IdToken → String
tokenDesc = const ""


-- * Could benefit from:
--
-- updateTM ∷ ∀ t x proxy. Typeable t ⇒ proxy t → (TM.Item x t → TM.Item x t) → TM.TypeMap x → TM.TypeMap x
-- updateTM = (⊥)

mkVIOMap  ∷ (MonadIO m) ⇒ m VIOMap
mkVIOMap  = VIOMap
  <$> (liftIO $ STM.newTVarIO $ TM.empty)

viomapAccess ∷ (MonadIO m) ⇒ VIOMap → m (TM.TypeMap VisualIOMap)
viomapAccess (VIOMap m) = liftIO $ STM.readTVarIO m

viomapReplace ∷ (MonadIO m) ⇒ VIOMap → TM.TypeMap VisualIOMap → m ()
viomapReplace (VIOMap m) tm = liftIO $ STM.atomically $ STM.writeTVar m tm

viomapAdd  ∷ (MonadIO m, Typeable a) ⇒ VIOMap → Proxy a → IdToken → Visual a → m ()
viomapAdd  (VIOMap m) p k v = liftIO $ do
  STM.atomically $ STM.modifyTVar' m $
    \tm→ TM.insert p (Map.insert k v $ fromMaybe Map.empty $ TM.lookup p tm) tm
    -- \tm→ update p (\idm→ Map.insert k v idm) tm

viomapDrop ∷ (MonadIO m, Typeable a) ⇒ VIOMap → Proxy (a ∷ *) → IdToken → m ()
viomapDrop (VIOMap m) p k = liftIO $ do
  STM.atomically $ STM.modifyTVar' m $
    \tm→ TM.insert p (Map.delete k $ fromMaybe Map.empty $ TM.lookup p tm) tm

viomapHas ∷ (MonadIO m, Typeable a) ⇒ VIOMap → Proxy (a ∷ *) → IdToken → m Bool
viomapHas iomap p k = isJust ∘ join ∘ (Map.lookup k <$>) ∘ TM.lookup p <$> viomapAccess iomap


-- | Usher Cairo + Pango -enabled surfaces onto a GL Window,
--   according to user-controlled Settings.

portDΠ ∷ Port → DΠ
portDΠ = sttsDΠ ∘ portSettings

portSetVSync ∷ (MonadIO m) ⇒ Bool → m ()
portSetVSync x = liftIO $ GL.swapInterval $ case x of
                                              True  → 1
                                              False → 0

portCreate  ∷ (MonadIO m) ⇒ GL.Window → Settings → m (Port)
portCreate portWindow portSettings@Settings{..} = do
  liftIO $ case sttsScreenMode of
             Windowed   → let Di (V2 w h) = sttsScreenDim
                          in GL.setWindowSize portWindow w h
             FullScreen → error "XXX: implement fullscreen"
  liftIO $ blankIdToken'setup

  portFontmap                      ← Cr.makeFontMap sttsDΠ Cr.fmDefault sttsFontPreferences
  liftIO $ putStrLn $ printf "%s" (show portFontmap)

  (portRenderer, portObjectStream) ← makeSimpleRenderedStream portWindow (("portStream", "portMtl") ∷ (ObjArrayNameS, UniformNameS))
  rendererDrawFrame portRenderer PipeDraw
  liftIO $ GL.swapBuffers portWindow
  portSetVSync False

  portVisualTracker@(VisualTracker _)     ← VisualTracker <$> mkVIOMap
  pure Port{..}

portUpdateSettings ∷ (MonadIO m) ⇒ Port → Settings → m (Port)
portUpdateSettings port portSettings = do
  pure $ port { portSettings = portSettings }

portNextFrame ∷ (MonadIO m) ⇒ Port → m Frame
portNextFrame Port{..} = do
  rendererDrawFrame  portRenderer PipeDraw
  liftIO $ GL.swapBuffers portWindow
  rendererSetupFrame portRenderer

portFont  ∷ Port → Cr.FontKey → Maybe (Cr.Font Found PU)
portFont Port{..} = Cr.lookupFont portFontmap

portFont' ∷ Port → Cr.FontKey → Cr.Font Found PU
portFont' po fk = portFont po fk
  & fromMaybe (Cr.errorMissingFontkey fk)

-- * Pipelinistan
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
      LB.writeFile (pipelineSrc <> ".json") (AE.encodePretty ppl)
      _ ← printTimeDiff "-- binding GPU pipeline to GL storage (GL.setStorage)... " $
        GL.setStorage renderer storage
      pure renderer

portWindowSize ∷ (MonadIO m) ⇒ GL.Window → m (Di Int)
portWindowSize win = liftIO (GL.getFramebufferSize win)
                     <&> uncurry (Di .: V2)

portSetupFrame ∷ (MonadIO m) ⇒ Port → m Frame
portSetupFrame Port{..} = liftIO $ do
  let slotU           = GL.uniformSetter portGLStorage
      overbrightBits  = 0
  GL.uniformFloat "identityLight" slotU $ 1 / (2 ^ (overbrightBits ∷ Int)) -- used by lc:mkColor
  -- XXX: shadow size computation
  dim@(Di (V2 screenW screenH)) ← portWindowSize portWindow
  GL.setScreenSize portGLStorage (fromIntegral screenW) (fromIntegral screenH)
  pure $ Frame dim

portDrawFrame ∷ (MonadIO m) ⇒ Port → PipeName → m ()
portDrawFrame Port{..} name = liftIO $ do
  GL.renderFrame ∘ Data.Maybe.fromJust $ Map.lookup name portPipelines

portPipeline ∷ Port → PipeName → Maybe GL.GLRenderer
portPipeline Port{..} = flip Map.lookup portPipelines

portShutdown ∷ (MonadIO m) ⇒ Port → m ()
portShutdown Port{..} = liftIO $ do
  _ ← traverse GL.disposeRenderer portPipelines
  GL.destroyWindow portWindow


defaultSettings ∷ (MonadIO m) ⇒ m Settings
defaultSettings = do
  let sttsDΠ ∷ DΠ         = 96
      sttsFontPreferences = Cr.FontPreferences
        [ ("default",     Left $ Cr.Alias "defaultMono" )
        , ("defaultSans", Right $ [ Cr.FontSpec "Bitstream Charter" "Regular" $ Cr.Outline (PUs 16)
                                  , Cr.FontSpec "Aurulent Sans"     "Regular" $ Cr.Outline (PUs 16) ])
        , ("defaultMono", Right $ [ Cr.FontSpec "Terminus"          "Regular" $ Cr.Bitmap  (PUs 15) LT ])
        ]
      sttsScreenMode      = Windowed
      sttsScreenDim       = di 800 600
  pure Settings{..}


-- | A Pango/Cairo-capable 'Drawable' to display in a qPort.

imageSurfaceGetPixels' :: GRC.Surface → IO (F.Ptr F.CUChar, V2 Int)
imageSurfaceGetPixels' pb = do
  pixPtr ← GRCI.imageSurfaceGetData pb
  when (pixPtr ≡ F.nullPtr) $ do
    fail "imageSurfaceGetPixels: image surface not available"
  h ← GRC.imageSurfaceGetHeight pb
  r ← GRC.imageSurfaceGetStride pb
  return (pixPtr, V2 r h)

portMakeDrawable ∷ (MonadIO m) ⇒ Port → IdToken → Di Double → m Drawable
portMakeDrawable Port{..} = makeDrawable portObjectStream

makeDrawable ∷ (HasCallStack, MonadIO m) ⇒ ObjectStream → IdToken → Di Double → m Drawable
makeDrawable dObjectStream@ObjectStream{..} ident dDi' = liftIO $ do
  let dDi@(Di (V2 w h)) = fmap ceiling dDi'
  unless (w * h ≢ 0) $
    error $ printf "makeDrawable: zero dimensions are not acceptable: %s" (show dDi')
  dSurface      ← GRC.createImageSurface GRC.FormatARGB32 w h
  dCairo        ← Cr.cairoCreate  dSurface
  dGIC          ← Cr.cairoToGICairo dCairo

  let (dx, dy) = (fromIntegral w, fromIntegral $ -h)
      -- position = V.fromList [ LCLin.V3  0 dy 0, LCLin.V3  0  0 0, LCLin.V3 dx  0 0, LCLin.V3  0 dy 0, LCLin.V3 dx  0 0, LCLin.V3 dx dy 0 ]
      position = V.fromList [ LCLin.V2  0 dy,   LCLin.V2  0  0,   LCLin.V2 dx  0,   LCLin.V2  0 dy,   LCLin.V2 dx  0,   LCLin.V2 dx dy ]
      texcoord = V.fromList [ LCLin.V2  0  1,   LCLin.V2  0  0,   LCLin.V2  1  0,   LCLin.V2  0  1,   LCLin.V2  1  0,   LCLin.V2  1  1 ]
      ids      = V.fromList $ L.replicate 6 $ fromIntegral (tokenHash ident)
      dMesh    = LC.Mesh { mPrimitive  = P_Triangles
                         , mAttributes = Map.fromList [ ("position",  A_V2F position)
                                                      , ("uv",        A_V2F texcoord)
                                                      , ("id",        A_Int ids)] }
  dGPUMesh      ← GL.uploadMeshToGPU dMesh
  -- XXX: this is leaky -- has to be done manually
  -- SMem.addFinalizer dGPUMesh $ do
  --   GL.disposeMesh dGPUMesh
  dGLObject     ← GL.addMeshToObjectArray osStorage (fromOANS osObjArray) [unameStr osUniform, "viewProj"] dGPUMesh

  dSurfaceData  ← imageSurfaceGetPixels' dSurface
  dTexId        ← F.alloca $! \pto → glGenTextures 1 pto >> F.peek pto
  trev ALLOC TEX (dDi^.di'v) (fromIntegral dTexId)

  -- dTexture      ← uploadTexture2DToGPU'''' False False False False $ (fromWi dStridePixels, h, GL_BGRA, pixels)
  pure Drawable{..}

disposeDrawable ∷ (HasCallStack, MonadIO m) ⇒ ObjectStream → Drawable → m ()
disposeDrawable ObjectStream{..} Drawable{..} = liftIO $ do
  -- see experiment in LCstress
  trev FREE TEX (dDi^.di'v) (fromIntegral dTexId)
  F.withArray [dTexId] $ glDeleteTextures 1 -- release tex id
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

-- * From lambdacube-quake3
--
vec4ToV4F ∷ Vec4 → LCLin.V4F
vec4ToV4F (Vc.Vec4 x y z w) = LCLin.V4 x y z w

vec3ToV3F ∷ Vec3 → LCLin.V3F
vec3ToV3F (Vec3 x y z) = LCLin.V3 x y z

mat4ToM44F ∷ Mat4 → LCLin.M44F
mat4ToM44F (Mat4 a b c d) = LCLin.V4 (vec4ToV4F a) (vec4ToV4F b) (vec4ToV4F c) (vec4ToV4F d)

rotationEuler ∷ Vec3 → Vc.Proj4
rotationEuler (Vec3 a b c) = Vc.orthogonal $ Vc.toOrthoUnsafe $ Vc.rotMatrixZ a Vc..*. Vc.rotMatrixX b Vc..*. Vc.rotMatrixY (-c)

-- * Look up the visual and, if present, check compatibility with
--   new requirements: style generation and size.
--   XXX: violates abstraction (fiddles with port and Holo's visual at the same time)
--   XXX: not thread-safe
visualiseHoloitem ∷ (HasCallStack, MonadIO m) ⇒ Port → Item PLayout → [Item PVisual] → m (Item PVisual)
visualiseHoloitem port@Port{..} hi children = case hi of
  Item{..} → do
    let dim@(Di (V2 w h)) = hiArea^.area'b.size'di
        mkVisual ∷ (MonadIO m, Holo a) ⇒ a → Style a → m (Visual a)
        mkVisual holo hiStyle = do
          newDrawable ← if (w * h ≢ 0)
                        then Just <$> makeDrawable portObjectStream hiToken dim
                        else pure Nothing
          Visual
                 <$> sequence ((\drw→ createVisual port (_sStyle hiStyle) hiArea drw holo) <$> newDrawable)
                 <*> pure (_sStyleGene $ hiStyle)
                 <*> pure newDrawable
    visMap ← viomapAccess (fromVT portVisualTracker)
    if not $ hiHasVisual hi
    then pure Item{hiVisual=Nothing, hiChildren=children, ..}
    else do
      -- XXX:                ..why is..                ..this.. the correctly-typed Proxy?
      (vis, updated ∷ Bool)
            ← case join $ Map.lookup hiToken <$> TM.lookup Proxy visMap of
              Nothing → do
                trev MISSALLOC VIS (w, h, _fromStyleGene $ _sStyleGene $ hiStyle) (tokenHash hiToken)
                (,True) <$> mkVisual holo hiStyle
              Just v@Visual{..} →
                let vDi          = fromMaybe zero $ dDi <$> vDrawable
                    styleChanged = vStyleGene ≢ hiStyleGene hi
                    sizeChanged  = vDi ≢ (ceiling <$> dim)
                    update       = sizeChanged ∨ styleChanged
                in if update
                then do
                  trev REALLOC VIS (w, h) (tokenHash hiToken)
                  sequence $ flip freeVisualOf Proxy <$> vVisual
                  sequence $ disposeDrawable portObjectStream <$> vDrawable
                  (,True) <$> mkVisual holo hiStyle
                else do
                  trev REUSE   VIS (w, h) (tokenHash hiToken)
                  pure $ (v, False)
      when updated $
        viomapAdd (fromVT portVisualTracker) Proxy hiToken vis
      pure Item{hiVisual=Just vis, hiChildren=children, ..}

portGarbageCollectVisuals ∷ (MonadIO m) ⇒ Port → Map.Map IdToken (Item a) → m ()
portGarbageCollectVisuals Port{..} validLeaves = do
  visMap ← viomapAccess $ fromVT portVisualTracker
  let gcTM ∷ MonadIO m ⇒ Proxy b -> Map.Map IdToken (Visual b) -> m (Map.Map IdToken (Visual b))
      gcTM proxy vismap = do
        let used   = Map.intersection vismap validLeaves
            unused = Map.difference   vismap used
        _ ← flip Map.traverseWithKey unused $ \_ Visual{..}→ do
          -- printf "releasing Visual for IdToken %s\n" (show $ U.hashUnique $ fromIdToken k)
          sequence $ flip freeVisualOf proxy <$> vVisual
          -- flip (trev FREE VIS (tokenHash k)) $
          --   case vDrawable of
          --     Nothing → (0, 0)
          --     Just vDrawable → (dDi vDrawable^.di'w, dDi vDrawable^.di'h)
          sequence $ disposeDrawable portObjectStream <$> vDrawable
        pure used
  visMap' ← TM.traverse gcTM visMap
  viomapReplace (fromVT portVisualTracker) visMap'

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


-- * Matrix works
--
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
orthoLU w h = HoloPort.ortho 0 w h 0 1 (-1)
orthoLB w h = HoloPort.ortho 0 w 0 h 1 (-1)

-- | To screen space conversion matrix.
screenM :: Int → Int → Mat4
screenM w h = scaleM -- Vc..*. flipM
  where (fw, fh) = (fromIntegral w, fromIntegral h)
        scaleM = Vc.Mat4 (Vc.Vec4 (1/fw)  0     0 0)
                         (Vc.Vec4  0     (1/fh) 0 0)
                         (Vc.Vec4  0      0     1 0)
                         (Vc.Vec4  0      0     0 0.5) -- where does that 0.5 factor COMEFROM?


-- * Actual drawing
--
dpx ∷ Po Double → Co Double → GRCI.Render ()
dpx (Po (V2 x y)) color = Cr.crColor color >>
                          -- GRC.rectangle (x) (y) 1 1 >> GRC.fill
                          GRC.rectangle (x-1) (y-1) 3 3 >> GRC.fill

clearDrawable ∷ (MonadIO m) ⇒ Drawable → m ()
clearDrawable Drawable{..} = do
  Cr.runCairo dCairo $ do
    GRC.save
    GRC.setOperator GRCI.OperatorSource
    Cr.crColor (co 0 0 0 0)
    GRC.paint
    GRC.restore

drawableDrawRect ∷ (MonadIO m, FromUnit u) ⇒ Port → Drawable → Co Double → Di (Unit u) → m ()
drawableDrawRect Port{portSettings=Settings{..}} d@Drawable{..} color dim' = do
  let dim = fromUnit sttsDΠ <$> dim'
  Cr.runCairo dCairo $ do
    Cr.crColor color
    GRC.rectangle (0) (0) (fromPU $ dim^.di'v._x) (fromPU $ dim^.di'v._y)
    GRC.fill
  drawableContentToGPU d
  pure ()

-- Render with: framePutDrawable frame px0 (doubleToFloat <$> po 0 0)
-- mkRectDrawable ∷ (MonadIO m, FromUnit u) ⇒ Port → Di (Unit u) → Co Double → m Drawable
-- mkRectDrawable port@Port{portSettings=Settings{..}} dim color = do
--   d@Drawable{..} ← portMakeDrawable port $ fromPU ∘ fromUnit sttsDΠ <$> dim
--   drawableDrawRect port d color dim
--   pure d

drawableBindFontLayout ∷ (MonadIO m, FromUnit u, FromUnit v) ⇒
  DΠ → Drawable → Cr.Font Found u → Di (Unit v) → Cr.TextSizeSpec v → m (Cr.WFont Bound, GIP.Layout)
drawableBindFontLayout dπ Drawable{..} = Cr.bindWFontLayout dπ dGIC

drawableDrawText ∷ (MonadIO m) ⇒ Drawable → GIP.Layout → Co Double → T.Text → m ()
drawableDrawText Drawable{..} layout color text = do
  Cr.layDrawText dCairo dGIC layout (po 0 0) color text

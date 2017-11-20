{-# LANGUAGE GADTs, StrictData #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns, LambdaCase, OverloadedStrings, PackageImports, RecordWildCards, ScopedTypeVariables, TupleSections, TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Wall #-}

module HoloCube
  ( ObjectStream(..), Renderer(..)
  , makeSimpleRenderedStream
  , rendererShutdown
  , ObjArrayNameS(..)
  , UniformNameS(..), unameStr
  , Frame(..)
  , rendererSetupFrame, rendererDrawFrame
  --
  , uploadTexture2DToGPU''''
  )
where

-- Basis
import           HoloPrelude
import           Prelude                           hiding ((.), id)

-- Generic
import qualified Data.ByteString.Char8             as SB
import qualified Data.ByteString.Lazy              as LB
import           Data.Map                                 (Map)
import qualified Data.Map                          as Map
import qualified Data.Maybe

-- Algebra
import           Linear

-- Serialization
import qualified Data.Aeson                        as AE

-- System
import qualified System.Directory                  as FS

-- Misc
import           Text.Show.Pretty                         (ppShow)

-- Dirty stuff
import qualified Foreign.C.Types                   as F
import qualified Foreign                           as F

-- Window system (..hello WIndowSys..)
import "GLFW-b"  Graphics.UI.GLFW                  as GLFW

-- …
import           Graphics.GL.Core33                as GL

-- LambdaCube
import qualified GameEngine.Utils                  as Q3
import qualified LambdaCube.Compiler               as LCC
import qualified LambdaCube.GL                     as GL
import qualified LambdaCube.GL.Type                as GL

-- Local imports
import           Flatland


-- | Render context for all objects with the same GL pipeline.
data ObjectStream where
  ObjectStream ∷
    { osStorage  ∷ GL.GLStorage
    , osObjArray ∷ ObjArrayNameS
    , osUniform  ∷ UniformNameS
    } → ObjectStream

newtype UniformNameS  = UniformNameS  { fromUNS  ∷ SB.ByteString } deriving (Eq, IsString, Ord, Show)
newtype ObjArrayNameS = ObjArrayNameS { fromOANS ∷ String }        deriving (Eq, IsString, Ord, Show)

unameStr ∷ UniformNameS → String
unameStr = SB.unpack ∘ fromUNS


-- * Pipelinistan
pipelineSchema ∷ [(ObjArrayNameS, UniformNameS)] → GL.PipelineSchema
pipelineSchema schemaPairs =
  let arrays   = fromOANS ∘ view _1            <$> schemaPairs
      textures = SB.unpack ∘ fromUNS ∘ view _2 <$> schemaPairs
      simplePosUVSchema =
        GL.ObjectArraySchema GL.Triangles $ Map.fromList
        [ ("position",       GL.Attribute_V2F)
        , ("uv",             GL.Attribute_V2F) ]
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

compilePipeline ∷ (MonadIO m) ⇒ FilePath → m Bool
compilePipeline jsonOutput = liftIO $ Q3.printTimeDiff "-- compiling graphics pipeline... " $ do
  let pipelineSrc = "Holotype.lc"
  LCC.compileMain ["lc"] LCC.OpenGL33 pipelineSrc >>= \case
    Left  err → printf "-- error compiling %s:\n%s\n" pipelineSrc (ppShow err) >> return False
    Right ppl → LB.writeFile jsonOutput (AE.encode ppl)                   >> return True

bindPipeline ∷ (MonadIO m) ⇒ GL.GLStorage → String → m (Maybe GL.GLRenderer)
bindPipeline storage pipelineJSON = liftIO $ do
    putStrLn $ "-- reading GPU pipeline from " ++ pipelineJSON
    let paths = [pipelineJSON]
    validPaths ← filterM FS.doesFileExist paths
    when (Prelude.null validPaths) $
      fail $ "GPU pipeline " ++ pipelineJSON ++ " couldn't be found in " ++ show paths
    renderer ← Q3.printTimeDiff "-- allocating GPU pipeline (GL.allocRenderer)... " $ do
      AE.eitherDecode <$> LB.readFile (Prelude.head validPaths) >>= \case
        Left err  → fail err
        Right ppl → GL.allocRenderer ppl
    _ ← Q3.printTimeDiff "-- binding GPU pipeline to GL storage (GL.setStorage)... " $ GL.setStorage renderer storage
    return $ Just renderer


-- | A 'Renderer' provides 'ObjArrayNameS'-named streams of flat objects,
--   textured with a corresponding, 'UniformNameS'-named texture.
data Renderer where
  Renderer ∷
    { rGLStorage  ∷ GL.GLStorage
    , rGLRenderer ∷ GL.GLRenderer
    , rStreams    ∷ Map (ObjArrayNameS, UniformNameS) ObjectStream
    , rWindow     ∷ GLFW.Window
    } → Renderer

-- | A GL 'Frame'.
data Frame where
  Frame ∷
    { fDim ∷ Di Int
    } → Frame

-- | Setup a 'Renderer', with streams where 'Canvas' objects have be attached,
--   to be put on screen.
--   'ous' is a list of object array/texture uniform name pairs, that have to be
--   recognized by the Lambdacube pipeline.
--   A GL context must have already been set up in 'IO', with f.e. 'makeGLWindow'.
makeRenderer ∷ (MonadIO m) ⇒ GLFW.Window → [(ObjArrayNameS, UniformNameS)] → m (Either String Renderer)
makeRenderer rWindow ous = liftIO $ do
    let schema = pipelineSchema ous

    rGLStorage ← GL.allocStorage schema
    let rStreams = Map.fromList [ (k, ObjectStream rGLStorage oa un')
                                | k@(oa, un') ← ous ]

    let pipelineJSON = "Holotype.json"
    success ← compilePipeline pipelineJSON
    unless success $
      fail "FATAL: failed to compile the GPU pipeline."
    renderer' ← bindPipeline rGLStorage pipelineJSON
    unless (Data.Maybe.isJust renderer') $
      fail "FATAL: failed to bind the compiled GPU pipeline."
    let rGLRenderer = Data.Maybe.fromJust renderer'

    mFailure ← GL.setStorage rGLRenderer rGLStorage
    pure $ case mFailure of -- XXX/expressivity: there ought to be some kind of a standard Maybe-to-Either transform..
      Just failure → Left failure
      Nothing      → Right Renderer{..}

rendererShutdown ∷ (MonadIO m) ⇒ Renderer → m ()
rendererShutdown Renderer{..} = liftIO $ do
  GL.disposeRenderer rGLRenderer
  GLFW.destroyWindow rWindow

rStream ∷ Renderer → (ObjArrayNameS, UniformNameS) → Maybe ObjectStream
rStream Renderer{..} = flip Map.lookup rStreams

makeSimpleRenderedStream ∷ (MonadIO m) ⇒ GLFW.Window → (ObjArrayNameS, UniformNameS) → m (Renderer, ObjectStream)
makeSimpleRenderedStream glWindow streamDesc = do
  rend' ← makeRenderer glWindow [streamDesc]
  let rend@Renderer{..} = case rend' of
                            Left failure → error $ printf "FATAL: failed to create a renderer: %s" failure
                            Right r → r
  let stream = rStream rend streamDesc
               & fromMaybe (error $ "Silly invariant #1 failure.")
  pure (rend, stream)

rendererQueryFrameSize ∷ (MonadIO m) ⇒ Renderer → m (Di Int)
rendererQueryFrameSize Renderer{..} = liftIO $ do
  (screenW, screenH) ← GLFW.getFramebufferSize rWindow
  pure $ di (Wi screenW) (He screenH)

rendererSetupFrame ∷ (MonadIO m) ⇒ Renderer → m Frame
rendererSetupFrame r@Renderer{..} = liftIO $ do
  let slotU           = GL.uniformSetter rGLStorage
      overbrightBits  = 0
  GL.uniformFloat "identityLight" slotU $ 1 / (2 ^ (overbrightBits ∷ Int)) -- used by lc:mkColor
  d@(Di (V2 screenW screenH)) ← rendererQueryFrameSize r
  GL.setScreenSize rGLStorage (fromIntegral screenW) (fromIntegral screenH)
  pure $ Frame d

rendererDrawFrame ∷ (MonadIO m) ⇒ Renderer → m ()
rendererDrawFrame Renderer{..} = liftIO $ do
  GL.renderFrame rGLRenderer
  GLFW.swapBuffers rWindow


-- * Shader attributery (XXX: unused)
-- canvasCommonAttrs ∷ UniformNameS → CommonAttrs
-- canvasCommonAttrs uname =
--   Q3.defaultCommonAttrs
--   { caSort   = 10.0
--   , caStages =
--     [ Q3.defaultStageAttrs
--       { saTexture        = ST_ClampMap ∘ SB.unpack ∘ fromUNS $ uname
--       , saTextureUniform = SB.unpack $ fromUNS uname
--       , saBlend          = Just ( B_SrcAlpha , B_OneMinusSrcAlpha )
--       , saTCGen          = TG_Base
--       , saDepthWrite     = True
--       , saRGBGen         = RGB_IdentityLighting
--       }]}


-- * GL Toolkit
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

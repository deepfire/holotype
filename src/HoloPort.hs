{-# LANGUAGE GADTs, TypeFamilies, TypeFamilyDependencies, TypeInType #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitForAll, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RankNTypes, UndecidableInstances #-}
{-# LANGUAGE LambdaCase, OverloadedStrings, PackageImports, PartialTypeSignatures, RecordWildCards, ScopedTypeVariables, TupleSections, TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Weverything #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors -Wno-missing-import-lists -Wno-implicit-prelude #-}
{-# OPTIONS_GHC -Wno-monomorphism-restriction -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unsafe #-}

module HoloPort where

-- Basis
import           HoloPrelude

-- Types
import qualified Data.Map                          as Map
import qualified Data.Text                         as T
import qualified Data.Vector                       as V
import qualified Data.Vect                         as Vc
import           Data.Vect                                (Mat4(..), Vec3(..))

-- Algebra
import           Linear

-- Manually-bound Cairo
import qualified Graphics.Rendering.Cairo          as GRC
import qualified Graphics.Rendering.Cairo.Internal as GRCI

-- glib-introspection -based Cairo and Pango
import qualified GI.Cairo                          as GIC
import qualified GI.Pango                          as GIP

-- Dirty stuff
import qualified Foreign.C.Types                   as F
import qualified Foreign                           as F
import qualified System.Mem.Weak                   as SMem

-- …
import           Graphics.GL.Core33                as GL
import "GLFW-b"  Graphics.UI.GLFW                  as GL

-- LambdaCube
import qualified LambdaCube.GL                     as GL
import qualified LambdaCube.GL.Mesh                as GL
import qualified LambdaCube.Linear                 as LCLin
import           LambdaCube.Mesh                   as LC

-- LambdaCube Quake III
import           GameEngine.Utils                  as Q3

-- Local imports
import           Flatland
import           HoloCairo
import           HoloCube
import           HoloFont


-- | Usher Cairo + Pango -enabled surfaces onto a GL Window,
--   according to user-controlled Settings.
data Port where
  Port ∷
    { portSettings     ∷ Settings
    , portFontmap      ∷ FontMap PU
    , portWindow       ∷ GL.Window
    , portObjectStream ∷ ObjectStream
    , portRenderer     ∷ Renderer
    } → Port

portDΠ ∷ Port → DΠ
portDΠ = sttsDΠ ∘ portSettings

portCreate  ∷ (MonadIO m) ⇒ GL.Window → Settings → m (Port)
portCreate portWindow portSettings@Settings{..} = do
  portFontmap                      ← makeFontMap sttsDΠ fmDefault sttsFontPreferences
  (portRenderer, portObjectStream) ← makeSimpleRenderedStream portWindow (("portStream", "portMtl") ∷ (ObjArrayNameS, UniformNameS))
  rendererDrawFrame portRenderer
  pure Port{..}

portUpdateSettings ∷ (MonadIO m) ⇒ Port → Settings → m (Port)
portUpdateSettings port portSettings = do
  pure $ port { portSettings = portSettings }

portNextFrame ∷ (MonadIO m) ⇒ Port → m Frame
portNextFrame Port{..} = do
  rendererDrawFrame  portRenderer
  rendererSetupFrame portRenderer

portFont  ∷ Port → FontKey → Maybe (Font Found PU)
portFont Port{..} = lookupFont portFontmap

portFont' ∷ Port → FontKey → Font Found PU
portFont' po fk = portFont po fk
  & fromMaybe (errorMissingFontkey fk)


data Settings where
  Settings ∷
    { sttsDΠ              ∷ DΠ
    , sttsFontPreferences ∷ FontPreferences PU
    } → Settings
    deriving (Eq, Show)

defaultSettings ∷ (MonadIO m) ⇒ m Settings
defaultSettings = do
  let sttsDΠ ∷ DΠ         = 96
      sttsFontPreferences = FontPreferences
        [ ("default",     Left $ Alias "defaultMono" )
        , ("defaultSans", Right $ [ FontSpec "Aurulent Sans" "Regular" $ FSROutline (PUs 16) ])
        , ("defaultMono", Right $ [ FontSpec "Terminus"      "Regular" $ FSRBitmap  (PUs 16) LT ])
        ]
  pure Settings{..}


-- | A Pango/Cairo-capable 'Drawable' to display in a qPort.
data Drawable where
  Drawable ∷
    { dObjectStream ∷ ObjectStream
    , dDi           ∷ Di Int
    , dSurfaceData  ∷ (F.Ptr F.CUChar, (Int, Int))
    , dCairo        ∷ Cairo
    , dGIC          ∷ GIC.Context
    --
    , dMesh         ∷ LC.Mesh
    , dGPUMesh      ∷ GL.GPUMesh
    , dGLObject     ∷ GL.Object
    , dTexId        ∷ GLuint
    } → Drawable

imageSurfaceGetPixels' :: GRC.Surface → IO (F.Ptr F.CUChar, (Int, Int))
imageSurfaceGetPixels' pb = do
  pixPtr ← GRCI.imageSurfaceGetData pb
  when (pixPtr ≡ F.nullPtr) $ do
    fail "imageSurfaceGetPixels: image surface not available"
  h ← GRC.imageSurfaceGetHeight pb
  r ← GRC.imageSurfaceGetStride pb
  return (pixPtr, (r, h))

makeDrawable ∷ (MonadIO m) ⇒ Port → Di Double → m Drawable
makeDrawable Port {portObjectStream = dObjectStream@ObjectStream{..}} dDi' = liftIO $ do
  let dDi@(Di (V2 w h)) = fmap ceiling dDi'
  dSurface      ← GRC.createImageSurface GRC.FormatARGB32 w h
  dCairo        ← cairoCreate  dSurface
  dGIC          ← cairoToGICairo dCairo

  let (dx, dy) = (fromIntegral w, fromIntegral $ -h)
      -- position = V.fromList [ LCLin.V3  0 dy 0, LCLin.V3  0  0 0, LCLin.V3 dx  0 0, LCLin.V3  0 dy 0, LCLin.V3 dx  0 0, LCLin.V3 dx dy 0 ]
      position = V.fromList [ LCLin.V2  0 dy,   LCLin.V2  0  0,   LCLin.V2 dx  0,   LCLin.V2  0 dy,   LCLin.V2 dx  0,   LCLin.V2 dx dy ]
      texcoord = V.fromList [ LCLin.V2  0  1,   LCLin.V2  0  0,   LCLin.V2  1  0,   LCLin.V2  0  1,   LCLin.V2  1  0,   LCLin.V2  1  1 ]
      dMesh    = LC.Mesh { mPrimitive  = P_Triangles
                         , mAttributes = Map.fromList [ ("position",  A_V2F position)
                                                      , ("uv",        A_V2F texcoord) ] }
  dGPUMesh      ← GL.uploadMeshToGPU dMesh
  SMem.addFinalizer dGPUMesh $
    GL.disposeMesh dGPUMesh
  dGLObject     ← GL.addMeshToObjectArray osStorage (fromOANS osObjArray) [unameStr osUniform, "viewProj"] dGPUMesh

  dSurfaceData  ← imageSurfaceGetPixels' dSurface
  dTexId        ← F.alloca $! \pto → glGenTextures 1 pto >> F.peek pto

  -- dTexture      ← uploadTexture2DToGPU'''' False False False False $ (fromWi dStridePixels, h, GL_BGRA, pixels)
  pure Drawable{..}

drawableContentToGPU ∷ (MonadIO m) ⇒ Drawable → m ()
drawableContentToGPU Drawable{..} = liftIO $ do
  let ObjectStream{..} = dObjectStream

  let (pixels, (strideBytes, pixelrows)) = dSurfaceData
  cTexture ← uploadTexture2DToGPU'''' False False False False (strideBytes `div` 4, pixelrows, GL_BGRA, pixels) dTexId

  GL.updateObjectUniforms dGLObject $ do
    fromUNS osUniform GL.@= return cTexture

framePutDrawable ∷ (MonadIO m) ⇒ Frame → Drawable → Po Float → m ()
framePutDrawable (Frame (Di (V2 screenW screenH))) Drawable{..} (Po (V2 x y)) = do
  let cvpos     = Vec3 x y 0
      toScreen  = screenM screenW screenH
  liftIO $ GL.uniformM44F "viewProj" (GL.objectUniformSetter $ dGLObject) $
    Q3.mat4ToM44F $! (Vc.fromProjective $! Vc.translation cvpos) Vc..*. toScreen


-- * Drawables and fonts
--
drawableBindFontLayout ∷ (MonadIO m, FromUnit u, FromUnit v) ⇒
  DΠ → Drawable → Font Found u → Di (Unit v) → TextSizeSpec v → m (WFont Bound, GIP.Layout)
drawableBindFontLayout dπ Drawable{..} = bindWFontLayout dπ dGIC

drawableDrawText ∷ (MonadIO m) ⇒ Drawable → GIP.Layout → Co Double → T.Text → m ()
drawableDrawText Drawable{..} layout color text = do
  layDrawText dCairo dGIC layout (po 0 0) color text


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
screenM w h =
  Vc.Mat4 (Vc.Vec4 (1/fw)  0     0 0)
          (Vc.Vec4  0     (1/fh) 0 0)
          (Vc.Vec4  0      0     1 0)
          (Vc.Vec4  0      0     0 0.5) -- where does that 0.5 factor COMEFROM?
  where (fw, fh) = (fromIntegral w, fromIntegral h)


-- * Render debug utils
--
dpx ∷ Po Double → Co Double → GRCI.Render ()
dpx (Po (V2 x y)) color = crColor color >>
                          -- GRC.rectangle (x) (y) 1 1 >> GRC.fill
                          GRC.rectangle (x-1) (y-1) 3 3 >> GRC.fill

redrawRectDrawable ∷ (MonadIO m, FromUnit u) ⇒ Port → Drawable → Co Double → Di (Unit u) → m ()
redrawRectDrawable Port{portSettings=Settings{..}} d@Drawable{..} color dim' = do
  let dim = fromUnit sttsDΠ <$> dim'
  runCairo dCairo $ do
    crColor color
    GRC.rectangle (0) (0) (fromPU $ dim^.di'v._x) (fromPU $ dim^.di'v._y)
    GRC.fill
  drawableContentToGPU d
  pure ()

-- Render with: framePutDrawable frame px0 (doubleToFloat <$> po 0 0)
mkRectDrawable ∷ (MonadIO m, FromUnit u) ⇒ Port → Di (Unit u) → Co Double → m Drawable
mkRectDrawable port@Port{portSettings=Settings{..}} dim color = do
  d@Drawable{..} ← makeDrawable port $ fromPU ∘ fromUnit sttsDΠ <$> dim
  redrawRectDrawable port d color dim
  pure d

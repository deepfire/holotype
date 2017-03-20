-- Repro for https://github.com/lambdacube3d/lambdacube-gl/issues/10
-- Usage:
--   ghc -threaded -eventlog -rtsopts -isrc --make LCstress.hs
--   ./LCstress +RTS -T -ls -N2
--
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Wall #-}

import           Control.Concurrent                       (threadDelay)
import           Control.Lens
import           Control.Monad                            (filterM, when)
import           Control.Monad.IO.Class                   (MonadIO, liftIO)
import qualified Data.Aeson                        as AE
import qualified Data.ByteString.Char8             as SB
import qualified Data.ByteString.Lazy              as LB
import           Data.List
import qualified Data.Map                          as Map
import           Data.Maybe
import           Data.String
import qualified Data.Vector                       as V

import qualified Foreign.Ptr                       as F
import qualified Foreign.ForeignPtr                as F

import qualified Data.GI.Base                      as GI
import qualified GI.Cairo                          as GIC
import qualified GI.PangoCairo.Functions           as GIPC
import qualified Graphics.Rendering.Cairo          as GRC
import qualified Graphics.Rendering.Cairo.Types    as GRC
import qualified Graphics.Rendering.Cairo.Internal as GRCI (create) --,destroy, imageSurfaceCreate, unCairo

import qualified Graphics.GL.Core33                as GL
import qualified "GLFW-b" Graphics.UI.GLFW         as GLFW

import qualified LambdaCube.Compiler               as LCC
import qualified LambdaCube.GL                     as GL
import qualified LambdaCube.GL.Mesh                as GL
import qualified LambdaCube.Linear                 as LCLin
import           LambdaCube.Mesh                   as LC

import qualified GameEngine.Utils                  as Q3

import           Text.Printf                              (printf)
import           Text.Show.Pretty                         (ppShow)

import qualified System.Clock                      as Sys
import qualified System.Directory                  as FS
import qualified System.IO                         as Sys
import qualified System.Mem                        as Sys
import qualified GHC.Stats                         as Sys

import qualified System.Mem.Weak                   as SMem


foreign import ccall "&cairo_destroy"
  cairo_destroy ∷ F.FinalizerPtr GRC.Cairo

main ∷ IO ()
main = do
  Sys.hSetBuffering Sys.stdout Sys.NoBuffering
  _ ← GLFW.init
  GLFW.defaultWindowHints
  mapM_ GLFW.windowHint
    [ GLFW.WindowHint'ContextVersionMajor 3
    , GLFW.WindowHint'ContextVersionMinor 3
    , GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
    , GLFW.WindowHint'OpenGLForwardCompat True
    ]
  Just win ← GLFW.createWindow 1024 768 "repro" Nothing Nothing
  GLFW.makeContextCurrent $ Just win
  GL.glEnable GL.GL_FRAMEBUFFER_SRGB
  GLFW.swapInterval 0

  osStorage ← GL.allocStorage $ pipelineSchema [("canvasStream", "canvasMtl")]

  let pipelineJSON = "Holotype.json"
      pipelineSrc  = "Holotype.lc"

  _ ← LCC.compileMain ["lc"] LCC.OpenGL33 pipelineSrc >>= \case
    Left  err → fail "-- error compiling %s:\n%s\n" pipelineSrc (ppShow err) >> return False
    Right ppl → LB.writeFile pipelineJSON (AE.encode ppl)                    >> return True
  let paths = [pipelineJSON]
  validPaths ← filterM FS.doesFileExist paths
  when (Prelude.null validPaths) $
    fail $ "GPU pipeline " ++ pipelineJSON ++ " couldn't be found in " ++ show paths

  renderer ← Q3.printTimeDiff "-- allocating GPU pipeline (GL.allocRenderer)... " $ do
    AE.eitherDecode <$> LB.readFile (Prelude.head validPaths) >>= \case
      Left err  → fail err
      Right ppl → GL.allocRenderer ppl

  _ ← GL.setStorage renderer osStorage <&>
    fromMaybe (error $ printf "setStorage failed")
  timeStart           ← getTime
  let memoryUsage = Sys.currentBytesUsed <$> Sys.getGCStats
  let (w, h) = (1, 1)
      navg = 10
      loop (iterN, timePre) avgPre preKB = do
        let (dx, dy)  = (fromIntegral w, fromIntegral $ -h)
            position   = V.fromList [ LCLin.V2  0 dy,   LCLin.V2  0  0,   LCLin.V2 dx  0,   LCLin.V2  0 dy,   LCLin.V2 dx  0,   LCLin.V2 dx dy ]
            texcoord   = V.fromList [ LCLin.V2  0  1,   LCLin.V2  0  0,   LCLin.V2  1  0,   LCLin.V2  0  1,   LCLin.V2  1  0,   LCLin.V2  1  1 ]
            dMesh      = LC.Mesh { mPrimitive  = P_Triangles
                                  , mAttributes = Map.fromList [ ("position",  A_V2F position)
                                                               , ("uv",        A_V2F texcoord) ] }

        dGPUMesh      ← GL.uploadMeshToGPU dMesh
        SMem.addFinalizer dGPUMesh $
          GL.disposeMesh dGPUMesh
        dGLObject     ← GL.addMeshToObjectArray osStorage "canvasStream" ["canvasMtl"] dGPUMesh
        SMem.addFinalizer dGLObject $
          GL.removeObject osStorage dGLObject

        timePreGC ← getTime
        gc
        new       ← gcKBytesUsed
        timePost  ← getTime
        let dt     = timePost  - timePre
            nonGCt = timePreGC - timePre
            avgPost@(avgVal, _) = avgStep dt avgPre
        when (preKB /= new) $
          printf "%5dn %dk %4ddK %5dK/f  %4.2fms, %4.2fms nonGC\n"
                 iterN new (new - preKB) (ceiling $ (fromIntegral new / fromIntegral iterN) ∷ Int)
                 (avgVal * 1000) (nonGCt * 1000)
        loop (iterN + 1, timePost) avgPost new
  loop (0 ∷ Integer, timeStart) (0.0, (navg, 0, [])) =<< gcKBytesUsed

newtype UniformNameS  = UniformNameS  { fromUNS  ∷ SB.ByteString } deriving (Eq, IsString, Ord, Show)
newtype ObjArrayNameS = ObjArrayNameS { fromOANS ∷ String }        deriving (Eq, IsString, Ord, Show)
pipelineSchema ∷ [(ObjArrayNameS, UniformNameS)] → GL.PipelineSchema
pipelineSchema schemaPairs =
  let arrays   = fromOANS . view _1            <$> schemaPairs
      textures = SB.unpack . fromUNS . view _2 <$> schemaPairs
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

timespecToSecs ∷ Sys.TimeSpec → Double
timespecToSecs = (/ 1000000000.0) . fromIntegral . Sys.toNanoSecs

getTime ∷ IO Double
getTime = timespecToSecs <$> Sys.getTime Sys.Monotonic

gc ∷ IO ()
gc = liftIO $ Sys.performGC

gcKBytesUsed ∷ (MonadIO m) ⇒ m Integer
gcKBytesUsed = liftIO $ (`div` 1024) . fromIntegral . Sys.currentBytesUsed <$> Sys.getGCStats

type Avg a = (Int, Int, [a])
avgStep ∷ Fractional a ⇒ a → (a, Avg a) → (a, Avg a)
avgStep x (_, (lim, cur, xs)) =
  let (ncur, nxs) = if cur < lim
                    then (cur + 1, x:xs)
                    else (lim,     x:Data.List.init xs)
  in ((sum nxs) / fromIntegral ncur, (lim, ncur, nxs))

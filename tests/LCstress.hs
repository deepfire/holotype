-- Repro for https://github.com/lambdacube3d/lambdacube-gl/issues/10
-- Usage:
--   ghc -threaded -eventlog -rtsopts -isrc --make LCstress.hs && ./LCstress +RTS -T -ls -N2
--
{-# LANGUAGE BangPatterns #-}
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
{-# OPTIONS_GHC -Wall -Wno-unused-imports -Wno-type-defaults #-}

import           Control.Lens
import           Control.Monad                            (filterM, when, forM_)
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

import           Text.Printf                              (printf)
import           Text.Show.Pretty                         (ppShow)

import qualified System.Clock                      as Sys
import qualified System.Directory                  as FS
import qualified System.Environment                as Sys
import qualified System.IO                         as Sys
import qualified System.Mem                        as Sys
import qualified GHC.Stats                         as Sys

import qualified System.Mem.Weak                   as SMem


import LambdaCube.GL.Type as T
import LambdaCube.GL.Util
import Data.Map (Map)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import LambdaCube.PipelineSchema
import Data.IORef
import Data.Vector (Vector,(//),(!))
import LambdaCube.GL.Input (createObjectCommands, mkUniform)
import LambdaCube.GL.Mesh (GPUMesh(..), GPUData(..))
import HoloPrelude


foreign import ccall "&cairo_destroy"
  cairo_destroy ∷ F.FinalizerPtr GRC.Cairo

data Scenario
  = ManMesh          -- memory reclaimed OK
  | ManMeshObj       -- memory reclaimed OK
  | AutoMesh         -- SIGSEGV on nVidia drivers
  | AutoMeshObj      -- memory usage climbs, ~3k/frame
  deriving (Eq, Read, Show)

experiment ∷ Scenario → Integer → GL.GLStorage → GL.Mesh → IO ()
experiment scen n store dMesh = do
  mesh ← GL.uploadMeshToGPU dMesh

  case scen of
    ManMesh → do
      GL.disposeMesh     mesh
    ManMeshObj → do
      object ← GL.addMeshToObjectArray store "portStream" ["portMtl"] mesh
      GL.removeObject          store  object
      GL.disposeMesh     mesh
    AutoMesh → do
      SMem.addFinalizer  mesh $ do
        GL.disposeMesh   mesh
    AutoMeshObj → do
      object ← GL.addMeshToObjectArray store "portStream" ["portMtl"] mesh
      SMem.addFinalizer               object $ do
        printf "running finalizer, n=%d\n" n
        GL.removeObject        store  object
        GL.disposeMesh   mesh

--- Mostly boring parts below
---
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

main ∷ IO ()
main = do
  args ← Sys.getArgs
  let scen = case args of
        []  → ManMeshObj
        x:_ → read x
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

  glstorage ← GL.allocStorage $ pipelineSchema [("portStream", "portMtl")]

  let pipelineJSON = "Holotype.json"
      pipelineSrc  = "Holotype.lc"

  _ ← LCC.compileMain ["lc"] LCC.OpenGL33 pipelineSrc >>= \case
    Left  err → error "-- error compiling %s:\n%s\n" pipelineSrc (ppShow err) >> return False
    Right ppl → LB.writeFile pipelineJSON (AE.encode ppl)                    >> return True
  let paths = [pipelineJSON]
  validPaths ← filterM FS.doesFileExist paths
  when (Prelude.null validPaths) $
    error $ "GPU pipeline " ++ pipelineJSON ++ " couldn't be found in " ++ show paths

  renderer ← printTimeDiff "-- allocating GPU pipeline (GL.allocRenderer)... " $ do
    AE.eitherDecode <$> LB.readFile (Prelude.head validPaths) >>= \case
      Left err  → error err
      Right ppl → GL.allocRenderer ppl

  _ ← GL.setStorage renderer glstorage <&>
    fromMaybe (error $ printf "setStorage failed")
  timeStart           ← getTime
  let (w, h) = (1, 1)
      navg = 10
      loop (iterN, timePre) avgPre preKB = do
        let (dx, dy)   = (fromIntegral w, fromIntegral $ -h)
            position   = V.fromList [ LCLin.V2  0 dy,   LCLin.V2  0  0,   LCLin.V2 dx  0,   LCLin.V2  0 dy,   LCLin.V2 dx  0,   LCLin.V2 dx dy ]
            texcoord   = V.fromList [ LCLin.V2  0  1,   LCLin.V2  0  0,   LCLin.V2  1  0,   LCLin.V2  0  1,   LCLin.V2  1  0,   LCLin.V2  1  1 ]
            mesh       = LC.Mesh { mPrimitive  = P_Triangles
                                 , mAttributes = Map.fromList [ ("position",  A_V2F position)
                                                              , ("uv",        A_V2F texcoord) ] }

        experiment scen iterN glstorage mesh

        timePreGC ← getTime
        gc
        new       ← gcKBytesUsed
        timePost  ← getTime
        let dt     = timePost  - timePre
            nonGCt = timePreGC - timePre
            avgPost@(avgVal, _) = avgStep dt avgPre
        when (0 == mod iterN 40) $
          printf " frame  used dFrMem avgFrMem avgFrTime frTimeNonGC   scenario: %s\n" (show scen)
        when (preKB /= new) $
          printf "%5dn %dk %4ddK %5dK/f    %4.2fms      %4.2fms\n"
                 iterN new (new - preKB) (ceiling $ (fromIntegral new / fromIntegral iterN) ∷ Int)
                 (avgVal * 1000) (nonGCt * 1000)
        loop (iterN + 1, timePost) avgPost new
  loop (0 ∷ Integer, timeStart) (0.0, (navg, 0, [])) =<< gcKBytesUsed

timespecToSecs ∷ Sys.TimeSpec → Double
timespecToSecs = (/ 1000000000.0) . fromIntegral . Sys.toNanoSecs

getTime ∷ IO Double
getTime = timespecToSecs <$> Sys.getTime Sys.Monotonic

gc ∷ IO ()
gc = liftIO $ Sys.performGC

gcKBytesUsed ∷ (MonadIO m) ⇒ m Integer
gcKBytesUsed = liftIO $ (`div` 1024) . fromIntegral . Sys.gcdetails_mem_in_use_bytes . Sys.gc <$> Sys.getRTSStats

type Avg a = (Int, Int, [a])
avgStep ∷ Fractional a ⇒ a → (a, Avg a) → (a, Avg a)
avgStep x (_, (lim, cur, xs)) =
  let (ncur, nxs) = if cur < lim
                    then (cur + 1, x:xs)
                    else (lim,     x:Data.List.init xs)
  in ((sum nxs) / fromIntegral ncur, (lim, ncur, nxs))

-- Repro for https://github.com/lambdacube3d/lambdacube-gl/issues/9
-- Usage:
--   ghc --make Holostress.hs && ./Holostress
--
{-# OPTIONS_GHC -Wall #-}
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

import           Control.Lens
import           Control.Monad                            (filterM, when)
import qualified Data.Aeson                        as AE
import qualified Data.ByteString.Char8             as SB
import qualified Data.ByteString.Lazy              as LB
import qualified Data.Map                          as Map
import           Data.Maybe
import           Data.String
import qualified Data.Vector                       as V

import qualified Foreign.Ptr                       as F

import qualified Data.GI.Base                      as GI
import qualified GI.Cairo                          as GIC
import qualified GI.PangoCairo.Functions           as GIPC
import qualified Graphics.Rendering.Cairo          as GRC
import qualified Graphics.Rendering.Cairo.Types    as GRC
import qualified Graphics.Rendering.Cairo.Internal as GRC (create)

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

import qualified System.Directory                  as FS
import qualified System.IO                         as Sys


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

  storage ← GL.allocStorage $ pipelineSchema [("canvasStream", "canvasMtl")]

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

  _ ← GL.setStorage renderer storage <&>
    fromMaybe (error $ printf "setStorage failed")

  let (w, h) = (1, 1)
      loop = do
        putStr "0"
        dSurface      ← GRC.createImageSurface GRC.FormatARGB32 w h
        putStr "1"
        dGRC          ← GRC.create dSurface
        putStr "2"
        dGIC          ← GIC.Context <$> GI.newManagedPtr (F.castPtr $ GRC.unCairo dGRC) (return ())
        let (dx, dy) = (fromIntegral w, fromIntegral $ -h)
            position = V.fromList [ LCLin.V2  0 dy,   LCLin.V2  0  0,   LCLin.V2 dx  0,   LCLin.V2  0 dy,   LCLin.V2 dx  0,   LCLin.V2 dx dy ]
            texcoord = V.fromList [ LCLin.V2  0  1,   LCLin.V2  0  0,   LCLin.V2  1  0,   LCLin.V2  0  1,   LCLin.V2  1  0,   LCLin.V2  1  1 ]
            dMesh    = LC.Mesh { mPrimitive  = P_Triangles
                               , mAttributes = Map.fromList [ ("position",  A_V2F position)
                                                            , ("uv",        A_V2F texcoord) ] }
        putStr "3"
        -- 450123450123450123450123450123450123450123  <-- the trace always ends on a '3'
        _ ← GL.uploadMeshToGPU dMesh
        putStr "4"
        _ ← GIPC.createContext dGIC
        putStr "5"
        loop
  loop

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

-- Usage:
--   cabal build Holostress && ./dist/build/Holostress/Holostress
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
{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors #-}

import           Prelude.Unicode

import           Control.Monad                            (when)
import qualified Data.Map                          as Map
import qualified Data.Vector                       as V


import qualified GI.PangoCairo.Functions           as GIPC
import qualified Graphics.Rendering.Cairo          as GRC

import qualified Graphics.GL.Core33                as GL
import qualified "GLFW-b" Graphics.UI.GLFW         as GLFW

import qualified LambdaCube.GL.Mesh                as GL
import qualified LambdaCube.Linear                 as LCLin
import           LambdaCube.Mesh                   as LC


import           Text.Printf                              (printf)

import qualified Data.Text                         as T
import qualified Data.Text.Zipper                  as T


import Flatland

import HoloCube
import HoloCairo
import HoloCanvas
import HoloSettings
import qualified HoloSys                           as HS


main ∷ IO ()
main = do
  HS.unbufferStdout
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

  (_renderer, stream)   ← makeSimpleRenderedStream win (("canvasStream", "canvasMtl") ∷ (ObjArrayNameS, UniformNameS))
  -- renderer ← Q3.printTimeDiff "-- allocating GPU pipeline (GL.allocRenderer)... " $ do
  --   AE.eitherDecode <$> LB.readFile (Prelude.head validPaths) >>= \case
  --     Left err  → fail err
  --     Right ppl → GL.allocRenderer ppl

  -- _ ← GL.setStorage renderer storage <&>
  --   fromMaybe (error $ printf "setStorage failed")

  -- * Holo
  stts@Settings{..}    ← defaultSettings

  let style  = (In (CanvasS @PU "default")
                 (In (RRectS { rrCLBezel = coGray 1 1, rrCDBezel = coGray 0.1 0.5, rrCBorder = coGray 0.5 1, rrCBG = coOpaq 0.1 0.1 0.5
                             , rrThBezel = 2, rrThBorder = 5, rrThPadding = 16 })
                   (TextS @PU "default" 7 $ coGray 1 1)))
      text n        = [ T.pack $ printf "Object #%d:" n
                      , "  Esc:           quit"
                      , "  F1:            toggle per-frame object stream"
                      , "  Editing keys:  edit"
                      , ""
                      , "Yay!"] ∷ [T.Text]
      zipper = textZipper $ text (42 ∷ Int)
      (w, h) = (1, 1)
      loop old = do
        dSurface       ← GRC.createImageSurface GRC.FormatARGB32 w h
        cairo          ← cairoCreate dSurface
        dGIC           ← cairoToGICairo cairo

        let (_dx, _dy)  = (fromIntegral w, fromIntegral $ -h)
            _position   = V.fromList [ LCLin.V2  0 _dy,   LCLin.V2  0  0,   LCLin.V2 _dx  0,   LCLin.V2  0 _dy,   LCLin.V2 _dx  0,   LCLin.V2 _dx _dy ]
            _texcoord   = V.fromList [ LCLin.V2  0   1,   LCLin.V2  0  0,   LCLin.V2   1  0,   LCLin.V2  0   1,   LCLin.V2   1  0,   LCLin.V2   1   1 ]
            _dMesh      = LC.Mesh { mPrimitive  = P_Triangles
                                  , mAttributes = Map.fromList [ ("position",  A_V2F _position)
                                                               , ("uv",        A_V2F _texcoord) ] }
        -- _ ← GL.uploadMeshToGPU _dMesh
        -- (GIP.Context _gipc@(GI.ManagedPtr _fptr ownedR))
        -- owned ← IO.readIORef ownedR
        _ ← GIPC.createContext dGIC

        -- HoloCanvas.Text
        vis ← assemble stts stream style $ zipperText zipper
        render vis

        --- do stats
        HS.gc
        new ← HS.gcBytesUsed
        when (old /= new) $
          printf "memory usage: %d\n" new
        loop new
  loop =<< HS.gcBytesUsed

textZipper ∷ [T.Text] → T.TextZipper T.Text
textZipper = flip T.textZipper Nothing

zipperText ∷ T.TextZipper T.Text → T.Text
zipperText = T.dropEnd 1 ∘ T.unlines ∘ T.getText

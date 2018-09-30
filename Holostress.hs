-- Usage:
--   cabal build Holostress && ./dist/build/Holostress/Holostress +RTS -T -RTS
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
{-# OPTIONS_GHC -Wextra -Wno-unticked-promoted-constructors -Wno-type-defaults #-}

import           HoloPrelude

import qualified Data.List
import qualified Data.Map                          as Map
import qualified Data.Vector                       as V

import           Linear

import qualified GI.PangoCairo.Functions           as GIPC
import qualified Graphics.Rendering.Cairo          as GRC

import qualified Graphics.GL.Core33                as GL
import qualified "GLFW-b" Graphics.UI.GLFW         as GLFW

import qualified LambdaCube.GL.Mesh                as GL
import qualified LambdaCube.GL.Input               as GL
import qualified LambdaCube.Linear                 as LCLin
import           LambdaCube.Mesh                   as LC

import qualified Data.Text                         as T
import qualified Data.Text.Zipper                  as T


import           Flatland
import           HoloTypes

import qualified HoloCairo                         as Cr
import           HoloCube
import qualified HoloCube                          as HC
import qualified HoloOS                            as HOS
import           HoloPort


main ∷ IO ()
main = do
  HOS.unbufferStdout
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

  (_renderer, ObjectStream{..}) ← makeSimpleRenderedStream win (("portStream", "portMtl") ∷ (ObjArrayNameS, UniformNameS))

  -- * Holo
  Settings{..}        ← defaultSettings

  timeStart           ← HOS.fromSec <$> HOS.getTime
  let text n        = [ T.pack $ printf "Object #%d:" n
                      , "  Esc:           quit"
                      , "  F1:            toggle per-frame object stream"
                      , "  Editing keys:  edit"
                      , ""
                      , "Yay!"] ∷ [T.Text]
      zipper = T.textZipper $ text (42 ∷ Int)
      dim@(Di (V2 w h)) = di 256 256
      navg = 10
      loop (iterN, timePre) avgPre preKB = do
        dSurface       ← GRC.createImageSurface GRC.FormatARGB32 w h
        cairo          ← Cr.cairoCreate dSurface
        dGIC           ← Cr.cairoToGICairo cairo

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

        -- cDrawable ← makeDrawable stream $ fromIntegral <$> dim
        do
          dSurface      ← GRC.createImageSurface GRC.FormatARGB32 w h
          dCairo        ← Cr.cairoCreate  dSurface
          dGIC          ← Cr.cairoToGICairo dCairo
          let (dx, dy) = (fromIntegral w, fromIntegral $ -h)
              position = V.fromList [ LCLin.V2  0 dy,   LCLin.V2  0  0,   LCLin.V2 dx  0,   LCLin.V2  0 dy,   LCLin.V2 dx  0,   LCLin.V2 dx dy ]
              texcoord = V.fromList [ LCLin.V2  0  1,   LCLin.V2  0  0,   LCLin.V2  1  0,   LCLin.V2  0  1,   LCLin.V2  1  0,   LCLin.V2  1  1 ]
              dMesh    = LC.Mesh { mPrimitive  = P_Triangles
                                 , mAttributes = Map.fromList [ ("position",  A_V2F position)
                                                              , ("uv",        A_V2F texcoord) ] }
          dGPUMesh      ← GL.uploadMeshToGPU dMesh
          dGLObject     ← GL.addMeshToObjectArray osStorage (HC.fromOANS osObjArray) [SB.unpack $ fromUNS osUniform, "viewProj"] dGPUMesh
          GL.removeObject osStorage dGLObject
          GL.disposeMesh dGPUMesh
        -- Canvas (RRect T.Text)
        -- let cStyle@(In (CanvasS cFontKey) innerStyle) = style
        --     innerContent = zipperText zipper
        -- vis ← do
        --   cPSpace   ← sPin (po 0 0) <$> query stts innerStyle innerContent
        --   cDrawable ← makeDrawable stream $ spaceDim cPSpace
        --   cFont     ← bindFont (lookupFont' fontmap cFontKey) dGIC
        --   let w = Canvas{..} where cInner = (⊥)                -- resolve circularity due to *ToInner..
        --   cInner ← make stts (CW w) innerStyle innerContent cPSpace
        --   pure w { cInner = cInner }
        -- render vis

        --- do stats
        timePreGC ← HOS.fromSec <$> HOS.getTime
        HOS.gc
        new       ← HOS.gcKBytesUsed
        timePost  ← HOS.fromSec <$> HOS.getTime
        let dt     = timePost  - timePre
            nonGCt = timePreGC - timePre
            avgPost@(avgVal, _) = avgStep dt avgPre
        when (0 == mod iterN 40) $
          printf " frame  used dFrMem avgFrMem avgFrTime frTimeNonGC\n"
        -- when (preKB /= new) $
        printf "%5dn %dk %4ddK %5dK/f    %4.2fms      %4.2fms\n"
                 iterN new (new - preKB) (ceiling $ (fromIntegral new / fromIntegral iterN) ∷ Int)
                 (avgVal ⋅ 1000) (nonGCt ⋅ 1000)
        loop (iterN + 1, timePost) avgPost new
  loop (0 ∷ Integer, timeStart) (0.0, (navg, 0, [])) =<< HOS.gcKBytesUsed

type Avg a = (Int, Int, [a])
avgStep ∷ Fractional a ⇒ a → (a, Avg a) → (a, Avg a)
avgStep x (_, (lim, cur, xs)) =
  let (ncur, nxs) = if cur < lim
                    then (cur + 1, x:xs)
                    else (lim,     x:Data.List.init xs)
  in ((sum nxs) / fromIntegral ncur, (lim, ncur, nxs))

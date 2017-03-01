{-# LANGUAGE DataKinds, GADTs, KindSignatures, TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitForAll, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RankNTypes #-}
{-# LANGUAGE LambdaCase, OverloadedStrings, RecordWildCards, ScopedTypeVariables, TupleSections, TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}

module HoloCanvas where

-- Basis
import           Prelude                           hiding ((.))
import           Prelude.Unicode
import           Control.Category
import           Control.Lens

-- Type-level
import           GHC.TypeLits                      hiding (Text)

-- Types
import           Control.Arrow                            ((***))
import           Control.Monad                            (join, unless, when, forM_, filterM)
import           Control.Monad.Trans.Reader               (ReaderT(..))
import qualified Data.ByteString.Char8             as SB
import           Data.Map                                 (Map)
import qualified Data.Map                          as Map
import           Data.Maybe                               (fromMaybe)
import           Data.MeasuredMonoid
import           Data.String                              (IsString)
import qualified Data.Text                         as T
import qualified Data.Vector                       as V
import qualified Data.Vect                         as Vc
import           Data.Vect                                (Mat4(..), Vec3(..), Vec4(..))
import           Numeric.Extra                            (floatToDouble, doubleToFloat)

-- Algebra
import           Linear

-- Misc
import           System.FilePath                          ((</>))
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
import HoloCube


-- | A GL/Cairo drawable.
data Canvas where
  Canvas ∷
    { cObjectStream ∷ ObjectStream
    , cDi           ∷ Di Int
    , cSurface      ∷ GRC.Surface
    , cStrideBytes  ∷ Wi Int
    , cStridePixels ∷ Wi Int
    , cGRC          ∷ GRC.Cairo
    , cGIC          ∷ GIC.Context
    , cTexture      ∷ GL.TextureData
    --
    , cMesh         ∷ LC.Mesh
    , cGPUMesh      ∷ GL.GPUMesh
    , cGLObject     ∷ GL.Object
    } → Canvas

grcToGIC ∷ GRC.Cairo → IO GIC.Context
grcToGIC grc = GIC.Context <$> GI.newManagedPtr (F.castPtr $ GRC.unCairo grc) (return ())

makeCanvas ∷ ObjectStream → Di Double → IO Canvas
makeCanvas cObjectStream@ObjectStream{..} cDi' = do
  let cDi@(Di (V2 w h)) = fmap ceiling cDi'
  cSurface      ← GRC.createImageSurface GRC.FormatARGB32 w h
  cStrideBytes  ← Wi <$> GRC.imageSurfaceGetStride cSurface
  let cStridePixels = (`div` 4) <$> cStrideBytes
  cGRC          ← GRC.create cSurface
  cGIC          ← grcToGIC cGRC

  -- pixels        ← GRCI.imageSurfaceGetData cSurface -- XXX/eff: convert to imageSurfaceGetPixels
  -- cTexture      ← uploadTexture2DToGPU'''' False False False False $ (fromWi cStridePixels, h, GL_BGRA, pixels)
  -- XXX/not initialised: cTexture

  let (dx, dy) = (fromIntegral w, fromIntegral $ -h)
      -- position = V.fromList [ LCLin.V3  0 dy 0, LCLin.V3  0  0 0, LCLin.V3 dx  0 0, LCLin.V3  0 dy 0, LCLin.V3 dx  0 0, LCLin.V3 dx dy 0 ]
      position = V.fromList [ LCLin.V2  0 dy,   LCLin.V2  0  0,   LCLin.V2 dx  0,   LCLin.V2  0 dy,   LCLin.V2 dx  0,   LCLin.V2 dx dy ]
      texcoord = V.fromList [ LCLin.V2  0  1,   LCLin.V2  0  0,   LCLin.V2  1  0,   LCLin.V2  0  1,   LCLin.V2  1  0,   LCLin.V2  1  1 ]
      cMesh    = LC.Mesh { mPrimitive  = P_Triangles
                         , mAttributes = Map.fromList [ ("position",  A_V2F position)
                                                      , ("uv",        A_V2F texcoord) ] }
  cGPUMesh      ← GL.uploadMeshToGPU cMesh
  cGLObject     ← GL.addMeshToObjectArray osStorage (fromOANS osObjArray) [unameStr osUniform, "viewProj"] cGPUMesh

  pure Canvas{..}

canvasContentToGPU ∷ Canvas → IO ()
canvasContentToGPU Canvas{..} = do
  let ObjectStream{..} = cObjectStream

  -- XXX/temp hack:
  let h = (view _y) ∘ fromDi $ cDi
  pixels        ← GRCI.imageSurfaceGetData cSurface -- XXX/eff: convert to imageSurfaceGetPixels
  cTexture      ← uploadTexture2DToGPU'''' False False False False $ (fromWi cStridePixels, h, GL_BGRA, pixels)

  GL.updateObjectUniforms cGLObject $ do
    fromUNS osUniform GL.@= return cTexture
  GL.uniformFTexture2D (fromUNS osUniform) (GL.uniformSetter osStorage) cTexture

canvasPosition ∷ Canvas → Di Int → Po Float → IO ()
canvasPosition Canvas{..} (Di (V2 screenW screenH)) (Po (V2 x y)) = do
  let cvpos    = Vec3 x y 0
      toScreen = screenM screenW screenH
  GL.uniformM44F "viewProj" (GL.objectUniformSetter $ cGLObject) $
    Q3.mat4ToM44F $! toScreen Vc..*. (Vc.fromProjective $! Vc.translation cvpos)


-- * Very early generic widget code.
type CanvasSpace p d = Space p Double d
type WidgetSpace   d = CanvasSpace False d

-- | Lifecycle phase of a 'Widget'.
data Phase
  = Shell      -- ^ A fully-parametrized widget, with, however, no content or canvas,
               --   and no dimension or position information at all.
  | Contentful -- ^ Content available, dimension requirements specified, unpositioned.
  | Bound      -- ^ Canvas available, position and dimensions fully specified.

class Widget  (w ∷ Phase → * → *) where
  type FillArg w ∷ *
  type BindArg w ∷ *
  type   Depth w ∷ Nat
  fill         ∷ w Shell a      → a         → FillArg w → IO (w Contentful a)
  spaceRequest ∷ w Contentful a → CanvasSpace False (Depth w)
  bind         ∷ w Contentful a → Canvas    → CanvasSpace True (Depth w) → BindArg w → IO (w Bound a)
  render       ∷ w Bound a      → IO ()


-- * Void: null widget
data Void (p ∷ Phase) a where
  Void  ∷ Void Shell a
  CVoid ∷ Void Contentful a
  PVoid ∷ Void Bound a

instance Widget Void where
  type  FillArg Void = ()
  type  BindArg Void = ()
  type    Depth Void = 0
  fill Void _ ()      = pure CVoid
  spaceRequest _      = End
  bind CVoid _ End () = pure PVoid
  render PVoid        = pure ()


-- * Text: end-point widget
data Text u (p ∷ Phase) a where
  Text ∷ a ~ T.Text ⇒
    { tSettings     ∷ TextSettings TSProto u
    , tColor        ∷ Co Double
    } → Text u Shell a
  CText ∷ a ~ T.Text ⇒
    { tShell        ∷ Text u Shell a
    , tInitialText  ∷ a
    , tUSpace       ∷ CanvasSpace False 1
    } → Text u Contentful a
  PText ∷ a ~ T.Text ⇒
    { tContent      ∷ Text u Contentful a
    , tPSpace       ∷ CanvasSpace True 1
    , tLayout       ∷ GIP.Layout
    , tText         ∷ T.Text
    , tCanvas       ∷ Canvas
    } → Text u Bound a

instance Widget (Text u) where
  type  FillArg (Text u) = Wi (Size PU)
  type  BindArg (Text u) = TextSettings TSPhys u
  type    Depth (Text u) = 1
  fill tShell@(Text TextSettings{..} _) tInitialText maxWi = do
    di ∷ Di (Size PU) ← layRunTextForSize tsLayout tsDΠ maxWi tInitialText -- XXX/GHC/inference: weak
    let tUSpace = sArea $ fromPU ∘ fromSz tsDΠ <$> di
    pure CText{..}
  spaceRequest = tUSpace

  bind tContent@(CText Text{..} tInitialText _) tCanvas@Canvas{..} tPSpace tc = do
    let tText = tInitialText
    tLayout ← makeTextLayout tc
    pure PText{..}
  render (PText (CText Text{..} _ _)
                (Spc (PWrap _ _ (Po (V2 cvx cvy)) _) End) lay text
                 Canvas{..}) = do
    (`runReaderT` cGRC) $ GRC.runRender $ do
      GRC.moveTo cvx cvy
      coSetSourceColor tColor
      GIP.layoutSetText lay text (-1)
      GIPC.showLayout cGIC lay


-- * Rounded rectangle: widget component
data RRect (p ∷ Phase) a where
  RRect ∷
    { rrCLBezel, rrCBorder, rrCDBezel, rrCBG ∷ Co Double
    , rrThBezel, rrThBorder, rrThPadding ∷ Th Double
    } → RRect Shell a
  CRRect ∷
    { rrShell        ∷ RRect Shell a
    , rrInside       ∷ a
    } → RRect Contentful a
  PRRect ∷
    { rrContent      ∷ RRect Contentful a
    , rrPSpace       ∷ CanvasSpace True 4
    , rrCanvas       ∷ Canvas
    } → RRect Bound a

instance Widget RRect where
  type  FillArg RRect = ()
  type  BindArg RRect = ()
  type    Depth RRect = 4
  fill rrShell rrInside _ =
    pure CRRect{..}
  spaceRequest (CRRect RRect{..} _) =
    sGrowS (fromTh rrThBezel) $ sGrowS (fromTh rrThBorder) $ sGrowS (fromTh rrThBezel) $ sGrowS (fromTh rrThPadding) End

  bind rrContent rrCanvas rrPSpace () =
    pure PRRect{..}
  render (PRRect (CRRect RRect{..} _)
                 (Spc obez (Spc bord (Spc ibez (Spc pad _))))
                 (Canvas _ _ _ _ _ cGRC cGIC _ _ _ _)) = do
    (`runReaderT` cGRC) $ GRC.runRender $ do
      -- ((layw, layh), ellipsized) ←
      let d (Po (V2 x y)) (Co (V4 r g b a)) = GRC.setSourceRGBA r g b a >> GRC.rectangle (x) (y) 1 1 >> GRC.fill -- GRC.rectangle (x-1) (y-1) 3 3 >> GRC.fill
          dCorn (RRCorn _ pos _ _) col = d pos col
          ths@[oth, bth, ith, pth]
                        = fmap (Th ∘ fromWi ∘ wL) [obez, bord, ibez, pad]
          totpadx       = sum ths
          or            =       R ∘ fromTh $ (totpadx - oth/2)
          br            = or - (R ∘ fromTh $ (oth+bth)*0.6)
          ir            = br - (R ∘ fromTh $ (bth+ith)/2)
      -- coSetSourceColor (co 0 1 0 1) >> GRC.paint
      -- background & border arcs
      let bfeats@[n, ne, _, se, _, sw, _, nw] = wrapRoundedRectFeatures bord br bth
      GRC.newPath >> thLineSet bth
      forM_ [n, ne, se, sw, nw] $ executeFeature Nothing Nothing
      coSetSourceColor rrCBG >>
        GRC.fillPreserve
      coSetSourceColor rrCBorder >>
        GRC.stroke

      thLineSet oth -- border bezels: light outer TL, dark outer SE
      let ofeats@[n, ne, e, se, s, sw, w, nw] = wrapRoundedRectFeatures obez or oth
      GRC.newPath
      (coSetSourceColor $ rrCLBezel) >>
        (forM_ [w, nw, n] $ executeFeature Nothing Nothing) >> GRC.stroke
      (coSetSourceColor $ rrCDBezel) >>
        (forM_ [e, se, s] $ executeFeature Nothing Nothing) >> GRC.stroke
      (coSetSourceColor $ rrCBorder) >>
        GRC.newPath >> (executeFeature (Just rrCDBezel) (Just rrCLBezel) sw) >> GRC.stroke >>
        GRC.newPath >> (executeFeature (Just rrCLBezel) (Just rrCDBezel) ne) >> GRC.stroke

      thLineSet ith -- border bezels: dark inner TL, light inner SE
      let [n, ne, e, se, s, sw, w, nw] = wrapRoundedRectFeatures ibez ir ith
      GRC.newPath
      (coSetSourceColor $ rrCDBezel) >>
        (forM_ [w, nw, n] $ executeFeature Nothing Nothing) >> GRC.stroke
      (coSetSourceColor $ rrCLBezel) >>
        (forM_ [e, se, s] $ executeFeature Nothing Nothing) >> GRC.stroke
      (coSetSourceColor $ rrCBorder) >>
        GRC.newPath >> (executeFeature (Just rrCLBezel) (Just rrCDBezel) sw) >> GRC.stroke >>
        GRC.newPath >> (executeFeature (Just rrCDBezel) (Just rrCLBezel) ne) >> GRC.stroke

       ∷ GRCI.Render () -- XXX/GHC: an apparent type checker bug
      -- ellipsized ← GIP.layoutIsEllipsized gip
      -- (, ellipsized) <$> GIP.layoutGetPixelSize gip


-- * Canvas
data CanvasS (u ∷ KUnit) where
  CanvasS ∷
    { tSettings     ∷ TextSettings TSProto u
    } → CanvasS u
deriving instance Show (CanvasS u)


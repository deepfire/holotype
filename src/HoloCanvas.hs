{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}
{-# LANGUAGE GADTs, TypeFamilies, TypeFamilyDependencies, TypeInType #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitForAll, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RankNTypes, UndecidableInstances #-}
{-# LANGUAGE LambdaCase, OverloadedStrings, PartialTypeSignatures, RecordWildCards, ScopedTypeVariables, TupleSections, TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}
-- {-# OPTIONS_GHC -ddump-deriv #-}

module HoloCanvas where

-- Basis
import           Prelude                           hiding ((.))
import           Prelude.Unicode
import           Control.Applicative
import           Control.Applicative.Free
import           Control.Category
import           Control.Lens
import           Control.Monad.Identity
import           Data.Functor.Apply

-- Type-level
import           GHC.Types
import           GHC.TypeLits                      hiding (Text)
import           GHC.TypeLits.Extra

-- Types
import           Control.Arrow                            ((***))
import           Control.Monad                            (join, unless, when, forM_)
import           Control.Monad.IO.Class                   (MonadIO, liftIO)
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
import HoloSettings


-- | A Cairo-capable 'Drawable' to display on a GL 'Frame'.
data Drawable where
  Drawable ∷
    { dObjectStream ∷ ObjectStream
    , dDi           ∷ Di Int
    , dSurface      ∷ GRC.Surface
    , dStrideBytes  ∷ Wi Int
    , dStridePixels ∷ Wi Int
    , dGRC          ∷ GRC.Cairo
    , dGIC          ∷ GIC.Context
    , dTexture      ∷ GL.TextureData
    --
    , dMesh         ∷ LC.Mesh
    , dGPUMesh      ∷ GL.GPUMesh
    , dGLObject     ∷ GL.Object
    } → Drawable

-- | A GL 'Frame' to display a Cairo-capable 'Drawable'.
data Frame where
  Frame ∷
    { fDim ∷ Di Int
    } → Frame

-- | Make 'Renderer' produce a new 'Frame' to draw on.
rendererFinaliseToNewFrame ∷ (MonadIO m) ⇒ Renderer → m Frame
rendererFinaliseToNewFrame renderer = do
  liftIO $ rendererFinaliseFrame renderer
  liftIO $ rendererWaitForVSync renderer
  Frame <$> (liftIO $ rendererSetupFrame renderer)

grcToGIC ∷ GRC.Cairo → IO GIC.Context
grcToGIC grc = GIC.Context <$> GI.newManagedPtr (F.castPtr $ GRC.unCairo grc) (return ())

makeDrawable ∷ ObjectStream → Di Double → IO Drawable
makeDrawable dObjectStream@ObjectStream{..} dDi' = do
  let dDi@(Di (V2 w h)) = fmap ceiling dDi'
  dSurface      ← GRC.createImageSurface GRC.FormatARGB32 w h
  dStrideBytes  ← Wi <$> GRC.imageSurfaceGetStride dSurface
  let dStridePixels = (`div` 4) <$> dStrideBytes
  dGRC          ← GRC.create dSurface
  dGIC          ← grcToGIC dGRC

  -- pixels        ← GRCI.imageSurfaceGetData cSurface -- XXX/eff: convert to imageSurfaceGetPixels
  -- cTexture      ← uploadTexture2DToGPU'''' False False False False $ (fromWi cStridePixels, h, GL_BGRA, pixels)
  -- XXX/not initialised: cTexture

  let (dx, dy) = (fromIntegral w, fromIntegral $ -h)
      -- position = V.fromList [ LCLin.V3  0 dy 0, LCLin.V3  0  0 0, LCLin.V3 dx  0 0, LCLin.V3  0 dy 0, LCLin.V3 dx  0 0, LCLin.V3 dx dy 0 ]
      position = V.fromList [ LCLin.V2  0 dy,   LCLin.V2  0  0,   LCLin.V2 dx  0,   LCLin.V2  0 dy,   LCLin.V2 dx  0,   LCLin.V2 dx dy ]
      texcoord = V.fromList [ LCLin.V2  0  1,   LCLin.V2  0  0,   LCLin.V2  1  0,   LCLin.V2  0  1,   LCLin.V2  1  0,   LCLin.V2  1  1 ]
      dMesh    = LC.Mesh { mPrimitive  = P_Triangles
                         , mAttributes = Map.fromList [ ("position",  A_V2F position)
                                                      , ("uv",        A_V2F texcoord) ] }
  dGPUMesh      ← GL.uploadMeshToGPU dMesh
  dGLObject     ← GL.addMeshToObjectArray osStorage (fromOANS osObjArray) [unameStr osUniform, "viewProj"] dGPUMesh

  pure Drawable{..}

drawableContentToGPU ∷ Drawable → IO ()
drawableContentToGPU Drawable{..} = do
  let ObjectStream{..} = dObjectStream

  -- XXX/temp hack:
  let h = (view _y) ∘ fromDi $ dDi
  pixels   ← GRCI.imageSurfaceGetData dSurface -- XXX/eff: convert to imageSurfaceGetPixels
  cTexture ← uploadTexture2DToGPU'''' False False False False $ (fromWi dStridePixels, h, GL_BGRA, pixels)

  GL.updateObjectUniforms dGLObject $ do
    fromUNS osUniform GL.@= return cTexture

-- | To screen space conversion matrix.
screenM :: Int → Int → Mat4
screenM w h =
  Vc.Mat4 (Vc.Vec4 (1/fw)  0     0 0)
          (Vc.Vec4  0     (1/fh) 0 0)
          (Vc.Vec4  0      0     1 0)
          (Vc.Vec4  0      0     0 0.5) -- where does that 0.5 factor COMEFROM?
  where (fw, fh) = (fromIntegral w, fromIntegral h)

framePutDrawable ∷ (MonadIO m) ⇒ Frame → Drawable → Po Float → m ()
framePutDrawable (Frame (Di (V2 screenW screenH))) Drawable{..} (Po (V2 x y)) = do
  let cvpos    = Vec3 x y 0
      toScreen = screenM screenW screenH
  liftIO $
    GL.uniformM44F "viewProj" (GL.objectUniformSetter $ dGLObject) $
    Q3.mat4ToM44F $! toScreen Vc..*. (Vc.fromProjective $! Vc.translation cvpos)


-- * Very early style code.

-- | Free Applicative Element
data FAE s where
  FAE ∷ { fromFAE ∷ s } → FAE s
deriving instance Show w ⇒ Show (FAE w)

type Style = Ap FAE
style ∷ s → Style s
style s = liftAp $ FAE s

-- | The key point of analysis -- vain, as yet, yet still..
runStyle ∷ Style s → s
runStyle s = runIdentity $ runAp (return ∘ fromFAE) s


-- * Very early generic widget code.
type DrawableSpace p d = Space p Double d
type WidgetSpace   d = DrawableSpace False d

class Show (StyleOf a) ⇒ Visual a where
  type StyleOf a = (r ∷ Type) | r → a
  type Content a ∷ Type
  type   Depth a ∷ Nat

class   Visual w ⇒ Container w where
  type   Inner w ∷ Type
  innerOf        ∷ w → Inner w
  spaceToInner   ∷ w → DrawableSpace p (Depth w) → DrawableSpace p (Depth (Inner w))
  styleToInner   ∷ w → StyleOf w → StyleOf (Inner w)

class Visual w ⇒ Widget w where
  -- | Query size: style meets content → compute spatial parameters.
  query          ∷ Settings PU           → StyleOf w → Content w → IO (DrawableSpace False (Depth w))
  -- | Add target and space: given a drawable and a pinned space, prepare for 'render'.
  make           ∷ Settings PU → CanvasW → StyleOf w → Content w →     DrawableSpace True  (Depth w) → IO w
  -- | Per-content-change: mutate pixels of the bound drawable.
  draw           ∷ CanvasW → w → IO ()

class Container w ⇒ WDrawable w where
  assemble       ∷ Settings PU → ObjectStream → StyleOf w → Content w → IO w
  drawableOf     ∷ w → Drawable
  render         ∷ w → IO ()


-- * Styles
-- Widget composition is inherently parametrized.
-- Different kinds of composition are parametrized differently.
-- Different instances of composition compose different kinds of widgets.
-- Applicative much?

data In o i where
  In ∷ --(Widget wo, Widget wi, StyleOf wo ~ o, StyleOf wi ~ i) ⇒ -- disabled by XXX/recursive pain
    { insideOf ∷ o
    , internal ∷ i
    } → In o i
deriving instance (Show o, Show i) ⇒ Show (In o i)

data By o b where
  By ∷ --(Widget wo, Widget wb, StyleOf wo ~ o, StyleOf wb ~ b) ⇒
    { bOrigin  ∷ o
    , bOrient  ∷ Orient
    , bBeside  ∷ b
    } → By o b
deriving instance (Show o, Show b) ⇒ Show (By o b)

data RRectS where
  RRectS ∷
    { rrCLBezel, rrCBorder, rrCDBezel, rrCBG ∷ Co Double
    , rrThBezel, rrThBorder, rrThPadding ∷ Th Double
    } → RRectS
deriving instance Show RRectS


-- * (): a null widget
instance Visual () where
  type  StyleOf () = ()
  type  Content () = ()
  type    Depth () = 0
instance Widget () where
  query _settings        _style _content = pure End
  make  _settings CW{..} _style _content        End = pure ()
  draw            CW{..}                             _widget = pure ()

d (Po (V2 x y)) (Co (V4 r g b a)) = GRC.setSourceRGBA r g b a >>
                                    -- GRC.rectangle (x) (y) 1 1 >> GRC.fill
                                    GRC.rectangle (x-1) (y-1) 3 3 >> GRC.fill


-- * Text
data TextS (u ∷ Unit) where
  TextS ∷
    { tFontKey      ∷ FontKey
    , tMaxParaLines ∷ Int
    , tColor        ∷ Co Double
    } → TextS u
deriving instance Show (TextS u)

data Text where
  Text ∷
    { tPSpace       ∷ DrawableSpace True 1
    , tStyle        ∷ StyleOf Text
    , tFont         ∷ Font Bound PU
    , tLayout       ∷ GIP.Layout
    , tText         ∷ T.Text
    } → Text

instance Visual Text where
  type  StyleOf Text = TextS PU
  type  Content Text = (T.Text, Wi (Size PU))
  type    Depth Text = 1
instance Widget Text where
  query Settings{..} TextS{..} (initialText, maxWi) = do
    let Font{..} = lookupFont' fontmap tFontKey
    laySetMaxParaLines fDetachedLayout tMaxParaLines
    di ∷ Di (Size PU) ← layRunTextForSize fDetachedLayout fDΠ maxWi initialText -- XXX/GHC/inference: weak
    pure $ sArea $ fromPU ∘ fromSz fDΠ <$> di
  make Settings{..} (CW (Canvas Drawable{..} _ _ tFont@FontBinding{..} _))
       tStyle@(TextS fKey _ _) (tText, _maxWi) tPSpace = do
    tLayout ← makeTextLayout fbContext
    pure Text{..}
  draw (CW (Canvas (Drawable{..}) _ _ _ _))
       (Text (Spc (PWrap _ _ (Po lt@(V2 cvx cvy)) (Po rb@(V2 cvxe cvye))) End)
             TextS{..}
             (FontBinding Font{..} _) lay text) = do
    laySetSize         lay fDΠ (Di (PUs <$> (rb ^-^ lt)))
    laySetMaxParaLines lay tMaxParaLines
    (`runReaderT` dGRC) $ GRC.runRender $ do
      GRC.moveTo cvx cvy
      coSetSourceColor tColor
      GIP.layoutSetText lay text (-1)
      GIPC.showLayout dGIC lay


-- * Rounded rectangle
data RRect a where
  RRect ∷
    { rrPSpace ∷ DrawableSpace True (Depth (RRect a))
    , rrStyle ∷ StyleOf (RRect a)
    , rrInner ∷ a
    } → RRect a
deriving instance (Show a, Show (StyleOf a)) ⇒ Show (RRect a)

instance Visual a ⇒ Visual (RRect a) where
  type             StyleOf (RRect a) = In RRectS (StyleOf a) -- XXX/recursive pain
  type             Content (RRect a) = Content a
  type               Depth (RRect a) = 4 + Depth a
instance Widget a ⇒ Container (RRect a) where
  type Inner (RRect a) = a
  innerOf                 = rrInner
  styleToInner w (In _ s) = s
  spaceToInner _ (Spc _ (Spc _ (Spc _ (Spc _ s)))) = s

instance Widget a ⇒ Widget (RRect a) where
  query st@Settings{..} (In RRectS{..} inner) internals = do
    innerSpace ← query st inner internals
    pure $ (sGrowS (fromTh rrThBezel) $ sGrowS (fromTh rrThBorder) $ sGrowS (fromTh rrThBezel) $ sGrowS (fromTh rrThPadding) End)
           <> innerSpace
  make st@Settings{..} drawable rrStyle rrContent rrPSpace = do
    let w = RRect{..} where rrInner = (⊥)    -- resolve circularity due to *ToInner..
    make st drawable (styleToInner w rrStyle) rrContent (spaceToInner w rrPSpace) <&> (\x→ w { rrInner = x }) -- XXX/lens
  draw canvas@(CW (Canvas (Drawable _ _ _ _ _ cGRC cGIC _ _ _ _) _ _ _ _))
       (RRect (Spc obez (Spc bord (Spc ibez (Spc pad _))))
              (In RRectS{..} _) inner) = do
    (`runReaderT` cGRC) $ GRC.runRender $ do
      -- ((layw, layh), ellipsized) ←
      let dCorn (RRCorn _ pos _ _) col = d pos col
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
    draw canvas inner


-- * Canvas
data CanvasS (u ∷ Unit) where
  CanvasS ∷
    { cFontKey      ∷ FontKey
    } → CanvasS u
deriving instance Show (CanvasS u)

data Canvas a where
  Canvas ∷
    { cDrawable     ∷ Drawable
    , cPSpace       ∷ DrawableSpace True (Depth a)
    , cStyle        ∷ StyleOf (Canvas a)
    , cFont         ∷ Font Bound PU
    , cInner        ∷ a
    } → Canvas a
data CanvasW where
  CW ∷ Widget a ⇒ { cPoly ∷ Canvas a } → CanvasW

instance Widget a ⇒ Visual (Canvas a) where
  type             StyleOf (Canvas a) = In (CanvasS PU) (StyleOf a)
  type             Content (Canvas a) = Content a
  type             Depth   (Canvas a) = Depth a
instance Widget a ⇒ Container (Canvas a) where
  type                  Inner (Canvas a) = a
  innerOf                   = cInner
  styleToInner   _ (In _ s) = s
  spaceToInner   _       s  = s

instance Widget a ⇒ WDrawable (Canvas a) where
  assemble settings@Settings{..} stream cStyle@(In (CanvasS cFontKey) innerStyle) innerContent = do
    cPSpace   ← sPin (po 0 0) <$> query settings innerStyle innerContent
    cDrawable ← makeDrawable stream (sDim cPSpace)
    cFont     ← bindFont (lookupFont' fontmap cFontKey) $ dGIC cDrawable
    let w = Canvas{..} where cInner = (⊥)                -- resolve circularity due to *ToInner..
    cInner ← make settings (CW w) innerStyle innerContent cPSpace
    pure w { cInner = cInner }
  drawableOf = cDrawable
  render self@Canvas{..} = do
    draw (CW self) cInner
    drawableContentToGPU cDrawable

makeCanvas ∷ Widget w ⇒ Settings PU → ObjectStream → StyleOf (Canvas w) → Content (Canvas w) → IO CanvasW
makeCanvas sts os sty co = CW <$> assemble sts os sty co

renderCanvas ∷ CanvasW → IO ()
renderCanvas (CW c) = render c

placeCanvas ∷ (MonadIO m) ⇒ CanvasW → Frame → Po Double → m ()
placeCanvas (CW c) f = framePutDrawable f (drawableOf c) ∘ (doubleToFloat <$>)

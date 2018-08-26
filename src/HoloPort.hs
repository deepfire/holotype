{-# LANGUAGE GADTs, TypeFamilies, TypeFamilyDependencies, TypeInType #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitForAll, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RankNTypes, UndecidableInstances #-}
{-# LANGUAGE LambdaCase, OverloadedStrings, PackageImports, PartialTypeSignatures, RecordWildCards, ScopedTypeVariables, TupleSections, TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Weverything #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors -Wno-missing-import-lists -Wno-implicit-prelude #-}
{-# OPTIONS_GHC -Wno-monomorphism-restriction -Wno-name-shadowing -Wno-all-missed-specialisations #-}
{-# OPTIONS_GHC -Wno-unsafe -Wno-missing-export-lists -Wno-type-defaults #-}

module HoloPort where

-- Basis
import           HoloPrelude

-- Types
import           Control.Monad
import qualified Data.Map.Strict                   as Map
-- import qualified Data.Set                          as Set
import           Data.Maybe
import           Data.Proxy
import qualified Data.Text                         as T
import           Data.Typeable
import qualified Data.TypeMap.Dynamic              as TM
import qualified Data.Vector                       as V
import qualified Data.Vect                         as Vc
import           Data.Vect                                (Mat4(..), Vec3(..))

-- Algebra
import           Linear

-- Manually-bound Cairo
import qualified Graphics.Rendering.Cairo          as GRC
import qualified Graphics.Rendering.Cairo.Internal as GRCI

-- glib-introspection -based Cairo and Pango
import qualified GI.Pango                          as GIP

-- Dirty stuff
import qualified Control.Concurrent.STM            as STM
import qualified Data.Unique                       as U
import qualified Foreign.C.Types                   as F
import qualified Foreign                           as F
import qualified System.IO.Unsafe                  as IO
import qualified Data.IORef                        as IO

-- …
import           GHC.Stack
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
import           HoloTypes

import           Flatland
import           HoloCairo
import           HoloCube
import           HoloFont


-- * Drawable identity support

newId ∷ (HasCallStack, MonadIO m) ⇒ String → m IdToken
newId desc = liftIO $ do
  tok ← U.newUnique
  trev ALLOC TOK (U.hashUnique tok) (U.hashUnique tok)

  pure $ IdToken (tok, desc <> "\n" <> prettyCallStack callStack)

blankIdToken'      ∷ IO.IORef IdToken
blankIdToken'      = IO.unsafePerformIO $ IO.newIORef  undefined
blankIdToken'setup ∷ IO ()
blankIdToken'setup = IO.writeIORef blankIdToken' =<< newId "<blank>"
blankIdToken       ∷ IdToken
blankIdToken       = IO.unsafePerformIO $ IO.readIORef blankIdToken'
{-# NOINLINE blankIdToken #-}

tokenHash ∷ IdToken → Int
tokenHash = U.hashUnique ∘ fromIdToken

tokenDesc ∷ IdToken → String
tokenDesc = snd ∘ fromIdToken'


-- * Impure IO-stateful map

mkIOMap ∷ (MonadIO m, Ord k) ⇒ m (IOMap k v)
mkIOMap = IOMap
  <$> (liftIO $ STM.newTVarIO $ Map.empty)

iomapAccess ∷ (MonadIO m) ⇒ IOMap k v → m (Map.Map k v)
iomapAccess IOMap{..} = liftIO $ STM.readTVarIO iomap

iomapAdd ∷ (MonadIO m, Ord k) ⇒ IOMap k v → k → v → m ()
iomapAdd IOMap{..} k v = liftIO $ do
  STM.atomically $ STM.modifyTVar' iomap (Map.insert k v)
  pure ()

iomapDrop ∷ (MonadIO m, Ord k) ⇒ IOMap k v → k → m ()
iomapDrop IOMap{..} x = liftIO $
  STM.atomically $ STM.modifyTVar' iomap (Map.delete x)

iomapHas ∷ (MonadIO m, Ord k) ⇒ IOMap k v → k → m Bool
iomapHas iomap x = Map.member x <$> iomapAccess iomap

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
  -- liftIO $ setupTracer [
  --     (ALLOC,     TOK, STACK, 0),(FREE,      TOK, TRACE, 0)
  --    ,(MISSALLOC, VIS, STACK, 4),(REUSE,     VIS, TRACE, 4),(REALLOC,   VIS, TRACE, 4),(ALLOC,     VIS, TRACE, 4),(FREE,        VIS, TRACE, 4)
  --    ,(ALLOC,     TEX, TRACE, 8),(FREE,      TEX, TRACE, 8)
  --   ]
  liftIO $ blankIdToken'setup

  portFontmap                      ← makeFontMap sttsDΠ fmDefault sttsFontPreferences
  liftIO $ putStrLn $ printf "%s" (show portFontmap)

  (portRenderer, portObjectStream) ← makeSimpleRenderedStream portWindow (("portStream", "portMtl") ∷ (ObjArrayNameS, UniformNameS))
  rendererDrawFrame portRenderer
  portSetVSync True

  portEmptyDrawable ← makeDrawable portObjectStream (di 1 1)
  portVisualTracker@(VisualTracker _)     ← VisualTracker <$> mkVIOMap
  -- iomapAdd dt blankIdToken portEmptyDrawable
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


defaultSettings ∷ (MonadIO m) ⇒ m Settings
defaultSettings = do
  let sttsDΠ ∷ DΠ         = 96
      sttsFontPreferences = FontPreferences
        [ ("default",     Left $ Alias "defaultMono" )
        , ("defaultSans", Right $ [ FontSpec "Bitstream Charter" "Regular" $ FSROutline (PUs 16)
                                  , FontSpec "Aurulent Sans"     "Regular" $ FSROutline (PUs 16) ])
        , ("defaultMono", Right $ [ FontSpec "Terminus"          "Regular" $ FSRBitmap  (PUs 15) LT ])
        ]
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

portMakeDrawable ∷ (MonadIO m) ⇒ Port → Di Double → m Drawable
portMakeDrawable Port{..} = makeDrawable portObjectStream

makeDrawable ∷ (HasCallStack, MonadIO m) ⇒ ObjectStream → Di Double → m Drawable
makeDrawable dObjectStream@ObjectStream{..} dDi' = liftIO $ do
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

framePutDrawable ∷ (MonadIO m) ⇒ Frame → Drawable → Po Float → m ()
framePutDrawable (Frame (Di (V2 screenW screenH))) Drawable{..} (Po (V2 x y)) = do
  let cvpos     = Vec3 x (-y) 0
      toScreen  = screenM screenW screenH
  liftIO $ GL.uniformM44F "viewProj" (GL.objectUniformSetter $ dGLObject) $
    Q3.mat4ToM44F $! (Vc.fromProjective $! Vc.translation cvpos) Vc..*. toScreen

-- * Look up the visual and, if present, check compatibility with
--   new requirements: style generation and size.
--   XXX: violates abstraction (fiddles with port and Holo's visual at the same time)
--   XXX: not thread-safe
visualiseHoloitem ∷ (HasCallStack, MonadIO m) ⇒ Port → HoloItem PLayout → [HoloItem PVisual] → m (HoloItem PVisual)
visualiseHoloitem port@Port{..} hi children = case hi of
  HoloItem{..} → do
    let dim@(Di (V2 w h)) = hiArea^.area'b.size'di
        mkVisual = do
          newDrawable ← makeDrawable portObjectStream dim
          (,) True <$>
            (Visual
             <$> createVisual port (_sStyle hiStyle) hiArea holo newDrawable
             <*> pure (_sStyleGene $ hiStyle)
             <*> pure newDrawable)
    visMap ← viomapAccess (fromVT portVisualTracker)
    (updated ∷ Bool, vis)
           ← case join $ Map.lookup hiToken <$> TM.lookup Proxy visMap of
             Nothing → do
               trev MISSALLOC VIS (w, h, _fromStyleGene $ _sStyleGene $ hiStyle) (tokenHash hiToken)
               mkVisual
             Just vis@Visual{..} →
               let vDi     = dDi vDrawable
                   styleChanged = vStyleGene ≢ hiStyleGene hi
                   sizeChanged  = vDi ≢ (ceiling <$> dim)
                   update       = sizeChanged ∨ styleChanged
               in if update
               then do
                 trev REALLOC VIS (w, h)    (tokenHash hiToken)
                 disposeDrawable portObjectStream vDrawable
                 mkVisual
               else do
                 trev REUSE   VIS (w, h)                       (tokenHash hiToken)
                 pure $ (,) False vis
    when updated $
      viomapAdd (fromVT portVisualTracker) Proxy hiToken vis
    pure HoloItem{hiVisual=vis, hiChildren=children, ..}

portGarbageCollectVisuals ∷ (MonadIO m) ⇒ Port → Map.Map IdToken (HoloItem a) → m ()
portGarbageCollectVisuals Port{..} validLeaves = do
  visMap ← viomapAccess $ fromVT portVisualTracker
  let gcTM ∷ MonadIO m ⇒ Proxy b -> Map.Map IdToken (Visual b) -> m (Map.Map IdToken (Visual b))
      gcTM proxy vismap = do
        let used   = Map.intersection vismap validLeaves
            unused = Map.difference   vismap used
        _ ← flip Map.traverseWithKey unused $ \k Visual{..}→ do
          -- printf "releasing Visual for IdToken %s\n" (show $ U.hashUnique $ fromIdToken k)
          freeVisualOf vVisual proxy
          trev FREE VIS (dDi vDrawable^.di'w, dDi vDrawable^.di'h) (tokenHash k)
          disposeDrawable portObjectStream vDrawable
        pure used
  visMap' ← TM.traverse gcTM visMap
  viomapReplace (fromVT portVisualTracker) visMap'


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
dpx (Po (V2 x y)) color = crColor color >>
                          -- GRC.rectangle (x) (y) 1 1 >> GRC.fill
                          GRC.rectangle (x-1) (y-1) 3 3 >> GRC.fill

clearDrawable ∷ (MonadIO m) ⇒ Drawable → m ()
clearDrawable Drawable{..} = do
  runCairo dCairo $ do
    GRC.save
    GRC.setOperator GRCI.OperatorSource
    crColor (co 0 0 0 0)
    GRC.paint
    GRC.restore

drawableDrawRect ∷ (MonadIO m, FromUnit u) ⇒ Port → Drawable → Co Double → Di (Unit u) → m ()
drawableDrawRect Port{portSettings=Settings{..}} d@Drawable{..} color dim' = do
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
  d@Drawable{..} ← portMakeDrawable port $ fromPU ∘ fromUnit sttsDΠ <$> dim
  drawableDrawRect port d color dim
  pure d

drawableBindFontLayout ∷ (MonadIO m, FromUnit u, FromUnit v) ⇒
  DΠ → Drawable → Font Found u → Di (Unit v) → TextSizeSpec v → m (WFont Bound, GIP.Layout)
drawableBindFontLayout dπ Drawable{..} = bindWFontLayout dπ dGIC

drawableDrawText ∷ (MonadIO m) ⇒ Drawable → GIP.Layout → Co Double → T.Text → m ()
drawableDrawText Drawable{..} layout color text = do
  layDrawText dCairo dGIC layout (po 0 0) color text

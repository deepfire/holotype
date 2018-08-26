{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

import           Control.Monad                            (forever)

import qualified Foreign                           as F
import qualified Foreign.ForeignPtr.Unsafe         as F

import qualified Data.GI.Base                      as GI
import qualified GI.GObject.Objects.Object         as GIO

import qualified Graphics.Rendering.Cairo          as GRC
import qualified Graphics.Rendering.Cairo.Internal as GRC (create, destroy, surfaceDestroy)
import qualified Graphics.Rendering.Cairo.Types    as GRC

import qualified GI.Cairo.Structs.Context          as GIC
import qualified GI.Pango                          as GIP
import qualified GI.PangoCairo.Functions           as GIPC

import qualified GI.Cairo.Structs.Context          as Cairo.Context
import qualified GI.Pango.Objects.Context          as Pango.Context


foreign import ccall "&cairo_destroy"
  cairo_destroy ∷ F.FinalizerPtr GRC.Cairo
foreign import ccall "pango_cairo_create_context" pango_cairo_create_context ::
    F.Ptr Cairo.Context.Context ->            -- cr : TInterface (Name {namespace = "cairo", name = "Context"})
    IO (F.Ptr Pango.Context.Context)
foreign import ccall "g_object_unref" g_object_unref :: 
    F.Ptr GIO.Object ->                       -- object : TInterface (Name {namespace = "GObject", name = "Object"})
    IO ()

main ∷ IO ()
main = forever scenarioB

scenarioA ∷ IO ()
scenarioA = do
  crSurf               ← GRC.createImageSurface GRC.FormatARGB32 256 256
  grccfptr ∷ F.ForeignPtr GRC.Cairo
                       ← F.newForeignPtr cairo_destroy =<< GRC.unCairo <$> GRC.create crSurf
  gic@(GIC.Context gicfp) ← GIC.Context <$> GI.newManagedPtr (F.castPtr $ F.unsafeForeignPtrToPtr grccfptr) (F.touchForeignPtr grccfptr)
  gipc                 ← pango_cairo_create_context =<< GI.unsafeManagedPtrCastPtr gicfp
  g_object_unref $ F.castPtr gipc
  GRC.surfaceFinish  crSurf

scenarioB ∷ IO ()
scenarioB = do
  crSurf               ← GRC.createImageSurface GRC.FormatARGB32 256 256
  grccfptr ∷ F.ForeignPtr GRC.Cairo
                       ← F.newForeignPtr cairo_destroy =<< GRC.unCairo <$> GRC.create crSurf
  gic@(GIC.Context gicfp) ← GIC.Context <$> GI.newManagedPtr (F.castPtr $ F.unsafeForeignPtrToPtr grccfptr) (F.touchForeignPtr grccfptr)
  gipc                 ← GIPC.createContext gic
  GIO.objectUnref gipc
  GRC.surfaceFinish  crSurf

-- full ∷ IO ()
-- full = do
--   crSurf               ← GRC.createImageSurface GRC.FormatARGB32 256 256
--   grccfptr ∷ F.ForeignPtr GRC.Cairo
--                        ← F.newForeignPtr cairo_destroy =<< GRC.unCairo <$> GRC.create crSurf
--   -- grcc ∷ GRC.Cairo   ← GRC.create crSurf
--   gic@(GIC.Context gicfp) ← GIC.Context <$> GI.newManagedPtr (F.castPtr $ F.unsafeForeignPtrToPtr grccfptr) (F.touchForeignPtr grccfptr)
--   -- gic                  ← GIC.Context <$> GI.newManagedPtr (F.castPtr $ GRC.unCairo grcc) (cairo_destroy grcc)
--   -- gipc                 ← GIPC.createContext gic
--   gicptr               ← GI.unsafeManagedPtrCastPtr gicfp
--   gipc                 ← pango_cairo_create_context $ gicptr
--   -- gipl         ← GIP.layoutNew      gipc
--   -- GIP.layoutSetWidth  lay $ fromIntegral w
--   -- GIP.layoutSetHeight lay $ fromIntegral h
--   -- GIP.layoutSetText   lay "lol" (-1)
--   -- GI.objectUnref gipl
--   -- GIO.objectUnref gipc
--   -- gipcptr              ← GI.unsafeManagedPtrCastPtr gipc
--   g_object_unref $ F.castPtr gipc
--   GRC.destroy grcc
--   GRC.surfaceFinish  crSurf

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

import           Control.Monad                            (forever)

import qualified Foreign                           as F
import qualified Foreign.ForeignPtr.Unsafe         as F
import qualified Foreign.Concurrent                as FC
import qualified System.Environment                as Sys
import qualified Data.IORef                        as IO

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
import           Control.Monad.IO.Class
import qualified Data.GI.Base.CallStack            as B.CallStack
import           Data.GI.Base.ShortPrelude            (checkUnexpectedReturnNULL)


foreign import ccall "&cairo_destroy"
  cairo_destroy ∷ F.FinalizerPtr GRC.Cairo
foreign import ccall "pango_cairo_create_context" pango_cairo_create_context ::
    F.Ptr Cairo.Context.Context ->            -- cr : TInterface (Name {namespace = "cairo", name = "Context"})
    IO (F.Ptr Pango.Context.Context)
foreign import ccall "g_object_unref" g_object_unref :: 
    F.Ptr GIO.Object ->                       -- object : TInterface (Name {namespace = "GObject", name = "Object"})
    IO ()

main ∷ IO ()
main = do
  args ← Sys.getArgs
  forever $ case args of
              ["a"] → scenarioA
              ["b"] → scenarioB
              ["c"] → scenarioC
              ["d"] → scenarioD
              ["e"] → scenarioE
              _     → scenarioB

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

createContext' ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    Cairo.Context.Context
    {- ^ /@cr@/: a Cairo context -}
    -> m Pango.Context.Context
    {- ^ __Returns:__ the newly created 'GI.Pango.Objects.Context.Context'. Free with
      'GI.GObject.Objects.Object.objectUnref'. -}
createContext' cr = liftIO $ do
    cr' <- GI.unsafeManagedPtrGetPtr cr
    result <- pango_cairo_create_context cr'
    checkUnexpectedReturnNULL "createContext" result
    fPtr <- FC.newForeignPtr result (pure ())
    GI.touchManagedPtr cr
    isDisownedRef <- IO.newIORef Nothing
    return $ Pango.Context.Context $ GI.ManagedPtr
             { GI.managedForeignPtr = fPtr
             , GI.managedPtrIsDisowned = isDisownedRef
             }

scenarioC ∷ IO ()
scenarioC = do
  crSurf               ← GRC.createImageSurface GRC.FormatARGB32 256 256
  grccfptr ∷ F.ForeignPtr GRC.Cairo
                       ← F.newForeignPtr cairo_destroy =<< GRC.unCairo <$> GRC.create crSurf
  gic@(GIC.Context gicfp) ← GIC.Context <$> GI.newManagedPtr (F.castPtr $ F.unsafeForeignPtrToPtr grccfptr) (F.touchForeignPtr grccfptr)
  gipc                 ← createContext' gic
  GIO.objectUnref gipc
  GRC.surfaceFinish crSurf

scenarioD ∷ IO ()
scenarioD = do
  crSurf               ← GRC.createImageSurface GRC.FormatARGB32 256 256
  grccfptr ∷ F.ForeignPtr GRC.Cairo
                       ← F.newForeignPtr cairo_destroy =<< GRC.unCairo <$> GRC.create crSurf
  gic@(GIC.Context gicfp) ← GIC.Context <$> GI.newManagedPtr (F.castPtr $ F.unsafeForeignPtrToPtr grccfptr) (F.touchForeignPtr grccfptr)
  gipc                 ← createContext' gic
  gipl                 ← GIP.layoutNew  gipc
  GIP.layoutSetWidth  gipl $ fromIntegral 256
  GIP.layoutSetHeight gipl $ fromIntegral 256
  GIP.layoutSetText   gipl "lol" (-1)
  GIO.objectUnref gipl
  GIO.objectUnref gipc
  GRC.surfaceFinish crSurf

foreign import ccall "pango_layout_new" pango_layout_new :: 
    F.Ptr Pango.Context.Context ->            -- context : TInterface (Name {namespace = "Pango", name = "Context"})
    IO (F.Ptr GIP.Layout)
{- |
Create a new 'GI.Pango.Objects.Layout.Layout' object with attributes initialized to
default values for a particular 'GI.Pango.Objects.Context.Context'.
-}
layoutNew' ::
    (B.CallStack.HasCallStack, MonadIO m, Pango.Context.IsContext a) =>
    a
    {- ^ /@context@/: a 'GI.Pango.Objects.Context.Context' -}
    -> m GIP.Layout
    {- ^ __Returns:__ the newly allocated 'GI.Pango.Objects.Layout.Layout', with a reference
              count of one, which should be freed with
              'GI.GObject.Objects.Object.objectUnref'. -}
layoutNew' context = liftIO $ do
    context' <- GI.unsafeManagedPtrCastPtr context
    result <- pango_layout_new context'
    checkUnexpectedReturnNULL "layoutNew" result
    fPtr <- FC.newForeignPtr result (pure ())
    GI.touchManagedPtr context
    isDisownedRef <- IO.newIORef Nothing
    return $ GIP.Layout $ GI.ManagedPtr
             { GI.managedForeignPtr = fPtr
             , GI.managedPtrIsDisowned = isDisownedRef
             }

scenarioE ∷ IO ()
scenarioE = do
  crSurf               ← GRC.createImageSurface GRC.FormatARGB32 256 256
  grccfptr ∷ F.ForeignPtr GRC.Cairo
                       ← F.newForeignPtr cairo_destroy =<< GRC.unCairo <$> GRC.create crSurf
  gic@(GIC.Context gicfp) ← GIC.Context <$> GI.newManagedPtr (F.castPtr $ F.unsafeForeignPtrToPtr grccfptr) (F.touchForeignPtr grccfptr)
  gipc                 ← createContext' gic
  gipl                 ← layoutNew'  gipc
  GIP.layoutSetWidth  gipl $ fromIntegral 256
  GIP.layoutSetHeight gipl $ fromIntegral 256
  GIP.layoutSetText   gipl "lol" (-1)
  GIO.objectUnref gipl
  GIO.objectUnref gipc
  GRC.surfaceFinish crSurf

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

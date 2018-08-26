{-# LANGUAGE DataKinds, GADTs, KindSignatures, TypeFamilies #-}
{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving, StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE LambdaCase, MultiWayIf, OverloadedStrings, RecordWildCards, ScopedTypeVariables, TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Wall #-}

module HoloCairo
  (
    Cairo(..), cairoCreate, cairoDestroy
  , runCairo
  , cairoToGICairo
  -- Temporary:  until https://github.com/haskell-gi/haskell-gi/issues/188 is fixed
  , pangoCairoCreateContext
  , pangoLayoutNew
  --
  , crColor
  )
where

import           Control.Monad.IO.Class                   (MonadIO, liftIO)
import           Control.Monad.Trans.Reader               (ReaderT(..))

import qualified Graphics.Rendering.Cairo          as GRC
import qualified Graphics.Rendering.Cairo.Internal as GRC (Render(..), create, destroy)
import qualified Graphics.Rendering.Cairo.Types    as GRC

import qualified Data.GI.Base                      as GI
import qualified GI.Cairo.Structs.Context          as GIC

import qualified Foreign                           as F
import qualified Foreign.ForeignPtr.Unsafe         as F

import           Linear

import           Flatland


-- | Temporary imports for the temporary exports.
--
import qualified Data.IORef                        as IO
import qualified GI.Pango                          as GIP
import qualified GI.Cairo.Structs.Context          as Cairo.Context
import qualified GI.Pango.Objects.Context          as Pango.Context
import qualified Foreign.Concurrent                as FC
import qualified Data.GI.Base.CallStack            as B.CallStack
import           Data.GI.Base.ShortPrelude            (checkUnexpectedReturnNULL)


newtype Cairo = Cairo { _unCairo ∷ F.ForeignPtr GRC.Cairo }

foreign import ccall "&cairo_destroy"
  cairo_destroy ∷ F.FinalizerPtr GRC.Cairo

cairoCreate ∷ (MonadIO m) ⇒ GRC.Surface → m Cairo
cairoCreate s = Cairo <$> (liftIO $ F.newForeignPtr cairo_destroy =<< GRC.unCairo <$> GRC.create s)

cairoDestroy ∷ (MonadIO m) ⇒ Cairo → m ()
cairoDestroy (Cairo fpcr) = liftIO $ GRC.destroy $ GRC.Cairo $ F.unsafeForeignPtrToPtr $ fpcr

runCairo ∷ (MonadIO m) ⇒ Cairo → GRC.Render a → m a
runCairo (Cairo fpGRC) body =
  liftIO $ F.withForeignPtr fpGRC $ \grc →
    (`runReaderT` (GRC.Cairo grc)) $ GRC.runRender
      body

cairoToGICairo ∷ (MonadIO m) ⇒ Cairo → m GIC.Context
cairoToGICairo (Cairo cairoFptr) =
  liftIO $ GIC.Context <$> GI.newManagedPtr (F.castPtr $ F.unsafeForeignPtrToPtr cairoFptr) (F.touchForeignPtr cairoFptr)



foreign import ccall "pango_cairo_create_context" pango_cairo_create_context ::
    F.Ptr Cairo.Context.Context ->            -- cr : TInterface (Name {namespace = "cairo", name = "Context"})
    IO (F.Ptr Pango.Context.Context)

pangoCairoCreateContext ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    Cairo.Context.Context
    {- ^ /@cr@/: a Cairo context -}
    -> m Pango.Context.Context
    {- ^ __Returns:__ the newly created 'GI.Pango.Objects.Context.Context'. Free with
      'GI.GObject.Objects.Object.objectUnref'. -}
pangoCairoCreateContext cr = liftIO $ do
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

foreign import ccall "pango_layout_new" pango_layout_new ::
    F.Ptr Pango.Context.Context ->            -- context : TInterface (Name {namespace = "Pango", name = "Context"})
    IO (F.Ptr GIP.Layout)
{- |
Create a new 'GI.Pango.Objects.Layout.Layout' object with attributes initialized to
default values for a particular 'GI.Pango.Objects.Context.Context'.
-}
pangoLayoutNew ::
    (B.CallStack.HasCallStack, MonadIO m, Pango.Context.IsContext a) =>
    a
    {- ^ /@context@/: a 'GI.Pango.Objects.Context.Context' -}
    -> m GIP.Layout
    {- ^ __Returns:__ the newly allocated 'GI.Pango.Objects.Layout.Layout', with a reference
              count of one, which should be freed with
              'GI.GObject.Objects.Object.objectUnref'. -}
pangoLayoutNew context = liftIO $ do
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


crColor ∷ Co Double → GRC.Render ()
crColor (Co (V4 r g b a)) = GRC.setSourceRGBA r g b a

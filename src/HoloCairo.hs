{-# LANGUAGE DataKinds, GADTs, KindSignatures, TypeFamilies #-}
{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving, StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE LambdaCase, MultiWayIf, OverloadedStrings, RecordWildCards, ScopedTypeVariables, TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Wall #-}

module HoloCairo
  (
    Cairo, cairoCreate, cairoDestroy
  , runCairo
  , cairoToGICairo
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


crColor ∷ Co Double → GRC.Render ()
crColor (Co (V4 r g b a)) = GRC.setSourceRGBA r g b a

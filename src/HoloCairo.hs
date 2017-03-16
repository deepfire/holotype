{-# LANGUAGE DataKinds, GADTs, KindSignatures, TypeFamilies #-}
{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving, StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE LambdaCase, MultiWayIf, OverloadedStrings, RecordWildCards, ScopedTypeVariables, TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Wall #-}

module HoloCairo
  (
    Cairo, cairoCreate
  , runCairo
  , cairoGICairo
  )
where

import           Control.Monad.IO.Class                   (MonadIO, liftIO)
import           Control.Monad.Trans.Reader               (ReaderT(..))

import qualified Graphics.Rendering.Cairo          as GRC
import qualified Graphics.Rendering.Cairo.Internal as GRC (Render(..), create)
import qualified Graphics.Rendering.Cairo.Types    as GRC

import qualified Data.GI.Base                      as GI
import qualified GI.Cairo.Structs.Context          as GIC

import qualified Foreign                           as F
import qualified Foreign.ForeignPtr.Unsafe         as F


newtype Cairo = Cairo { _unCairo ∷ F.ForeignPtr GRC.Cairo }

foreign import ccall "&cairo_destroy"
  cairo_destroy ∷ F.FinalizerPtr GRC.Cairo

cairoCreate ∷ (MonadIO m) ⇒ GRC.Surface → m Cairo
cairoCreate s = Cairo <$> (liftIO $ F.newForeignPtr cairo_destroy =<< GRC.unCairo <$> GRC.create s)

runCairo ∷ (MonadIO m) ⇒ Cairo → GRC.Render a → m a
runCairo (Cairo fpGRC) body =
  liftIO $ F.withForeignPtr fpGRC $ \grc →
    (`runReaderT` (GRC.Cairo grc)) $ GRC.runRender
      body

cairoGICairo ∷ (MonadIO m) ⇒ Cairo → m GIC.Context
cairoGICairo (Cairo cairoFptr) =
  liftIO $ GIC.Context <$> GI.newManagedPtr (F.castPtr $ F.unsafeForeignPtrToPtr cairoFptr) (F.touchForeignPtr cairoFptr)

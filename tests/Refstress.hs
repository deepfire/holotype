{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors -Wno-unused-imports -Wno-type-defaults -Wno-unused-matches -Wno-unused-do-bind #-}
module Main where
import           Control.Applicative
import           Control.Lens
import           Control.Monad.IO.Class
import qualified Control.Monad.Ref
import qualified Data.Time.Clock                   as Time
import           Prelude.Unicode
import           Reflex                            hiding (Additive)
import           Reflex.GLFW
import qualified "GLFW-b" Graphics.UI.GLFW         as GLFW

main ∷ IO ()
main = do
  basicGL33Host "reflex stress" guest

guest ∷ ∀ t m. ReflexGLFWGuest t m
guest win _evCtl windowFrameE inputE = do
  liftIO $ putStrLn "So it begins.."
  let keyNameE = (\(U (EventKey  _ k _ GLFW.KeyState'Pressed _))→ replicate 2 $ last $ show k) <$>
                 ffilter (\case (U (EventKey  _ k _ GLFW.KeyState'Pressed _))→ True; _ → False) inputE
  -- scen1 keyNameE
  scen2 keyNameE
  -- loopDyn

  hold False $ fmap (\case (U (EventKey  _ GLFW.Key'Escape    _ GLFW.KeyState'Pressed   _))→ True; _ → False) inputE

delayDyn ∷ ReflexGLFWCtx t m ⇒ Dynamic t a → ReflexGLFW t m (Dynamic t a)
delayDyn dyn =
  buildDynamic (sample $ current dyn) ∘ tag (current dyn) =<< delay 1 (updated dyn)

scen2 ∷ (ReflexGLFWCtx t m, a ~ String) ⇒ Event t a → ReflexGLFW t m (Dynamic t a)
scen2 inputE = mdo
  inputD    ← holdDyn "12" inputE
  let outD   = zipDynWith (\input loop→ input) inputD delayeD
  delayeD   ← delayDyn outD
  pure delayeD

scen1 ∷ (ReflexGLFWCtx t m, a ~ String) ⇒ Event t a → ReflexGLFW t m (Dynamic t a)
scen1 inputE = mdo
  cE   ← performEvent $ attachPromptlyDyn c'D inputE <&>
         \(loop, input)→ liftIO $ do
           putStrLn $ input <> " " <> loop
           pure (drop (length input - 2) input <> loop)
  c'D  ← delayDyn =<< holdDyn "  " cE
  pure c'D

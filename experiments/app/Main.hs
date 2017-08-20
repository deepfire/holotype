{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Wall -Wno-unused-imports -Wno-unticked-promoted-constructors -Wno-type-defaults -Wno-missing-signatures #-}

module Main where

import           Control.Applicative
import           Control.Applicative.Free
import           Prelude.Unicode
import           Text.Printf              (printf)


type Dim  = (Int, Int)
type Pos  = (Int, Int)

data KPos  = Pos | NoPos
data KSize = Size | NoSize

data Ctx where
  Ctx ∷
    { limit ∷ Dim
    , coord ∷ Pos
    } → Ctx
  deriving (Show)

data CF a where
  C ∷
    { ctx ∷ Maybe Ctx
    , xs  ∷ W (CA a)
    } → CF a
  deriving Functor
type CA a = Ap CF a

data W a where
  CHBox   ∷ { subs    ∷ [a] }                     → W a
  CVBox   ∷ { subs    ∷ [a] }                     → W a
  CFBox   ∷ { subs    ∷ [a] }                     → W a
  CReq    ∷ { canGrow ∷ Bool, dim ∷ Dim, obj ∷ a} → W a
  CConstr ∷ { dim     ∷ Dim,  obj ∷ a }           → W a
  CPos    ∷ { posOf   ∷ Pos,  obj ∷ a }           → W a
  deriving Functor

pass ∷ Ctx → CF a → CF a
pass c@Ctx{..} (C Nothing w@CReq{..}) =
  C (Just c)
  $ w { obj = runAp (liftAp ∘ pass c) obj }
pass c@Ctx{..} (C Nothing CFBox{..}) =
  C (Just c)
  $ CFBox { subs = runAp (liftAp ∘ pass c) <$> subs }

hbox, vbox, fbox ∷ [CA a] → CA a
constr      ∷  Dim → CA a → CA a
hbox        = liftAp ∘ C Nothing ∘ CHBox
vbox        = liftAp ∘ C Nothing ∘ CVBox
fbox        = liftAp ∘ C Nothing ∘ CFBox
constr d    = liftAp ∘ C Nothing ∘ CConstr d
req         ∷  Bool → Dim → CA a → CA a
req  g d    = liftAp ∘ C Nothing ∘ CReq g d
pos         ∷  Pos → CA a → CA a
pos    p    = liftAp ∘ C Nothing ∘ CPos p

scene =
  hbox $
  [ req True (10, 10) $ pure "a"
  , req True (10, 10) $ pure "b"
  , req True (10, 10) $ pure "c"
  , req True (10, 10) $ pure "d"
  ]


main ∷ IO ()
main = do

  putStrLn "You are standing at the end of a road before a small brick building."
  putStrLn "Around you is a forest.  A small stream flows out of the building and"
  putStrLn "down a gully."

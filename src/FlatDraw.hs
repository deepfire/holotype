{-# LANGUAGE DataKinds, KindSignatures, TypeApplications, TypeInType #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, InstanceSigs, MultiParamTypeClasses, NoMonomorphismRestriction, RankNTypes, TypeSynonymInstances, UndecidableInstances #-}
{-# LANGUAGE GADTs, TypeFamilies, TypeFamilyDependencies #-}
{-# LANGUAGE BangPatterns, MultiWayIf, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TypeOperators #-}
{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveGeneric, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors -Wno-orphans #-}
module FlatDraw
  ( -- * Colors
    coOpaq, coGray, coMult
  , coSetSourceColor, coGradientSet
  , coPatternGradLinear, coWithMaybePatternGradLinear, coPatternGradRadial

    -- * Drawing: simple
  , thLineSet, poArc

  -- * Rounded rectangle
  , wrapRoundedRectFeatures
  , executeFeature
  )
where

-- Basis
import           Prelude.Unicode

-- General types
import           Control.Lens                      hiding (children)
import           Control.Monad.State
import           Data.Maybe

-- Algebra
import           Linear

-- Manually-bound Cairo
import qualified Graphics.Rendering.Cairo          as GRC
import qualified Graphics.Rendering.Cairo.Internal as GRCI

-- Local
import Flatland


-- * Colors
coOpaq ∷ Num a ⇒ a → a → a → Co a
coOpaq r g b = Co $ V4 r g b 1

coGray ∷ a → a → Co a
coGray x a = Co $ V4 x x x a

coMult ∷ Num a ⇒ a → Co a → Co a
coMult x (Co (V4 r g b a)) = Co $ V4 (r*x) (g*x) (b*x) a

coSetSourceColor ∷ Co Double → GRCI.Render ()
coSetSourceColor !(Co (V4 r g b a))
  = GRC.setSourceRGBA r g b a

coGradientSet ∷ Co Double → Co Double → GRC.Pattern → IO ()
coGradientSet !(Co (V4 rs gs bs as)) !(Co (V4 re' ge be ae)) pat = do
  GRCI.patternAddColorStopRGBA pat 0 rs gs bs as
  GRCI.patternAddColorStopRGBA pat 1 re' ge be ae

-- TODO:  figure out if we can get away with pattern singletons
coPatternGradLinear ∷ Po Double → Co Double → Po Double → Co Double → IO GRC.Pattern
coPatternGradLinear !(Po (V2 xs ys)) !sco !(Po (V2 xe ye)) !eco = do
  p ← GRCI.patternCreateLinear xs ys xe ye
  coGradientSet sco eco p
  pure p

coWithMaybePatternGradLinear ∷ Po Double → Co Double → Po Double → Co Double → Either (GRCI.Render ()) (GRC.Pattern → GRCI.Render ()) → GRCI.Render ()
coWithMaybePatternGradLinear _ _ _ _ (Left f) = f
coWithMaybePatternGradLinear !(Po (V2 xs ys)) !sco !(Po (V2 xe ye)) !eco (Right f) =
  GRC.withLinearPattern xs ys xe ye $ \pat → do
    liftIO $ coGradientSet sco eco pat
    f pat

coPatternGradRadial ∷ Po Double → Double → Co Double → Po Double → Double → Co Double → IO GRC.Pattern
coPatternGradRadial !(Po (V2 xi yi)) !ir !ico !(Po (V2 xo yo)) !origin !oco = do
  p ← GRCI.patternCreateRadial xi yi ir xo yo origin
  coGradientSet ico oco p
  pure p


-- * Drawing: simple
--
thLineSet ∷ Th Double → GRCI.Render ()
thLineSet !(Th th)
  = GRC.setLineWidth th

poArc ∷ Po Double → Double → An2 Double → GRCI.Render ()
poArc !(Po (V2 x y)) !r !(An2 (V2 angs ange))
  = GRC.arc x y r angs ange


-- * Drawing: complex

-- | Description of rounded rectangle features -- sides and corners.
--
data RoundRectFeature dk a where
  RRCorn ∷
    { _rrcOri ∷ !(Orient Corn)
    , _rrcCt ∷ !(Po a)
    , _rrcAn ∷ !(An2 a)
    , _rrcR  ∷ !(R a)
    } → RoundRectFeature Corn a
  RRSide ∷
    { _rrsOri ∷ !(Orient Card)
    , _rrsFr ∷ !(Po a)
    , _rrsTo ∷ !(Po a)
    } → RoundRectFeature Card a
deriving instance Show a ⇒ Show (RoundRectFeature dk a)

data WRoundRectFeature a where
  WRR ∷ RoundRectFeature dk a → WRoundRectFeature a

-- | Wrap rendering as a rounded rectangle.
--   XXX: see if `Linear.V2.perp` can help with that.
--   XXX: lost type safety..
wrapRoundedRectFeatures ∷ ∀ a b d. (Floating d, FromArea a b LU RB d) ⇒ Area' a b d → R d → Th d → [WRoundRectFeature d]
wrapRoundedRectFeatures area rr@(R r) (Th th) =
  let lurb ∷ Area'LURB d = from'area area
      lurb'δ             = V2 (th / 2) (th / 2)
      Area (LU (Po (V2 w n)))
           (RB (Po (V2 e s)))
                         = lurb & area'a∘lu'po %~ po'add lurb'δ
                                & area'b∘rb'po %~ po'sub lurb'δ
      !degrees           = pi/180
  in [WRR $ RRSide ON  (po (w + r)  n)      (po  (e - r) n)
     ,WRR $ RRCorn ONE (po (e - r) (n + r)) (an2 (-90 ⋅ degrees)   (0 ⋅ degrees)) rr
     ,WRR $ RRSide OE  (po  e      (n + r)) (po   e     (s - r))
     ,WRR $ RRCorn OSE (po (e - r) (s - r)) (an2   (0 ⋅ degrees)  (90 ⋅ degrees)) rr
     ,WRR $ RRSide OS  (po (w + r)  s)      (po  (e - r) s)
     ,WRR $ RRCorn OSW (po (w + r) (s - r)) (an2  (90 ⋅ degrees) (180 ⋅ degrees)) rr
     ,WRR $ RRSide OW  (po  w      (n + r)) (po   w     (s - r))
     ,WRR $ RRCorn ONW (po (w + r) (n + r)) (an2 (180 ⋅ degrees) (270 ⋅ degrees)) rr]

executeFeature ∷ Maybe (Co Double) → Maybe (Co Double) → WRoundRectFeature Double → GRC.Render ()
executeFeature !cStart !cEnd !(WRR (RRSide _ (Po (V2 sx sy)) (Po (V2 ex' ey')))) = do
  -- pat ← UN.unsafeInterleaveIO $ coPatternGradLinear rrsFr cStart rrsTo cEnd
  if | cStart ≢ cEnd    → error "Not implemented: sidewise gradients."
     | Nothing ← cStart → pure ()
     | Just c  ← cStart → coSetSourceColor c
  GRC.moveTo sx sy
  GRC.lineTo ex' ey'
executeFeature !cStart !cEnd !(WRR (RRCorn o c@(Po (V2 cx cy)) (An2 (V2 sa ea)) r)) = do
  let (cs, ce) = chord'CW o c r
  coWithMaybePatternGradLinear cs (fromJust cStart) ce (fromJust cEnd) $
    if cStart ≢ cEnd
    then Right $ \grad → do
      GRC.setSource grad
      GRC.arc cx cy (_r'val r) sa ea
    else Left $ do
      case cStart of
        Nothing → pure ()
        Just c' → coSetSourceColor c'
      GRC.arc cx cy (_r'val r) sa ea

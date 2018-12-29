{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors -Wno-orphans #-}
module Graphics.FlatDraw
  ( -- * Colors
    opaque, white, gray, black, red, green, blue, coMult
  , coSetSourceColor, coGradientSet
  , coPatternGradLinear, coWithMaybePatternGradLinear, coPatternGradRadial

    -- * Drawing: simple
  , dpx
  , thLineSet, poArc
  , paintFill
  , paintRect
  , paintCircle
  , paintDebugColorFrames

  -- * Rounded rectangle
  , wrapRoundedRectFeatures
  , executeFeature
  , paintRoundedRect
  )
where

import           Control.Lens                      hiding (children)
import           Control.Monad.State
import           Data.List                                (cycle)
import           Data.Maybe
import           Linear
import           Prelude.Unicode
import qualified Graphics.Rendering.Cairo          as GRC
import qualified Graphics.Rendering.Cairo.Internal as GRCI

-- Local
import           Graphics.Flatland


-- * Colors
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
dpx ∷ Po Double → Co Double → GRCI.Render ()
dpx (Po (V2 x y)) color = coSetSourceColor color >>
                          -- GRC.rectangle (x) (y) 1 1 >> GRC.fill
                          GRC.rectangle (x-1) (y-1) 3 3 >> GRC.fill

thLineSet ∷ Th Double → GRCI.Render ()
thLineSet !(Th th)
  = GRC.setLineWidth th

poArc ∷ Po Double → Double → An2 Double → GRCI.Render ()
poArc !(Po (V2 x y)) !r !(An2 (V2 angs ange))
  = GRC.arc x y r angs ange

paintFill ∷ Co Double → GRCI.Render ()
paintFill color = do
  GRC.save
  GRC.setOperator GRCI.OperatorSource
  coSetSourceColor color
  GRC.paint
  GRC.restore

paintRect ∷ Co Double → Di (Unit PU) → GRCI.Render ()
paintRect color dim' = do
  coSetSourceColor color
  GRC.rectangle (0) (0) (fromPU $ dim'^.di'v._x) (fromPU $ dim'^.di'v._y)
  GRC.fill

paintCircle ∷ Co Double → R Double → Po Double → GRC.Render ()
paintCircle color (R r) (Po (V2 x y)) = do
  coSetSourceColor color
  GRC.newPath >> thLineSet 0
  GRC.arc x y r 0 180
  GRC.arc x y r 180 0
  GRC.fill

paintDebugColorFrames ∷ Di Int → GRCI.Render ()
paintDebugColorFrames (Di (V2 w h)) = do
  let n ∷ Int = floor $ (fromIntegral $ min w h) / (2.0 ∷ Double)
  GRC.setLineWidth 1
  forM_ (take n $ zip [0..] $ cycle [red, green, blue]) $
    \(x ∷ Int, color)→ do
      coSetSourceColor color
      GRC.rectangle (fromIntegral x) (fromIntegral x) (fromIntegral $ w - x) (fromIntegral $ h - x)
      GRC.stroke


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

deriving instance Show a ⇒ Show (WRoundRectFeature a)

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

paintRoundedRect ∷ (Co Double, Co Double) → Th Double → R Double → Wi Double → Pad Double → GRC.Render ()
paintRoundedRect (fgColor, bgColor) lw r@(R radius) (Wi interfocal) (Pad pad) = do
  coSetSourceColor fgColor
  let h = 2 * radius
      w = h + interfocal
      area = Area (LU $ po pad pad) (Size $ unsafe'di w h)
      [n, ne, _, se, _, sw, _, nw] = wrapRoundedRectFeatures area r lw
  GRC.newPath >> thLineSet lw
  forM_ [n, ne, se, sw, nw] $ executeFeature Nothing Nothing
  coSetSourceColor bgColor >>
    GRC.fillPreserve
  coSetSourceColor fgColor >>
    GRC.stroke

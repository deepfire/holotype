module Holo.Instances.As
where

import qualified GI.Pango                          as GIP
import           Prelude.Unicode

import           Graphics.Flatland
import           Graphics.FlatDraw
import           Graphics.Cairo                           (FKind(..))
import qualified Graphics.Cairo                    as Cr
import           Holo.Classes


instance As () where
  type Denoted () = ()
  type     Sty () = ()
  type  IStruc () = ()
  type     Vis () = ()
  defAs             _px   = ()
  defSty            _px   = ()
  compSty            _x   = ()
  sizeRequest  _port () () _sty = pure $ (,) () diNothing
  setupVis     _port () () _sty _area _istruc      _drw = pure ()
  render       _port () () _sty _off  _istruc _vis _drw = pure ()
  freeVis           _px                       _vis      = pure ()


-- * Co Double - Rect - Di (Unit PU) - Interp (Di (Unit PU))
--
data Rect = Rect

instance As Rect where
  type Denoted Rect              = Di (Unit PU)
  type Sty     Rect              = Co Double
  defAs                        _ = Rect
  defSty                       _ = blue
  sizeRequest port Rect dim _sty = pure $ (,) () $ Just ∘ fromPU ∘ fromUnit (Port.portDΠ port) <$> dim
  setupVis         _ _ _ _ _ _ _ = pure ()
  render Port.Port{portSettings=Port.Settings{..}} Rect dim' color () shift drw@Drawable{..} () = do
    let dim = fromUnit sttsDΠ <$> dim'
    Cr.runCairo dCairo $ do
      Cr.crMoveTo shift
      paintRect color dim -- $ PUs <$> _area^.area'b.size'di
    Port.drawableContentToGPU drw


-- * TextS - TextVisual - TextLine - Text - Interp Text a
--
data TextS where
  TextS ∷
    { _tsFontKey     ∷ Cr.FontKey
    , _tsSizeSpec    ∷ Cr.TextSizeSpec PU
    , _tsColor       ∷ Co Double
    } → TextS
tsFontKey   ∷ Lens' TextS Cr.FontKey
tsFontKey  f ts@(TextS x _ _) = (\xx→ts{_tsFontKey=xx})  <$> f x
tsSizeSpec  ∷ Lens' TextS (Cr.TextSizeSpec PU)
tsSizeSpec f ts@(TextS _ x _) = (\xx→ts{_tsSizeSpec=xx}) <$> f x
tsColor     ∷ Lens' TextS (Co Double)
tsColor    f ts@(TextS _ _ x) = (\xx→ts{_tsColor=xx})    <$> f x
data TextVisual where
  TextVisual ∷
    { tFont          ∷ Cr.WFont Bound
    , tLayout        ∷ GIP.Layout
    } → TextVisual

data TextLine = TextLine

instance As TextLine where
  type Denoted TextLine = Text
  type Sty     TextLine = TextS
  type Vis     TextLine = TextVisual
  defAs            _ = TextLine
  defSty           _ = TextS
    { _tsFontKey     = "default"
    , _tsSizeSpec    = Cr.TextSizeSpec Nothing Cr.OneLine
    , _tsColor       = white
    }
  sizeRequest port TextLine content TextS{..} = do
    let font = Port.portFont' port _tsFontKey -- XXX: non-total
    (,) () ∘ (Just ∘ fromPU <$>) ∘ either errorT id <$> Cr.fontQuerySize font (convert (Port.portDΠ port) _tsSizeSpec) (partial (≢ "") content)
  setupVis port TextLine _content TextS{..} () area' drw = do
    -- 1. find font, 2. bind font to GIC, 3. create layout
    let font = Port.portFont' port _tsFontKey -- XXX: non-total
        tDim = fromUnit (Port.portDΠ port) ∘ PUs <$> dimOf area'
    -- liftIO $ putStrLn $ printf "setupVisual T.Text: %s → %s" (show _tsFontKey) (show font)
    (,) tFont tLayout ← Port.drawableBindFontLayout (Port.portDΠ port) drw font tDim _tsSizeSpec
    -- drawableBindFontLayout allocates:
    --   GIPC.createContext gic  -- released by FFI finalizers
    --   GIP.layoutNew      gipc -- same as above
    pure $ TextVisual{..}
  render _ TextLine content TextS{..} () shift drw TextVisual{..} =
    -- 1. execute GIP draw & GIPC composition
    Port.drawableDrawText drw tLayout _tsColor shift content
  freeVis _ TextVisual{..} =
    Cr.unbindFontLayout tFont tLayout


-- * Switch - Bool - Interp Bool x
--
data SwitchS
  = SwitchS
  { ssRadius     ∷ Double
  , ssTolerance  ∷ Double
  , ssLineWeight ∷ Double
  , ssInterfocal ∷ Double
  , ssColorOn    ∷ Co Double
  , ssColorOff   ∷ Co Double
  } -- specifying scale as part of style is a smell
--
--  Height: 2*(radius + line-weight + tolerance + line-weight + 0.5*(line-weight + tolerance))
--  Width:  2*(0.5*interfocal + radius + line-weight + tolerance + line-weight + 0.5*(line-weight + tolerance))
--
--     ,------T-----------------L------.
--    /       V  Tolerance              \
--   /   /~~~~~~`\              Line    `\
--  /   /          \            weight    \
--  /  /            \                     \
--  T→ |     o--R--→|  Radius             |
--  \  \            /                     /
--  \   \          /                     /
--   \   \_______/`                     /`
--    \                                /
--     ```--------------------------'''
--
-- defSty      ∷               Proxy r             → Sty r
-- compSty     ∷                     r             → Sty r
-- sizeRequest ∷ MonadIO m ⇒ VPort → r → Denoted r → Sty r → m (Di (Maybe Double))
-- setupVis    ∷ MonadIO m ⇒ VPort → r → Denoted r → Sty r → Area'LU Double → Drawable → m (Vis r)
-- render      ∷ MonadIO m ⇒ VPort → r → Denoted r → Sty r                  → Drawable → Vis r → m () -- ^ Update visual.
-- freeVis     ∷ MonadIO m ⇒   Proxy r                                                 → Vis r → m ()
data Switch = Switch

instance As Switch where
  type Denoted Switch = Bool
  type Sty     Switch = SwitchS
  defAs   _ = Switch
  defSty  _ = SwitchS
    { ssRadius     = 10
    , ssTolerance  = 2    -- pixel counting → 2
    , ssLineWeight = 2    -- pixel counting → 3
    , ssInterfocal = 12
    , ssColorOn    = green
    , ssColorOff   = gray 0.4 1.0
    }
  sizeRequest _ Switch _ SwitchS{..} =
    let padding = 0.5 * (ssLineWeight + ssTolerance)
        h = 2 * (ssRadius + ssTolerance + ssLineWeight + padding)
        w = h + ssInterfocal
    in pure ∘ (,) () ∘ (Just <$>) ∘ Di $ V2 w h
  setupVis _ _ _ _ _ _ _ = pure ()
  render _ Switch val SwitchS{..} () shift d@Drawable{..} () = do
    let padding  = 0.5 * (ssLineWeight + ssTolerance)
        rrRadius = ssRadius + ssTolerance + ssLineWeight
        hCenter  = rrRadius + padding
    Cr.runCairo dCairo $ do
      -- paintDebugColorFrames dDi
      Cr.crMoveTo shift
      paintRoundedRect (white, if val then ssColorOn else ssColorOff)
        (Th ssLineWeight) (R rrRadius) (Wi ssInterfocal) (Pad padding)
      paintCircle white (R ssRadius) (Po $ V2 (hCenter + if val then 0 else ssInterfocal) hCenter)
    Port.drawableContentToGPU d
-- paintRoundedRect color lw@(Th lineWeight) r@(R radius) (Wi interfocal) (Pad pad) = do


-- * Text → As → As
--
newtype Labelled a = Labelled (Text, a)

instance As a ⇒ As (Labelled a) where
  type Denoted (Labelled a) = Denoted a
  type Sty     (Labelled a) = (TextS,      Sty a)
  type IStruc  (Labelled a) = (Wi Double, IStruc a)
  type Vis     (Labelled a) = (TextVisual, Vis a)
  defAs                   _ = Labelled ("fi", defAs Proxy)
  defSty                  _ = (defSty $ Proxy @TextLine, defSty $ Proxy @a)
  sizeRequest p (Labelled (label, asA)) content (tSty, aSty) = do
    let effLabel = label <> ": "
    (_, rTL@(Di (V2 w _))) ← ((fromMaybe 0 <$>) <$>) <$> sizeRequest p TextLine effLabel tSty
    (aS, rA)               ← ((fromMaybe 0 <$>) <$>) <$> sizeRequest p asA      content  aSty
    pure $ ((Wi w, aS),) $ Just <$> (_reqt'di $ reqt'add X (Reqt rTL) (Reqt rA))
  setupVis        p (Labelled (label, asA)) content (sT, sA) (Wi cShift, isA) area' drw = do
    let (areaL, areaR) = area'split'start X cShift area'
    let effLabel = label <> ": "
    vT ← setupVis p TextLine                effLabel sT      ()               areaL drw
    vA ← setupVis p asA                     content      sA              isA  areaR drw
    pure $ (,) vT vA
  render          p (Labelled (label, asA)) content (sT, sA) (Wi cShift, isA) shift drw (tV, aV) = do
    let effLabel = label <> ": "
    render        p TextLine   effLabel              sT      ()               shift drw  tV
    render        p asA                     content      sA  isA             (shift & po'd X %~ (+cShift)) drw aV
  freeVis _ (tV, aV) = do
    freeVis (Proxy @TextLine) tV
    freeVis (Proxy @a) aV


-- * Axis → a → b → (,) a b
--
--  XXX:  what could be a meaning that wouldn't involve managing input focus internally?
--        1. two Mutables with non-conflicting subscriptions?
instance (As a, As b) ⇒ As (Axis, (a, b)) where
  type Denoted (Axis, (a, b)) = (Denoted a, Denoted b)
  type Sty     (Axis, (a, b)) = (Sty     a,     Sty b)
  type IStruc  (Axis, (a, b)) = (Double
                                ,(IStruc a,  IStruc b))
  type Vis     (Axis, (a, b)) = (Vis     a,     Vis b)
  defAs                     _ = (X, (defAs Proxy, defAs Proxy))
  defSty                    _ = (defSty $ Proxy @a, defSty $ Proxy @b)
  sizeRequest p (ax, (asA, asB)) (cA, cB) (sA, sB) = do
    (isA, rA@(Di v)) ← ((fromMaybe 0 <$>) <$>) <$> sizeRequest p asA cA sA
    (isB, rB)        ← ((fromMaybe 0 <$>) <$>) <$> sizeRequest p asB cB sB
    pure $ ((v2'proj ax v, (isA, isB)),) $ Just <$> (_reqt'di $ reqt'add ax (Reqt rA) (Reqt rB))
  setupVis    p (ax, (asA, asB)) (cA, cB) (sA, sB) (shift, (isA, isB)) area' drw = do
    let (aLU, aRB) = area'split'start ax shift area'
    vA ← setupVis p asA           cA       sA               isA        aLU   drw
    vB ← setupVis p asB               cB       sB                isB   aRB   drw
    pure $ (,) vA vB
  render      p (ax, (asA, asB)) (cA, cB) (sA, sB) (shiftN, (isA, isB)) shift drw (aV, bV) = do
    render        p asA           cA       sA                isA        shift drw  aV
    render        p asB               cB       sB                 isB  (shift & po'd ax %~ (+shiftN)) drw bV
  freeVis _ (aV, bV) = do
    freeVis (Proxy @a) aV
    freeVis (Proxy @b) bV

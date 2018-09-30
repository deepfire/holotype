{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors -Wno-unused-imports -Wno-type-defaults #-}
module Holotype where

import           Control.Arrow
import           Control.Monad
import           Data.Foldable
import           Data.Functor.Misc                        (Const2(..))
import           Data.Maybe
import           Data.Semigroup
import           Data.Singletons
import           Data.Text                                (Text)
import           Data.Tuple
import           Linear                            hiding (trace)
import           Prelude                           hiding (id, Word)
import           Reflex                            hiding (Query, Query(..))
import           Reflex.GLFW                              (ReflexGLFW, ReflexGLFWCtx, ReflexGLFWGuest, InputU(..))
import qualified Codec.Picture                     as Juicy
import qualified Codec.Picture.Saving              as Juicy
import qualified Control.Concurrent.STM            as STM
import qualified Control.Monad.Ref
import qualified Data.ByteString.Lazy              as B
import qualified Data.Map.Monoidal.Strict          as MMap
import qualified Data.Map.Strict                   as M
import qualified Data.Sequence                     as Seq
import qualified Data.Set                          as Set
import qualified Data.Text                         as T
import qualified Data.Text.Zipper                  as T
import qualified Data.Time.Clock                   as Time
import qualified Data.TypeMap.Dynamic              as TM
import qualified Data.Unique                       as U
import qualified GHC.Generics                      as GHC
import qualified Graphics.GL.Core33                as GL
import qualified Options.Applicative               as Opt
import qualified Reflex.GLFW                       as GLFW
import qualified Text.Parser.Char                  as P
import qualified Text.Parser.Combinators           as P
import qualified Text.Parser.Token                 as P
import qualified Text.Trifecta.Parser              as P
import qualified Text.Trifecta.Result              as P

-- Local imports
import           Elsewhere
import           Flatland
import           Flex

import           HoloTypes

import           HoloPrelude                       hiding ((<>))
import           Holo                                     (tsFontKey, tsSizeSpec, tsColor)
import qualified Holo
import qualified HoloCairo                         as Cr
import           HoloPort
import qualified HoloOS                            as HOS

-- TEMPORARY
import           MRecord
import           Generics.SOP                             (Proxy)
import qualified Generics.SOP                      as SOP
import qualified "GLFW-b" Graphics.UI.GLFW         as GLFW


newPortFrame ∷ ReflexGLFWCtx t m ⇒ Event t Port → ReflexGLFW t m (Event t (Port, Frame))
newPortFrame portFrameE = performEvent $ portFrameE <&>
  \port@Port{..}→ do
    newFrame ← portNextFrame port
    pure (port, newFrame)

type Avg a = (Int, Int, [a])
avgStep ∷ Fractional a ⇒ a → (a, Avg a) → (a, Avg a)
avgStep x (_, (lim, cur, xs)) =
  let (ncur, nxs) = if cur < lim
                    then (cur + 1, x:xs)
                    else (lim,     x:Prelude.init xs)
  in ((sum nxs) / fromIntegral ncur, (lim, ncur, nxs))

average ∷ (Fractional a, ReflexGLFWCtx t m) ⇒ Int → Event t a → ReflexGLFW t m (Dynamic t a)
average n e = (fst <$>) <$> foldDyn avgStep (0, (n, 0, [])) e



type HoloBlank      = Holo.Item Holo.PBlank
type Widget   t   a =                           (Dynamic t Subscription, Dynamic t a)
value               ∷                           (Dynamic t Subscription, Dynamic t a) → Dynamic t a
value               = snd
type WidgetM  t m a = Reflex t ⇒ ReflexGLFW t m (Widget t a)
type InputMux t     = EventSelector t (Const2 IdToken Input)

routeInput ∷ ∀ t m. Reflex t ⇒ Event t Input → Event t IdToken → Dynamic t Subscription → ReflexGLFW t m (InputMux t) -- Subscription → InputMux t WorldEvent
routeInput inputE pickedE subsD = do
  -- XXX: this accumulates the focus
  pickeD ← holdDyn Nothing $ Just <$> pickedE
  let inputs = zipDynWith (,) pickeD (traceDyn "===== new subs: " subsD)
      routed ∷ Event t (M.Map IdToken Input)
      routed = routeSingle <$> attachPromptlyDyn inputs inputE
      routeSingle ∷ ((Maybe IdToken, Subscription), Input) → M.Map IdToken Input
      routeSingle ((picked, Subscription ss), ev) =
        case MMap.lookup (GLFW.eventUType $ inInput ev) ss of
          Nothing         → --trace ("rejected type: "<>show ev<>"/"<>show (inInput ev))
                            mempty -- no-one cares, nothing happened..
          Just potentials →
            let matches = flip Seq.filter potentials (flip inputMatch ev ∘ snd)
            in case (picked, toList matches) of
                 (_, [])               → --trace ("rejected unmatched: "<>show ev)
                                         mempty
                 (Just pick, matched)  → case lookup pick matched of
                   Nothing → mempty -- XXX: mis-focus -- we allowed to focus a non-interested entity
                   Just _  → M.singleton pick ev
                 (Nothing, (tok, _):_) → M.singleton tok ev
  pure $ fanMap routed



widget ∷ (IdToken → ReflexGLFW t m (InputMask, Dynamic t a)) → WidgetM t m a
widget ctor = do
  token ← newId
  (,) im@(InputMask em) w ← ctor token
  let emTypes = GLFW.eventMaskTypes em
      mmap    = MMap.fromList $ zip emTypes $ repeat $ Seq.singleton (token, im)
  pure $ (,) (constDyn $ Subscription mmap) w

mkColorRectD ∷ Dynamic t (Di (Unit PU)) → Dynamic t (Co Double) → WidgetM t m HoloBlank
mkColorRectD diD coD = widget $ \tokenV → pure $ (,) mempty $
  (\val@(Holo.Rect dim _)→
      Holo.leaf tokenV defStyle val & size.~(Just∘fromPU <$> dim))
  <$> zipDynWith Holo.Rect diD coD

mkTextD ∷ Dynamic t (Style T.Text) → Dynamic t T.Text → WidgetM t m HoloBlank
mkTextD styleD valD = widget $ \tokenV → pure $ (,) mempty $
  zipDynWith (Holo.leaf tokenV) styleD valD

mkTextEntryD ∷ InputMux t → Behavior t (Style T.Text) → T.Text → WidgetM t m (T.Text, HoloBlank)
mkTextEntryD mux styleB initialV = widget $ \tokenV → do
  let editE     = select mux $ Const2 tokenV
  valD         ← (zipperText <$>) <$> foldDyn (\Edit{..} tz → eeEdit tz) (textZipper [initialV]) (translateEditEvent <$> editE)
  setupE       ← getPostBuild
  initV        ← sample $ current valD
  let holoE     = attachWith (Holo.leaf tokenV) styleB $ leftmost [updated valD, initV <$ setupE]
  holdDyn (initialV, Holo.emptyHolo) (attachPromptlyDyn valD holoE)
   <&> (,) editMaskKeys

mkTextEntryValidatedD ∷ InputMux t → Behavior t (Style T.Text) → T.Text → (T.Text → Bool) → WidgetM t m (T.Text, HoloBlank)
mkTextEntryValidatedD mux styleB initialV testF = do
  unless (testF initialV) $
    error $ "Initial value not accepted by test: " <> T.unpack initialV
  (subD, textD) ← mkTextEntryD mux styleB initialV
  initial ← sample $ current textD
  foldDyn (\(new, newHoloi) (oldValid, _)→
               (if testF new then new else oldValid, newHoloi))
    initial (updated textD)
    <&> (subD,)

vboxD ∷ ∀ t m. Reflex t ⇒ [Widget t HoloBlank] → WidgetM t m HoloBlank
vboxD chi = do
  let dyn ∷ (Dynamic t Subscription, Dynamic t [HoloBlank])
      dyn = foldr (\(s, hb) (ss, hbs)→ (zipDynWith (<>) s ss, zipDynWith (:) hb hbs))
            (constDyn mempty, constDyn [])
            chi
  pure $ (id *** (Holo.vbox <$>)) dyn

data EditEvent where
  Edit ∷
    { eeEdit ∷ T.TextZipper T.Text → T.TextZipper T.Text
    } → EditEvent

translateEditEvent ∷ Input → EditEvent
translateEditEvent = \case
  (Input (U (GLFW.EventChar _ c)))                                              → Edit $ T.insertChar c
  (Input (U (GLFW.EventKey  _ GLFW.Key'Enter     _ GLFW.KeyState'Pressed   _))) → Edit $ T.breakLine
  (Input (U (GLFW.EventKey  _ GLFW.Key'Backspace _ GLFW.KeyState'Pressed   _))) → Edit $ T.deletePrevChar
  (Input (U (GLFW.EventKey  _ GLFW.Key'Delete    _ GLFW.KeyState'Pressed   _))) → Edit $ T.deleteChar
  (Input (U (GLFW.EventKey  _ GLFW.Key'Left      _ GLFW.KeyState'Pressed   _))) → Edit $ T.moveLeft
  (Input (U (GLFW.EventKey  _ GLFW.Key'Up        _ GLFW.KeyState'Pressed   _))) → Edit $ T.moveUp
  (Input (U (GLFW.EventKey  _ GLFW.Key'Right     _ GLFW.KeyState'Pressed   _))) → Edit $ T.moveRight
  (Input (U (GLFW.EventKey  _ GLFW.Key'Down      _ GLFW.KeyState'Pressed   _))) → Edit $ T.moveDown
  (Input (U (GLFW.EventKey  _ GLFW.Key'Home      _ GLFW.KeyState'Pressed   _))) → Edit $ T.gotoBOL
  (Input (U (GLFW.EventKey  _ GLFW.Key'End       _ GLFW.KeyState'Pressed   _))) → Edit $ T.gotoEOL
  (Input (U (GLFW.EventKey  _ GLFW.Key'Enter     _ GLFW.KeyState'Repeating _))) → Edit $ T.breakLine
  (Input (U (GLFW.EventKey  _ GLFW.Key'Backspace _ GLFW.KeyState'Repeating _))) → Edit $ T.deletePrevChar
  (Input (U (GLFW.EventKey  _ GLFW.Key'Delete    _ GLFW.KeyState'Repeating _))) → Edit $ T.deleteChar
  (Input (U (GLFW.EventKey  _ GLFW.Key'Left      _ GLFW.KeyState'Repeating _))) → Edit $ T.moveLeft
  (Input (U (GLFW.EventKey  _ GLFW.Key'Up        _ GLFW.KeyState'Repeating _))) → Edit $ T.moveUp
  (Input (U (GLFW.EventKey  _ GLFW.Key'Right     _ GLFW.KeyState'Repeating _))) → Edit $ T.moveRight
  (Input (U (GLFW.EventKey  _ GLFW.Key'Down      _ GLFW.KeyState'Repeating _))) → Edit $ T.moveDown
  (Input (U (GLFW.EventKey  _ GLFW.Key'Home      _ GLFW.KeyState'Repeating _))) → Edit $ T.gotoBOL
  (Input (U (GLFW.EventKey  _ GLFW.Key'End       _ GLFW.KeyState'Repeating _))) → Edit $ T.gotoEOL
  x → error $ "Unexpected event (non-edit): " <> show x



fpsCounterD ∷ ∀ t m. Event t Frame → ReflexGLFW t m (Dynamic t Double)
fpsCounterD frameE = do
  frameMomentE     ← performEvent $ fmap (\_ → HOS.fromSec <$> HOS.getTime) frameE
  frameΔD          ← (fst <$>) <$> foldDyn (\y (_,x)->(y-x,y)) (0,0) frameMomentE
  avgFrameΔD       ← average 20 $ updated frameΔD
  pure (recip <$> avgFrameΔD)

nextFrame ∷ ReflexGLFWCtx t m ⇒ GLFW.Window → Event t () → ReflexGLFW t m (Event t ())
nextFrame win windowFrameE = performEvent $ windowFrameE <&>
  \_ → liftIO $ do
    GLFW.swapBuffers win
    -- GL.flush  -- not necessary, but someone recommended it
    GLFW.pollEvents

trackStyle ∷ ∀ a t m. (Holo a, ReflexGLFWCtx t m) ⇒ Dynamic t (StyleOf a) → ReflexGLFW t m (Dynamic t (Style a))
trackStyle sof = do
  gene ← count $ updated sof
  pure $ zipDynWith Style sof (StyleGene ∘ fromIntegral <$> gene)

scene ∷ InputMux t → Dynamic t Integer → Dynamic t Int → Dynamic t Double → WidgetM t m HoloBlank
scene muxV statsValD frameNoD fpsValueD = mdo

  fpsD             ← mkTextD (constDyn defStyle) (T.pack ∘ printf "%3d fps" ∘ (floor ∷ Double → Integer) <$> fpsValueD)
  statsD           ← mkTextD (constDyn defStyle) $ statsValD <&>
                     \(mem)→ T.pack $ printf "mem: %d" mem

  let rectDiD       = (PUs <$>) ∘ join unsafe'di ∘ fromIntegral ∘ max 1 ∘ flip mod 200 <$> frameNoD
  rectD            ← mkColorRectD rectDiD (constDyn $ co 1 0 0 1)
  frameCountD      ← mkTextD (constDyn defStyle) (T.pack ∘ printf "frame #%04d" <$> frameNoD)
  -- varlenTextD      ← mkTextD portV (constDyn defStyle) (constDyn $ T.pack $ printf "even: %s" $ show True) --(T.pack ∘ printf "even: %s" ∘ show ∘ even <$> frameNoD)
  varlenTextD      ← mkTextD (constDyn defStyle)               (T.pack ∘ printf "even: %s" ∘ show ∘ even <$> frameNoD)

  longStaticTextD  ← mkTextD (constDyn defStyle) (constDyn "0....5...10...15...20...25...30...35...40...45...50...55...60...65...70...75...80...85...90...95..100")

  let fontNameStyle name = defStyleOf & tsFontKey .~ Cr.FK name

  styleEntryD
                   ← mkTextEntryValidatedD muxV styleB "defaultSans" $
                     (\x→ x ≡ "defaultMono" ∨ x ≡ "defaultSans")

  styleD           ← trackStyle $ fontNameStyle ∘ fst <$> (traceDynWith (show ∘ fst) (value styleEntryD))
  let styleB        = current styleD

  text2HoloQD      ← mkTextEntryD muxV styleB "watch me"

  vboxD [ frameCountD
        , (snd <$>) <$> text2HoloQD
        , (snd <$>) <$> styleEntryD
        , rectD
        , fpsD
        , longStaticTextD
        , statsD
        , varlenTextD ]



data Options where
  Options ∷
    { oTrace ∷ Bool
    } → Options
parseOptions ∷ Opt.Parser Options
parseOptions =
  Options
  <$> Opt.switch (Opt.long "trace" <> Opt.help "[DEBUG] Enable allocation tracing")

data AnObject where
  AnObject ∷
    { objName   ∷ String
    , objDPI    ∷ DΠ
    , objDim    ∷ Di Int
    } → AnObject
    deriving (Eq, GHC.Generic, Show)
instance SOP.Generic         AnObject
instance SOP.HasDatatypeInfo AnObject

data CName where
  CName ∷ Text → ADTChoiceT → CName

-- instance {-# OVERLAPPABLE #-} (SOP.Generic a, SOP.HasDatatypeInfo a) ⇒ Record AnObject where
-- instance {-# OVERLAPPABLE #-} (SOP.Generic a, SOP.HasDatatypeInfo a) ⇒ CtxRecord AnObject AnObject where
type instance ConsCtx a = CName
instance Ctx AnObject where
instance {-# OVERLAPPABLE #-} Record AnObject where
  prefixChars = const 3
instance {-# OVERLAPPABLE #-} CtxRecord AnObject AnObject where
  consCtx _ _ n ix = CName n ix
instance {-# OVERLAPPABLE #-}
  ( CtxRecord a a
  , Record      (Dynamic t Subscription, Dynamic t (a, HoloBlank)))
  ⇒ CtxRecord a (Dynamic t Subscription, Dynamic t (a, HoloBlank)) where
  consCtx _ _ n ix = CName n ix

-- class (SOP.Generic a, SOP.HasDatatypeInfo a, Ctx ctx, Record a) ⇒ CtxRecord ctx a where

-- * Top level network
--
holotype ∷ ∀ t m. ReflexGLFWGuest t m
holotype win evCtl windowFrameE inputE = mdo
  Options{..} ← liftIO $ Opt.execParser $ Opt.info (parseOptions <**> Opt.helper)
                ( Opt.fullDesc
                  -- <> header   "A simple holotype."
                  <> Opt.progDesc "A simple holotype.")
  when oTrace $
    liftIO $ setupTracer [
    (ALLOC,     TOK, TRACE, 0),(FREE,      TOK, TRACE, 0)
    ,(MISSALLOC, VIS, TRACE, 4),(REUSE,     VIS, TRACE, 4),(REALLOC,   VIS, TRACE, 4),(ALLOC,     VIS, TRACE, 4),(FREE,        VIS, TRACE, 4)
    ,(ALLOC,     TEX, TRACE, 8),(FREE,      TEX, TRACE, 8)
    ]

  -- What do we want?
  -- 1. input:
  --    - initial value
  -- 2. output:
  --    - dynamic
  --    - (,) value HoloBlank
  x ∷ Widget t (AnObject, HoloBlank) ← recover (undefined ∷ AnObject)

  HOS.unbufferStdout

  initE            ← getPostBuild

  winD             ← holdDyn win $ win <$ initE
  (Di (V2 initW initH))
                   ← portWindowSize win
  let fbSizeE       = ffilter (\case (U GLFW.EventFramebufferSize{}) → True; _ → False) $
                      leftmost [inputE, (U (GLFW.EventFramebufferSize win initW initH)) <$ initE]
  liftIO $ GLFW.enableEvent evCtl GLFW.FramebufferSize

  settingsD        ← foldDyn (\(U (GLFW.EventFramebufferSize _ w h)) oldStts →
                                 oldStts { sttsScreenDim = unsafe'di w h } )
                     defaultSettings fbSizeE

  maybePortD       ← portCreate winD settingsD
  portFrameE       ← newPortFrame $ fmapMaybe id $ fst <$> attachPromptlyDyn maybePortD windowFrameE

  -- * EXTERNAL STIMULI

  fpsValueD        ← fpsCounterD  $ snd <$> portFrameE
  frameNoD ∷ Dynamic t Int
                   ← count       portFrameE
  statsValE        ← performEvent $ portFrameE <&> const HOS.gcKBytesUsed
  statsValD        ← holdDyn 0 statsValE

  -- * SCENE
  inputMux         ← routeInput (Input <$> inputE) pickedE subscriptionsD
  (,) subscriptionsD sceneD
                   ← scene inputMux statsValD frameNoD fpsValueD

  -- * LAYOUT
  -- needs port because of DPI and fonts
  sceneQueriedE    ← performEvent $ (\(s, (p, _f))→ Holo.queryHolotree p s) <$>
                     attachPromptlyDyn sceneD portFrameE

  sceneQueriedD    ← holdDyn mempty sceneQueriedE

  let sceneLaidTreeD ∷ Dynamic t (Item Holo.PLayout)
      sceneLaidTreeD = layout (Size $ fromPU <$> di 800 600) <$> sceneQueriedD

  -- * RENDER
      sceneDrawE     = attachPromptlyDyn sceneLaidTreeD portFrameE
  drawnPortE       ← performEvent $ sceneDrawE <&>
                     \(tree, (,) port f@Frame{..}) → do
                       let leaves = Holo.holotreeLeaves tree
                       -- liftIO $ printf "   leaves: %d\n" $ M.size leaves
                       portGarbageCollectVisuals port leaves
                       tree' ← Holo.visualiseHolotree port tree
                       Holo.renderHolotreeVisuals port tree'
                       Holo.drawHolotreeVisuals f tree'
                       pure port
  drawnPortD       ← holdDyn Nothing $ Just <$> drawnPortE

  -- * PICKING
  let clickE        = ffilter (\case (U GLFW.EventMouseButton{}) → True; _ → False) inputE
      pickE         = fmapMaybe id $ attachPromptlyDyn drawnPortD clickE <&> \case
                        (Nothing, _) → Nothing
                        (Just x, y)  → Just (x, y)
  pickedE          ← mousePointId $ (id *** (\(U x@GLFW.EventMouseButton{})→ x)) <$> pickE
  performEvent_ $ pickedE <&>
    \token→ liftIO $ printf "%x\n" (tokenHash token)

  -- * Limit frame rate to vsync.  XXX:  also, flicker.
  worldE ∷ Event t WorldEvent
                   ← performEvent $ inputE <&> translateEvent
  waitForVSyncD    ← toggle True $ ffilter (\case VSyncToggle → True; _ → False) worldE
  performEvent_ $ portSetVSync <$> updated waitForVSyncD

  hold False ((\case Shutdown → True; _ → False)
               <$> worldE)

mousePointId ∷ ∀ t m. ReflexGLFWCtx t m ⇒ Event t (Port, GLFW.Input 'GLFW.MouseButton) → ReflexGLFW t m (Event t IdToken)
mousePointId ev = (ffilter ((≢ 0) ∘ tokenHash) <$>) <$>
                  performEvent $ ev <&> \(port@Port{..}, GLFW.EventMouseButton _ _ _ _) → do
                    (,) x y ← liftIO $ (GLFW.getCursorPos portWindow)
                    portPick port $ floor <$> po x y



data WorldEvent where
  Move ∷
    { weΔ ∷ Po Double
    } → WorldEvent
  Click ∷
    { weMButton ∷ GLFW.MouseButton
    , weCoord   ∷ Po Double
    } → WorldEvent
  ObjStream   ∷ WorldEvent
  VSyncToggle ∷ WorldEvent
  GCing       ∷ WorldEvent
  Spawn       ∷ WorldEvent
  Shutdown    ∷ WorldEvent
  NonEvent    ∷ WorldEvent

translateEvent ∷ (MonadIO m) ⇒ InputU → m WorldEvent
translateEvent (U (GLFW.EventMouseButton w button GLFW.MouseButtonState'Pressed _)) = do
  (,) x y ← liftIO $ GLFW.getCursorPos w
  pure $ Click button (po x y)
-- how to process key chords?
translateEvent (U (GLFW.EventKey  _ GLFW.Key'F1        _ GLFW.KeyState'Pressed   _)) = pure $ ObjStream
translateEvent (U (GLFW.EventKey  _ GLFW.Key'F2        _ GLFW.KeyState'Pressed   _)) = pure $ GCing
translateEvent (U (GLFW.EventKey  _ GLFW.Key'F3        _ GLFW.KeyState'Pressed   _)) = pure $ VSyncToggle
translateEvent (U (GLFW.EventKey  _ GLFW.Key'Insert    _ GLFW.KeyState'Pressed   _)) = pure $ Spawn
translateEvent (U (GLFW.EventKey  _ GLFW.Key'Escape    _ GLFW.KeyState'Pressed   _)) = pure $ Shutdown
translateEvent _                                                                     = pure $ NonEvent

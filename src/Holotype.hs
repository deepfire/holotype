{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
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
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors -Wno-unused-imports -Wno-type-defaults -Wno-orphans -fconstraint-solver-iterations=0 #-}
module Holotype where

import           Control.Arrow
import           Control.Compose
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.Primitive
import           Control.Monad.Ref
import           Data.Foldable
import           Data.Functor.Misc                        (Const2(..))
import           Data.Maybe
import           Data.Semigroup
import           Data.Singletons
import           Data.Text                                (Text)
import           Data.Text.Zipper                         (TextZipper)
import           Data.Tuple
import           Data.Typeable
import           Generics.SOP.Monadic
-- import           GHC.IOR
import           Linear                            hiding (trace)
import           Prelude                           hiding (id, Word)
import           Reflex                            hiding (Query, Query(..))
import           Reflex.Host.Class                        (ReflexHost, MonadReflexHost)
import           Reflex.GLFW                              (RGLFW, RGLFWGuest, InputU(..))
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
import qualified Flex

import           HoloPrelude                       hiding ((<>))
import           Holo.Instances
import           Holo                                     ( Vis
                                                          , Holo, HoloBlank, InputEvent, InputEventMux, Item, Style(..), StyleOf, StyleGene(..), Subscription(..), VPort
                                                          , Static(..)
                                                          , Widget, liftW, liftWDynamic
                                                          , WH, wWH
                                                          , Result(..))
import qualified Holo
import qualified HoloCairo                         as Cr
import qualified HoloPort                          as Port
import           HoloPort                             (Port(..), IdToken)
import qualified HoloOS                            as HOS

-- TEMPORARY
import           Generics.SOP                             (Proxy)
import qualified Generics.SOP                      as SOP
import qualified "GLFW-b" Graphics.UI.GLFW         as GLFW


newPortFrame ∷ RGLFW t m ⇒ Event t VPort → m (Event t (VPort, Port.Frame))
newPortFrame portFrameE = performEvent $ portFrameE <&>
  \port@Port{..}→ do
    newFrame ← Port.portNextFrame port
    pure (port, newFrame)

type Avg a = (Int, Int, [a])
avgStep ∷ Fractional a ⇒ a → (a, Avg a) → (a, Avg a)
avgStep x (_, (lim, cur, xs)) =
  let (ncur, nxs) = if cur < lim
                    then (cur + 1, x:xs)
                    else (lim,     x:Prelude.init xs)
  in ((sum nxs) / fromIntegral ncur, (lim, ncur, nxs))

average ∷ (Fractional a, RGLFW t m) ⇒ Int → Event t a → m (Dynamic t a)
average n e = (fst <$>) <$> foldDyn avgStep (0, (n, 0, [])) e



routeInputEvent ∷ ∀ t m. (RGLFW t m)
           ⇒ Event t InputEvent          -- ^ Events to distribute
           → Event t IdToken             -- ^ Carries the (possibly) new picked entity
           → Dynamic t Subscription      -- ^ The total mass of subscriptions
           → m (InputEventMux t)         -- ^ Global event wire for all IdTokens
routeInputEvent inputE pickedE subsD = do
  pickeD ← holdDyn Nothing $ Just <$> pickedE -- Compute the latest focus
  let inputsD = zipDynWith (,) pickeD (traceDyn "===== new subs: " subsD)
      -- | Process the incoming events using the latest listener and total set subscriptions
      routedE ∷ Event t (M.Map IdToken InputEvent)
      routedE = routeSingle <$> attachPromptlyDyn inputsD inputE
      routeSingle ∷ ((Maybe IdToken, Subscription), InputEvent) → M.Map IdToken InputEvent
      routeSingle ((picked, Subscription ss), ev) =
        case MMap.lookup (GLFW.eventUType $ Holo.inInputEvent ev) ss of
          Nothing         → --trace ("rejected type: "<>show ev<>"/"<>show (inInputEvent ev))
                            mempty -- no-one cares, nothing happened..
          Just potentials →
            let matches = flip Seq.filter potentials (flip Holo.inputMatch ev ∘ snd)
            in case (picked, toList matches) of
                 (_, [])               → --trace ("rejected unmatched: "<>show ev)
                                         mempty
                 (Just pick, matched)  → case lookup pick matched of
                   Nothing → mempty -- XXX: mis-focus -- we allowed to focus a non-interested entity
                   Just _  → M.singleton pick ev
                 (Nothing, (tok, _):_) → M.singleton tok ev
  pure $ fanMap routedE


-- mkTextEntryStyleD ∷ RGLFW t m ⇒ InputEventMux t → Behavior t (Style Text) → Text → m (W t (Text, HoloBlank))
-- mkTextEntryStyleD mux styleB initialV = do
--   tokenV       ← newId
--   let editE = select mux $ Const2 tokenV
--   valD         ← liftDyn initialV editE
--   setupE       ← getPostBuild
--   let holoE     = attachWith (Holo.leafStyled tokenV) styleB $ leftmost [updated valD, initialV <$ setupE]
--   holdDyn (initialV, Holo.emptyHolo) (attachPromptlyDyn valD holoE)
--    <&> (,) editMaskKeys

mkTextEntryValidatedStyleD ∷ RGLFW t m ⇒ InputEventMux t → Behavior t (Style Text) → Text → (Text → Bool) → m (Result t Text)
mkTextEntryValidatedStyleD mux styleB initialV testF = do
  unless (testF initialV) $
    error $ "Initial value not accepted by test: " <> T.unpack initialV
  -- (subD, textD) ← mkTextEntryStyleD mux styleB initialV
  W (subD, textD) ← Holo.liftW mux initialV
  initial ← sample $ current textD
  foldDyn (\(new, newHoloi) (oldValid, _)→
               (if testF new then new else oldValid, newHoloi))
    initial (updated textD)
    <&> W ∘ (subD,)

vboxD ∷ ∀ t m. (RGLFW t m) ⇒ [WH t] → m (WH t)
vboxD chi = do
  let dyn ∷ (Dynamic t Subscription, Dynamic t [HoloBlank])
      dyn = foldr (\(s, hb) (ss, hbs)→
                      ( zipDynWith (<>) s ss
                      , zipDynWith (:) hb hbs ))
            (constDyn mempty, constDyn [])
            chi
  pure $ (id *** (Holo.vbox <$>)) dyn



fpsCounterD ∷ RGLFW t m ⇒ Event t Port.Frame → m (Dynamic t Double)
fpsCounterD frameE = do
  frameMomentE     ← performEvent $ fmap (\_ → HOS.fromSec <$> HOS.getTime) frameE
  frameΔD          ← (fst <$>) <$> foldDyn (\y (_,x)->(y-x,y)) (0,0) frameMomentE
  avgFrameΔD       ← average 20 $ updated frameΔD
  pure (recip <$> avgFrameΔD)

nextFrame ∷ RGLFW t m ⇒ GLFW.Window → Event t () → m (Event t ())
nextFrame win windowFrameE = performEvent $ windowFrameE <&>
  \_ → liftIO $ do
    GLFW.swapBuffers win
    -- GL.flush  -- not necessary, but someone recommended it
    GLFW.pollEvents

trackStyle ∷ (Holo a, RGLFW t m) ⇒ Dynamic t (StyleOf a) → m (Dynamic t (Style a))
trackStyle sof = do
  gene ← count $ updated sof
  pure $ zipDynWith Style sof (StyleGene ∘ fromIntegral <$> gene)



data MegaSub where
  MegaSub ∷
    { suSub1    ∷ Text
    , suSub2    ∷ Text
    } → MegaSub
    deriving (Eq, GHC.Generic, Show)
instance SOP.Generic         MegaSub
instance SOP.HasDatatypeInfo MegaSub

data AnObject where
  AnObject ∷
    { objName   ∷ Text
    , objLol    ∷ Text
    , objYay    ∷ Bool
    -- , objDPI    ∷ DΠ
    -- , objDim    ∷ Di Int
    } → AnObject
    deriving (Eq, GHC.Generic, Show)
instance SOP.Generic         AnObject
instance SOP.HasDatatypeInfo AnObject

scene ∷ ∀ t m. ( RGLFW t m
               , Typeable t)
  ⇒ Port.Settings
  → InputEventMux   t
  → Dynamic    t Integer
  → Dynamic    t Int
  → Dynamic    t Double
  → m (WH t)
scene defSettingsV eV statsValD frameNoD fpsValueD = mdo

  fpsD             ← liftWDynamic (T.pack ∘ printf "%3d fps" ∘ (floor ∷ Double → Integer) <$> fpsValueD)
  statsD           ← liftWDynamic $ statsValD <&>
                     \(mem)→ T.pack $ printf "mem: %d" mem

  let rectDiD       = (PUs <$>) ∘ join unsafe'di ∘ fromIntegral ∘ max 1 ∘ flip mod 200 <$> frameNoD
  rectD            ← liftWDynamic $ zipDynWith Rect rectDiD (constDyn $ co 1 0 0 1)
  frameCountD      ← liftWDynamic $ T.pack ∘ printf "frame #%04d" <$> frameNoD
  -- varlenTextD      ← mkTextD portV (constDyn defStyle) (constDyn $ T.pack $ printf "even: %s" $ show True) --(T.pack ∘ printf "even: %s" ∘ show ∘ even <$> frameNoD)
  varlenTextD      ← liftWDynamic $ T.pack ∘ printf "even: %s" ∘ show ∘ even <$> frameNoD

  -- xStts            ← liftRecord muxV defSettingsV
  xDD@(W (_, xDDv)) ← liftWRecord @(Static t AnObject) (eV, AnObject "yayyity" "lol" True)
  _                ← performEvent $ (updated xDDv) <&>
                     \(x, _) → liftIO $ putStrLn (show x)
  xSD@(W (_, xSDv)) ← liftWRecord @(Static t Port.Settings) (eV, defSettingsV)

  longStaticTextD  ← liftW eV ("0....5...10...15...20...25...30...35...40...45...50...55...60...65...70...75...80...85...90...95..100" ∷ Text)

  let fontNameStyle name = Holo.defStyleOf (Proxy @Text) & tsFontKey .~ Cr.FK name

  W styleEntryD ← mkTextEntryValidatedStyleD eV styleB "defaultSans" $
                     (\x→ x ≡ "defaultMono" ∨ x ≡ "defaultSans")

  styleD           ← trackStyle $ fontNameStyle ∘ fst <$> (traceDynWith (show ∘ fst) (snd styleEntryD))
  let styleB        = current styleD

  -- text2HoloQD      ← mkTextEntryStyleD muxV styleB "watch me"

  vboxD [ wWH $ frameCountD
        -- , (snd <$>) <$> text2HoloQD
        , (snd <$>) <$> styleEntryD
        -- , wWH xD
        , wWH xDD
        , wWH $ rectD
        , wWH $ fpsD
        , wWH $ longStaticTextD
        , wWH $ statsD
        , wWH $ varlenTextD ]



data Options where
  Options ∷
    { oTrace ∷ Bool
    } → Options
parseOptions ∷ Opt.Parser Options
parseOptions =
  Options
  <$> Opt.switch (Opt.long "trace" <> Opt.help "[DEBUG] Enable allocation tracing")

-- * Top level network
--
holotype ∷ ∀ t m. (Typeable t) ⇒ RGLFWGuest t m
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

  HOS.unbufferStdout

  initE            ← getPostBuild

  winD             ← holdDyn win $ win <$ initE
  liftIO $ GLFW.enableEvent evCtl GLFW.FramebufferSize

  (Di (V2 initW initH))
                   ← Port.portWindowSize win
  let fbSizeE       = ffilter (\case (U GLFW.EventFramebufferSize{}) → True; _ → False) $
                      leftmost [inputE, (U (GLFW.EventFramebufferSize win initW initH)) <$ initE]
  settingsD        ← foldDyn (\(U (GLFW.EventFramebufferSize _ w h)) oldStts →
                                 oldStts { Port.sttsScreenDim = Port.ScreenDim $ unsafe'di w h } )
                     Port.defaultSettings fbSizeE

  maybePortD       ← Port.portCreate winD settingsD
  portFrameE       ← newPortFrame $ fmapMaybe id $ fst <$> attachPromptlyDyn maybePortD windowFrameE

  -- * EXTERNAL STIMULI

  fpsValueD        ← fpsCounterD  $ snd <$> portFrameE
  frameNoD ∷ Dynamic t Int
                   ← count       portFrameE
  statsValE        ← performEvent $ portFrameE <&> const HOS.gcKBytesUsed
  statsValD        ← holdDyn 0 statsValE

  -- * SCENE
  -- not a loop:  subscriptionsD only used/sampled during inputE, which is independent
  inputMux         ← routeInputEvent (Holo.InputEvent <$> inputE) pickedE subscriptionsD
  (,) subscriptionsD sceneD
                   ← scene Port.defaultSettings inputMux statsValD frameNoD fpsValueD

  -- * LAYOUT
  -- needs port because of DPI and fonts
  sceneQueriedE    ← performEvent $ (\(s, (p, _f))→ Holo.hiSizeRequest p s) <$>
                     attachPromptlyDyn sceneD portFrameE
  sceneQueriedD    ← holdDyn mempty sceneQueriedE

  let sceneLaidTreeD ∷ Dynamic t (Item Holo Holo.PLayout)
      sceneLaidTreeD = Flex.layout (Size $ fromPU <$> di 800 600) <$> sceneQueriedD

  -- * RENDER
      sceneDrawE     = attachPromptlyDyn sceneLaidTreeD portFrameE
  drawnPortE       ← performEvent $ sceneDrawE <&>
                     \(tree, (,) port f@Port.Frame{..}) → do
                       -- Flex.dump (\x→ "La: "<>Flex.ppItemArea x<>" ← "<>Flex.ppItemSize x) tree
                       let leaves = Holo.hiLeaves tree
                       -- liftIO $ printf "   leaves: %d\n" $ M.size leaves
                       Port.portGarbageCollectVisuals port leaves
                       tree' ← Holo.ensureHolotreeVisuals port tree
                       -- XXX: 'render' is called every frame for everything
                       Holo.renderHolotreeVisuals port tree'
                       Holo.showHolotreeVisuals f tree'
                       pure port
  drawnPortD       ← holdDyn Nothing $ Just <$> drawnPortE

  -- * PICKING
  let clickE        = ffilter (\case (U GLFW.EventMouseButton{}) → True; _ → False) inputE
      pickE         = fmapMaybe id $ attachPromptlyDyn drawnPortD clickE <&> \case
                        (Nothing, _) → Nothing
                        (Just x, y)  → Just (x, y)
  pickedE          ← mousePointId $ (id *** (\(U x@GLFW.EventMouseButton{})→ x)) <$> pickE
  performEvent_ $ pickedE <&>
    \token→ liftIO $ printf "%x\n" (Port.tokenHash token)

  -- * Limit frame rate to vsync.  XXX:  also, flicker.
  worldE ∷ Event t WorldEvent
                   ← performEvent $ inputE <&> translateEvent
  waitForVSyncD    ← toggle True $ ffilter (\case VSyncToggle → True; _ → False) worldE
  performEvent_ $ Port.portSetVSync <$> updated waitForVSyncD

  hold False ((\case Shutdown → True; _ → False)
               <$> worldE)

mousePointId ∷ RGLFW t m ⇒ Event t (VPort, GLFW.Input 'GLFW.MouseButton) → m (Event t IdToken)
mousePointId ev = (ffilter ((≢ 0) ∘ Port.tokenHash) <$>) <$>
                  performEvent $ ev <&> \(port@Port{..}, GLFW.EventMouseButton _ _ _ _) → do
                    (,) x y ← liftIO $ (GLFW.getCursorPos portWindow)
                    Port.portPick port $ floor <$> po x y



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

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
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
import           Holo.Record
import           Holo                                     ( As(..), Vocab, namely
                                                          , Vis
                                                          , Holo, BlankHolo, Blank, InputEvent, InputEventMux, Item, Style(..), Sty, StyleGene(..), Subscription(..), VPort
                                                          , HGLFW, API, APIt, APIm
                                                          , Widget, liftW, liftPureDynamic, liftDynamic
                                                          , WH, stripW
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
           → Event t InputEvent          -- ^ Carries the (possibly) new picked entity
           → Dynamic t Subscription      -- ^ The total mass of subscriptions
           → m (InputEventMux t)         -- ^ Global event wire for all IdTokens
routeInputEvent inputE clickedE subsD = do
  pickeD ← holdDyn Nothing $ Just ∘ Holo.inIdToken <$> clickedE -- Compute the latest focus (or just a mouse click)
  -- XXX: filter the above on the left click -- as it stands any mouse button changes selection
  let fullInputE = leftmost [clickedE, inputE]
      inputsD = zipDynWith (,) pickeD (traceDyn "===== new subs: " subsD)
      -- | Process the incoming events using the latest listener and total set subscriptions
      routedE ∷ Event t (M.Map IdToken InputEvent)
      routedE = routeSingle <$> attachPromptlyDyn inputsD fullInputE
      routeSingle ∷ ((Maybe IdToken, Subscription), InputEvent) → M.Map IdToken InputEvent
      routeSingle ((mClickOrPicked, Subscription ss), ev) =
        let eventType = Holo.inputEventType ev
        in case MMap.lookup eventType ss of
           Nothing         → mempty -- no-one cares about this type of events
           Just eventTypeSubscribers  →
             let eventListenerSet ∷ Seq.Seq (IdToken, Holo.InputEventMask) =
                   flip Seq.filter eventTypeSubscribers (flip Holo.inputMatch ev ∘ snd)
             in case (eventType, trace ("routing: "<>show mClickOrPicked) mClickOrPicked, toList eventListenerSet) of
                  (_, _, []) → mempty -- no-one's event mask matches this event
                  -- --------------- here we need the click, not the accumulated pick
                  -- (GLFW.MouseButton, Just clicked, eventListeners) → case lookup clicked eventListeners of
                  --   Nothing → mempty -- focused ID is not among subscribers
                  --   Just x  → if x ≡ clicked
                  --             then M.singleton clicked ev
                  --             else mempty
                  (_, Just pick, eventListeners)  → case lookup pick eventListeners of
                    Nothing → mempty -- focused ID is not among subscribers
                    Just _  → M.singleton (trace ("routed to: "<>show pick) pick) ev
                  -- (_, Nothing, (tok, _):_) → M.singleton tok ev
                  (_, Nothing, _) → mempty
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

mkTextEntryValidatedStyleD ∷ ∀ i t m. HGLFW i t m ⇒ InputEventMux t → Behavior t (Style TextLine) → Text → (Text → Bool) → m (Result i Text)
mkTextEntryValidatedStyleD mux styleB initialV testF = do
  unless (testF initialV) $
    error $ "Initial value not accepted by test: " <> T.unpack initialV
  -- (subD, textD) ← mkTextEntryStyleD mux styleB initialV
  W (subD, itemD, textD) ← Holo.liftW @i @Text mux (namely @Text TextLine) initialV
  initial ← sample $ current textD
  foldDyn (\new oldValid→
               if testF new then new else oldValid)
    initial (updated textD)
    <&> W ∘ (subD,itemD,)

vboxD ∷ ∀ i t m. (HGLFW i t m) ⇒ [WH i] → m (WH i)
vboxD chi = do
  let dyn ∷ (Dynamic t Subscription, Dynamic t [Blank i])
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

trackStyle ∷ (As a, RGLFW t m) ⇒ Dynamic t (Sty a) → m (Dynamic t (Style a))
trackStyle sof = do
  gene ← count $ updated sof
  pure $ zipDynWith Style sof (StyleGene ∘ fromIntegral <$> gene)



data Settings where
  Settings ∷
    { sttsDΠ              ∷ DΠ
    -- , sttsFontPreferences ∷ Cr.FontPreferences PU
    , sttsScreenMode      ∷ Port.ScreenMode
    , sttsScreenDim       ∷ Port.ScreenDim (Di Int)
    } → Settings
    deriving (Eq, GHC.Generic, Show)
instance SOP.Generic         Settings
instance SOP.HasDatatypeInfo Settings
type instance Structure Settings = Settings
defSettings ∷ Settings
defSettings =
  let sttsDΠ ∷ DΠ         = 96
      sttsFontPreferences = Cr.FontPreferences
        [ ("default",     Left $ Cr.Alias "defaultMono" )
        , ("defaultSans", Right $ [ Cr.FontSpec "Bitstream Charter" "Regular" $ Cr.Outline (PUs 16)
                                  , Cr.FontSpec "Aurulent Sans"     "Regular" $ Cr.Outline (PUs 16) ])
        , ("defaultMono", Right $ [ Cr.FontSpec "Terminus"          "Regular" $ Cr.Bitmap  (PUs 15) LT ])
        ]
      sttsScreenMode      = Port.Windowed
      sttsScreenDim       = Port.ScreenDim $ di 800 600
  in Settings{..}
instance Holo i (Port.ScreenMode)
instance Typeable a ⇒ Holo i (Port.ScreenDim a)

data AnObject where
  AnObject ∷
    { objName   ∷ Text
    , objLol    ∷ Double
    , objYay    ∷ Bool
    -- , objDPI    ∷ DΠ
    -- , objDim    ∷ Di Int
    } → AnObject
    deriving (Eq, GHC.Generic, Show)
instance SOP.Generic         AnObject
instance SOP.HasDatatypeInfo AnObject
type instance Structure AnObject = AnObject

instance SOP.Generic         (Cr.FontPreferences PU)
instance SOP.HasDatatypeInfo (Cr.FontPreferences PU)
type instance Structure (Cr.FontPreferences 'PU) = (Cr.FontPreferences PU)

instance {-# OVERLAPPABLE #-} Holo.Named a a
instance {-# OVERLAPPABLE #-} Holo.Named Text a
instance {-# OVERLAPS #-}     Holo.Named Text Text

instance Holo.Named Text Double
instance Holo.Named (Text,Text) (Double,Double)

(<:) ∷ Typeable b ⇒ TM.TypeMap a → (Proxy b, TM.Item a b) → TM.TypeMap a
(<:) tm (k, v) = TM.insert k v tm

defVocab ∷ Vocab i (Holo i)
defVocab = Vocab
  (TM.empty
    <: (Proxy @Bool,             HoloName TextLine)
    <: (Proxy @Bool,             HoloName Switch)
    <: (Proxy @Double,           HoloName TextLine)
    <: (Proxy @DΠ,               HoloName TextLine)
    <: (Proxy @Port.ScreenMode,  HoloName TextLine)
    <: (Proxy @Float,            HoloName TextLine)
    <: (Proxy @Int,              HoloName TextLine)
    <: (Proxy @Integer,          HoloName TextLine)
    <: (Proxy @Text,             HoloName TextLine)
    -- <: (Proxy @(Port.ScreenDim (Di Int)),
    --                              HoloName TextLine)
  )
deriving newtype instance Read DΠ
deriving         instance Read Port.ScreenMode

scene ∷ ∀ i t m. ( HGLFW i t m
                 , Typeable t)
  ⇒ Proxy i
  → Port.Settings
  → InputEventMux   t
  → Dynamic    t Integer
  → Dynamic    t Int
  → Dynamic    t Double
  → m (WH i)
scene pI defSettingsV eV statsValD frameNoD fpsValueD = mdo

  -- XXX: we have a conceptual conflict:
  --      1. Holo implies editability
  --      2. liftWDynamic takes an already formed dynamic and turns into a visual
  --      3. ..yet it requires Holo, which insists on Mutable, which we don't care about..
  fpsD ∷ Widget i Text
                   ← liftPureDynamic TextLine (T.pack ∘ printf "%3d fps" ∘ (floor ∷ Double → Integer) <$> fpsValueD)
  statsD ∷ Widget i Text
                   ← liftPureDynamic TextLine $ statsValD <&>
                     \(mem)→ T.pack $ printf "mem: %d" mem
  lolD ∷ Widget i Text
                   ← liftPureDynamic (Labelled ("mem", TextLine)) $ statsValD <&>
                     \(mem)→ T.pack $ printf "%d" mem

  let rectDiD       = (PUs <$>) ∘ join unsafe'di ∘ fromIntegral ∘ max 1 ∘ flip mod 200 <$> frameNoD
  rectD ∷ Widget i (Di (Unit PU))
                   ← liftPureDynamic Rect rectDiD

  frameCountD ∷ Widget i Text
                   ← liftPureDynamic TextLine $ T.pack ∘ printf "frame #%04d" <$> frameNoD
  -- varlenTextD      ← mkTextD portV (constDyn defStyle) (constDyn $ T.pack $ printf "even: %s" $ show True) --(T.pack ∘ printf "even: %s" ∘ show ∘ even <$> frameNoD)
  varlenTextD ∷ Widget i Text
                   ← liftPureDynamic TextLine $ T.pack ∘ printf "even: %s" ∘ show ∘ even <$> frameNoD

  doubleD ∷ Widget i Double
                   ← liftW eV (namely @Double TextLine) 0

  -- dimD ∷ Widget i (Double, Double)
  --                  ← liftW eV (X, (Labelled ("x", TextLine)
  --                                 ,Labelled ("y", TextLine)))
  --                    (0,0)

  recWD@(W(_,_,xDDv)) ← liftW @i eV defVocab
                      (AnObject "yayyity" 3.14 True)
  _                ← performEvent $ (updated xDDv) <&>
                     \x → liftIO $ putStrLn (show x)
  tupleWD          ← liftW @i @(Int, Text) eV defVocab
                      (42, "seriously?")
  -- sttsWD           ← liftW @i @Settings eV defVocab
  --                     defSettings
  -- • Couldn't match type ‘'[ '[(Cr.FontKey,  Either Cr.FontAlias [Cr.Font 'Cr.Spec 'PU]),
  --                             [(Cr.FontKey, Either Cr.FontAlias [Cr.Font 'Cr.Spec 'PU])]
  --                            ]
  --                         ]’
  --                  with ‘'[]’
  --     arising from a use of ‘liftW’

  longStaticTextD  ← liftW @i eV (namely @Text TextLine) ("0....5...10...15...20...25...30...35...40...45...50...55...60...65...70...75...80...85...90...95..100" ∷ Text)

  -- The loop demo (currently incapacitated due to definition of mkTextEntryValidatedStyleD)
  let fontNameStyle name = Holo.defSty (Proxy @TextLine) & tsFontKey .~ Cr.FK name
  styleNameD       ← mkTextEntryValidatedStyleD @i eV styleB "defaultSans" $
                     (\x→ x ≡ "defaultMono" ∨ x ≡ "defaultSans")
  styleD           ← trackStyle $ fontNameStyle <$> (traceDynWith show $ Holo.wValD styleNameD)
  let styleB        = current styleD

  --
  vboxD @i [ stripW $ frameCountD
        -- , (snd <$>) <$> text2HoloQD
        , stripW styleNameD
        -- , stripW xD
        , stripW lolD
        , stripW doubleD
        , stripW recWD
        -- , stripW sttsWD
        , stripW tupleWD
        , stripW rectD
        , stripW fpsD
        , stripW longStaticTextD
        , stripW statsD
        , stripW varlenTextD ]



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
    liftIO $ setupTracer False
    [(ALLOC,     TOK,  TRACE, 0),(FREE,      TOK, TRACE, 0)
    ,(SIZE,      HOLO, TRACE, 0)
    -- (ALLOC,     TOK, TRACE, 0),(FREE,      TOK, TRACE, 0)
    -- ,(MISSALLOC, VIS, TRACE, 4),(REUSE,     VIS, TRACE, 4),(REALLOC,   VIS, TRACE, 4),(ALLOC,     VIS, TRACE, 4),(FREE,        VIS, TRACE, 4)
    -- ,(ALLOC,     TEX, TRACE, 8),(FREE,      TEX, TRACE, 8)
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
  inputMux         ← routeInputEvent (Holo.InputEvent <$> ffilter (\case (U GLFW.EventMouseButton{}) → False; _ → True) inputE) clickedE subscriptionsD
  (,) subscriptionsD sceneD
                   ← scene (Proxy @(API t m)) Port.defaultSettings inputMux statsValD frameNoD fpsValueD

  -- * LAYOUT
  -- needs port because of DPI and fonts
  sceneQueriedE    ← performEvent $ (\(s, (p, _f))→ Holo.iSizeRequest p s) <$>
                     attachPromptlyDyn sceneD portFrameE
  sceneQueriedD    ← holdDyn mempty sceneQueriedE

  let sceneLaidTreeD ∷ Dynamic t (Item Holo.Unconstr Holo.PLayout)
      sceneLaidTreeD = Flex.layout (Size $ fromPU <$> di 800 600) <$> sceneQueriedD

  -- * RENDER
      sceneDrawE     = attachPromptlyDyn sceneLaidTreeD portFrameE
  drawnPortE       ← performEvent $ sceneDrawE <&>
                     \(tree, (,) port f@Port.Frame{..}) → do
                       -- let ppItem = \case
                       --       x@Holo.Node{..} → "N: "<>Flex.ppItemArea x<>" ← "<>Flex.ppItemSize x<>" geoΔ: "<>Flex.ppdefGeoDiff (Holo.iGeo x)
                       --       x@Holo.Leaf{..} → "L: "<>Flex.ppItemArea x<>" ← "<>Flex.ppItemSize x<>" geoΔ: "<>Flex.ppdefGeoDiff (Holo.iGeo x)
                       -- Flex.dump ppItem tree
                       let leaves = Holo.treeLeaves tree
                       -- liftIO $ printf "   leaves: %d\n" $ M.size leaves
                       Port.portGarbageCollectVisuals port leaves
                       tree' ← Holo.ensureTreeVisuals port tree
                       -- XXX: 'render' is called every frame for everything
                       Holo.renderTreeVisuals port tree'
                       Holo.showTreeVisuals f tree'
                       pure port
  drawnPortD       ← holdDyn Nothing $ Just <$> drawnPortE

  -- * PICKING
  let clickE        = ffilter (\case (U GLFW.EventMouseButton{}) → True; _ → False) inputE
      pickE         = fmapMaybe id $ attachPromptlyDyn drawnPortD clickE <&> \case
                        (Nothing, _) → Nothing -- We may have no drawn picture yet.
                        (Just x, y)  → Just (x, y)
  clickedE          ← mousePointId $ (id *** (\(U x@GLFW.EventMouseButton{})→ x)) <$> pickE
  performEvent_ $ clickedE <&>
    \Holo.ClickEvent{..}→ liftIO $ printf "pick=0x%x\n" (Port.tokenHash inIdToken)

  -- * Limit frame rate to vsync.  XXX:  also, flicker.
  worldE ∷ Event t WorldEvent
                   ← performEvent $ inputE <&> translateEvent
  waitForVSyncD    ← toggle True $ ffilter (\case VSyncToggle → True; _ → False) worldE
  performEvent_ $ Port.portSetVSync <$> updated waitForVSyncD

  hold False ((\case Shutdown → True; _ → False)
               <$> worldE)

mousePointId ∷ RGLFW t m ⇒ Event t (VPort, GLFW.Input 'GLFW.MouseButton) → m (Event t InputEvent)
mousePointId ev = (ffilter ((≢ 0) ∘ Port.tokenHash ∘ Holo.inIdToken) <$>) <$>
                  performEvent $ ev <&> \(port@Port{..}, e@(GLFW.EventMouseButton _ _ _ _)) → do
                    (,) x y ← liftIO $ (GLFW.getCursorPos portWindow)
                    Holo.ClickEvent e <$> (Port.portPick port $ floor <$> po x y)



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

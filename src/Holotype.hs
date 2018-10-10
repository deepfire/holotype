{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
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
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}
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
import           Data.Text.Zipper                         (TextZipper)
import           Data.Tuple
import           Data.Typeable
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



liftDynHolo ∷ ∀ t m a. (Holo a, ReflexGLFWCtx t m) ⇒ Dynamic t a → MWidget t m a
liftDynHolo h = do
  tok ← newId
  pure ( constDyn $ subscription (Proxy ∷ Proxy a) tok
       , h <&> \x→ (,) x $ Holo.leafStyled tok (initStyle $ compStyle x) x)

liftHoloStyled ∷ ∀ t m a. (Holo a, ReflexGLFWCtx t m) ⇒ InputMux t → Behavior t (Style a) → a → MWidget t m a
liftHoloStyled mux style initial = do
  tok  ← newId
  let rawD = liftDyn initial $ select mux $ Const2 tok
  valD ← ((id &&& \x→ Holo.leafStyled tok (initStyle $ compStyle x) x) <$>) <$> rawD
  pure ( constDyn $ subscription (Proxy @a) tok
       , valD)

liftHolo ∷ ∀ t m a. (Holo a, ReflexGLFWCtx t m) ⇒ InputMux t → a → MWidget t m a
liftHolo mux initial = do
  tok  ← newId
  valD ← ((id &&& \x→ Holo.leafStyled tok (initStyle $ compStyle x) x) <$>) <$>
         (liftDyn initial $ select mux $ Const2 tok)
  pure ( constDyn $ subscription (Proxy @a) tok
       , valD)

-- mkTextEntryStyleD ∷ InputMux t → Behavior t (Style Text) → Text → MWidget t m (Text, HoloBlank)
-- mkTextEntryStyleD mux styleB initialV = Holo.widget $ \tokenV → do
--   let editE = select mux $ Const2 tokenV
--   valD         ← textDyn initialV editE
--   setupE       ← getPostBuild
--   let holoE     = attachWith (Holo.leafStyled tokenV) styleB $ leftmost [updated valD, initialV <$ setupE]
--   holdDyn (initialV, Holo.emptyHolo) (attachPromptlyDyn nvalD holoE)
--    <&> (,) editMaskKeys

mkTextEntryValidatedStyleD ∷ InputMux t → Behavior t (Style Text) → Text → (Text → Bool) → MWidget t m Text
mkTextEntryValidatedStyleD mux styleB initialV testF = do
  unless (testF initialV) $
    error $ "Initial value not accepted by test: " <> T.unpack initialV
  -- (subD, textD) ← mkTextEntryStyleD mux styleB initialV
  (subD, textD) ← liftHolo mux initialV
  initial ← sample $ current textD
  foldDyn (\(new, newHoloi) (oldValid, _)→
               (if testF new then new else oldValid, newHoloi))
    initial (updated textD)
    <&> (subD,)

vboxD ∷ ∀ t m. Reflex t ⇒ [Widget' t] → MWidget' t m
vboxD chi = do
  let dyn ∷ (Dynamic t Subscription, Dynamic t [HoloBlank])
      dyn = foldr (\(s, hb) (ss, hbs)→
                      ( zipDynWith (<>) s ss
                      , zipDynWith (:) hb hbs ))
            (constDyn mempty, constDyn [])
            chi
  pure $ (id *** (Holo.vbox <$>)) dyn



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

scene ∷ ∀ t m. InputMux t → Dynamic t Integer → Dynamic t Int → Dynamic t Double → MWidget' t m
scene muxV statsValD frameNoD fpsValueD = mdo

  fpsD             ← liftDynHolo  (T.pack ∘ printf "%3d fps" ∘ (floor ∷ Double → Integer) <$> fpsValueD)
  statsD           ← liftDynHolo $ statsValD <&>
                     \(mem)→ T.pack $ printf "mem: %d" mem

  let rectDiD       = (PUs <$>) ∘ join unsafe'di ∘ fromIntegral ∘ max 1 ∘ flip mod 200 <$> frameNoD
  rectD            ← liftDynHolo $ zipDynWith Holo.Rect rectDiD (constDyn $ co 1 0 0 1)
  frameCountD      ← liftDynHolo $ T.pack ∘ printf "frame #%04d" <$> frameNoD
  -- varlenTextD      ← mkTextD portV (constDyn defStyle) (constDyn $ T.pack $ printf "even: %s" $ show True) --(T.pack ∘ printf "even: %s" ∘ show ∘ even <$> frameNoD)
  varlenTextD      ← liftDynHolo $ T.pack ∘ printf "even: %s" ∘ show ∘ even <$> frameNoD

  let action  ∷ MWidget t m Text
      action  = readField (muxV, "foo") "field"
      -- action2 ∷ MWidget t m AnObject
      -- action2 = recover2 (⊥)
  xD ∷ Widget t Text ← action

  longStaticTextD  ← liftDynHolo $ constDyn ("0....5...10...15...20...25...30...35...40...45...50...55...60...65...70...75...80...85...90...95..100" ∷ Text)

  let fontNameStyle name = defStyleOf & tsFontKey .~ Cr.FK name

  styleEntryD      ← mkTextEntryValidatedStyleD muxV styleB "defaultSans" $
                     (\x→ x ≡ "defaultMono" ∨ x ≡ "defaultSans")

  styleD           ← trackStyle $ fontNameStyle ∘ fst <$> (traceDynWith (show ∘ fst) (value styleEntryD))
  let styleB        = current styleD

  -- text2HoloQD      ← mkTextEntryStyleD muxV styleB "watch me"

  vboxD [ trim $ frameCountD
        -- , (snd <$>) <$> text2HoloQD
        , (snd <$>) <$> styleEntryD
        , trim xD
        , trim $ rectD
        , trim $ fpsD
        , trim $ longStaticTextD
        , trim $ statsD
        , trim $ varlenTextD ]



data Options where
  Options ∷
    { oTrace ∷ Bool
    } → Options
parseOptions ∷ Opt.Parser Options
parseOptions =
  Options
  <$> Opt.switch (Opt.long "trace" <> Opt.help "[DEBUG] Enable allocation tracing")

-- liftHolo' ∷ ∀ t m a. (Holo a, ReflexGLFWCtx t m) ⇒ a → MWidget t m a
liftHolo' ∷ ∀ t m a. (Holo a, ReflexGLFWCtx t m) ⇒ a → ReflexGLFW t m (Dynamic t Subscription, Dynamic t (a, HoloBlank))
liftHolo' initial = do
  tok  ← newId
  valD ← ((id &&& \x→ Holo.leafStyled tok (initStyle $ compStyle x) x) <$>) <$>
         (liftDyn initial $ select (⊥) $ Const2 tok)
  pure ( constDyn $ subscription (Proxy @a) tok
       , valD)

data AnObject where
  AnObject ∷
    { objName   ∷ Text
    -- , objDPI    ∷ DΠ
    -- , objDim    ∷ Di Int
    } → AnObject
    deriving (Eq, GHC.Generic, Show)
instance SOP.Generic         AnObject
instance SOP.HasDatatypeInfo AnObject

-- data CName where
--   CName ∷ Text → ADTChoiceT → CName

-- instance {-# OVERLAPPABLE #-} (SOP.Generic a, SOP.HasDatatypeInfo a) ⇒ Record AnObject where
-- instance {-# OVERLAPPABLE #-} (SOP.Generic a, SOP.HasDatatypeInfo a) ⇒ CtxRecord AnObject AnObject where
-- type instance ConsCtx a = CName
-- instance Ctx AnObject where
-- instance {-# OVERLAPPABLE #-} Record AnObject where
--   prefixChars = const 3
-- instance {-# OVERLAPPABLE #-} CtxRecord AnObject AnObject where
--   consCtx _ _ n ix = CName n ix
-- instance {-# OVERLAPPABLE #-}
--   ( CtxRecord a a
--   , Record      (Dynamic t Subscription, Dynamic t (a, HoloBlank)))
--   ⇒ CtxRecord a (Dynamic t Subscription, Dynamic t (a, HoloBlank)) where
--   consCtx _ _ n ix = CName n ix

-- *
-- instance Holo a ⇒ Ctx a where
-- instance Holo a ⇒ Record a where
-- -- class (SOP.Generic a, SOP.HasDatatypeInfo a, Ctx ctx, Record a) ⇒ CtxRecord ctx a where
-- instance (Holo a, SOP.Generic a, SOP.HasDatatypeInfo a) ⇒ CtxRecord a (Widget t (a, HoloBlank)) where

type instance Structure (Widget t a) = a
instance (Holo a, ReflexGLFWCtx t m) ⇒ Field (PostBuildT t (TriggerEventT t (PerformEventT t m))) (Widget t a) where
  type FieldCtx (Widget t a) = (InputMux t, a)
  readField (mux, initV) (FieldName fname) = do
    labelId ← newId
    liftHolo mux initV <&>
      (id *** (<&> (id *** (\x→ Holo.vbox [Holo.leaf labelId fname, x]))))
instance (SOP.Generic a, SOP.HasDatatypeInfo a) ⇒ Record (Widget t a) where
instance (SOP.Generic a, SOP.HasDatatypeInfo a, ReflexGLFWCtx t m) ⇒
  CtxRecord m (Widget t a) where

-- * Top level network
--
holotype ∷ ∀ t m. Typeable m ⇒ ReflexGLFWGuest t m
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

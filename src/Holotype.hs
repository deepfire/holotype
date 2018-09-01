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
{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors -Wno-unused-imports -Wno-type-defaults #-}
module Holotype where

import           Control.Monad
import           Data.Semigroup
import           Data.Singletons
import           Data.Tuple
import           Linear
import           Prelude                           hiding (id, Word)
import           Reflex                            hiding (Query, Query(..))
import           Reflex.GLFW
import qualified Codec.Picture                     as Juicy
import qualified Codec.Picture.Saving              as Juicy
import qualified Control.Concurrent.STM            as STM
import qualified Control.Monad.Ref
import qualified Data.ByteString.Lazy              as B
import qualified Data.Map.Strict                   as M
import qualified Data.Set                          as S
import qualified Data.Text                         as T
import qualified Data.Text.Zipper                  as T
import qualified Data.Time.Clock                   as Time
import qualified Data.TypeMap.Dynamic              as TM
import qualified Data.Unique                       as U
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
import           HoloCube
import qualified HoloCairo                         as Cr
import           HoloPort
import qualified HoloOS                            as HOS

-- TEMPORARY
import qualified "GLFW-b" Graphics.UI.GLFW         as GLFW


-- * Elsewhere
simpler ∷ Reflex t ⇒ Event t a → Event t ()
simpler = (() <$)

someFire ∷ Reflex t ⇒ Event t a → Event t b → Event t ()
someFire a b = simpler a <> simpler b


newPortFrame ∷ ReflexGLFWCtx t m ⇒ Event t Port → ReflexGLFW t m (Event t Frame)
newPortFrame portE = performEvent $ portNextFrame <$> portE

type Avg a = (Int, Int, [a])
avgStep ∷ Fractional a ⇒ a → (a, Avg a) → (a, Avg a)
avgStep x (_, (lim, cur, xs)) =
  let (ncur, nxs) = if cur < lim
                    then (cur + 1, x:xs)
                    else (lim,     x:Prelude.init xs)
  in ((sum nxs) / fromIntegral ncur, (lim, ncur, nxs))

average ∷ (Fractional a, ReflexGLFWCtx t m) ⇒ Int → Event t a → ReflexGLFW t m (Dynamic t a)
average n e = (fst <$>) <$> foldDyn avgStep (0, (n, 0, [])) e


-- * Words of query language: the beginning
--
data Word where
  WText   ∷ { wText ∷ T.Text } → Word
  WSource ∷ { wText ∷ T.Text } → Word
  WLens   ∷ { wText ∷ T.Text } → Word
  WError  ∷ { wText ∷ T.Text } → Word
  deriving (Eq, Show)

parserWText, parserWSource, parserWLens ∷ (Monad p, P.TokenParsing p) ⇒ p Word
parserWText   = WText   ∘ T.pack <$> P.choice
  [ P.between (P.symbol "\"") (P.symbol "\"") (P.some (P.notChar '"'))
  , P.some P.alphaNum ]
parserWSource = WSource ∘ T.pack <$>
  (P.symbol "." >> P.some P.alphaNum)
parserWLens   = WLens   ∘ T.pack <$>
  (⊥)

parserQuery ∷ (Monad p, P.TokenParsing p) ⇒ p [Word]
parserQuery = flip P.sepEndBy (P.oneOf " ") $ P.choice
  [ parserWText
  , parserWSource ]

parseQuery ∷ T.Text → Either T.Text [Word]
parseQuery x = case P.parseString parserQuery mempty (T.unpack x) of
                 P.Success r       → Right r
                 P.Failure errinfo → Left $ T.pack $ show errinfo

wordInterpStyle ∷ Word → Holo.TextStyle
wordInterpStyle x = defStyleOf
  & tsFontKey .~ "defaultSans"
  & tsColor   .~ case x of
                   WText   _ → co 0.514 0.580 0.588 1
                   WSource _ → co 0.149 0.545 0.824 1
                   WLens   _ → co 0.710 0.537 0.000 1
                   WError  _ → co 0.863 0.196 0.184 1

data QueryParseState =
  QueryParseState
  { qpsLastGoodParse ∷ [Word]
  , qpsError         ∷ Maybe Word
  }

updateQueryParseState ∷ T.Text → Maybe QueryParseState → QueryParseState
updateQueryParseState text qps =
  case parseQuery text of
    Left err → QueryParseState (fromMaybe [] $ qpsLastGoodParse <$> qps) (Just $ WError err)
    Right ws → QueryParseState ws Nothing



mkColorRectD ∷ Port → Dynamic t (Di (Unit PU)) → Dynamic t (Co Double) → ReflexGLFW t m (Dynamic t (Holo.Item Holo.PLayout))
mkColorRectD portV diD coD = do
  setupE       ← getPostBuild
  initDiV      ← sample $ current diD
  initCoV      ← sample $ current coD
  tokenV       ← newId $ printf "rect: %s" (show initCoV)

  let valD      = zipDynWith Holo.Rect diD coD
      holoD     = zipDynWith (\sty val@(Holo.Rect di _)→
                                Holo.leaf tokenV sty val & size.~(Just∘fromPU <$> di))
                             (constDyn defStyle) valD
  initHoloV    ← sample $ current holoD
  holoIOE      ← (performEvent $ (flip $ Holo.queryHoloitem portV) [] <$> leftmost [updated holoD, initHoloV <$ setupE])
  holdDyn Holo.emptyLayoutHolo holoIOE

mkTextD ∷ Port → Dynamic t (Style T.Text) → Dynamic t T.Text → ReflexGLFW t m (Dynamic t (Holo.Item Holo.PLayout))
mkTextD portV styleD valD = do
  setupE       ← getPostBuild
  initV        ← sample $ current valD
  tokenV       ← newId $ printf "%d: %s" (T.length initV) (T.unpack initV)
  let holoD     = zipDynWith (Holo.leaf tokenV) styleD valD
  initHoloV    ← sample $ current holoD
  holoIOE      ← (performEvent $ (flip $ Holo.queryHoloitem portV) [] <$> leftmost [updated holoD, initHoloV <$ setupE])
  holdDyn Holo.emptyLayoutHolo holoIOE

mkTextEntryD ∷ Port → Behavior t (Style T.Text) → Event t WorldEvent → T.Text → ReflexGLFW t m (Dynamic t (T.Text, Holo.Item Holo.PLayout))
mkTextEntryD portV styleB editE' initialV = do
  valD         ← (zipperText <$>) <$> foldDyn (\Edit{..} tz → weEdit tz) (textZipper [initialV]) editE'
  setupE       ← getPostBuild
  initV        ← sample $ current valD
  tokenV       ← newId $ printf "%d: %s" (T.length initV) (T.unpack initV)
  let holoE     = attachWith (Holo.leaf tokenV) styleB $ leftmost [updated valD, initV <$ setupE]
  holoIOE      ← (performEvent $ (flip $ Holo.queryHoloitem portV) [] <$> holoE)
  holdDyn (initialV, Holo.emptyLayoutHolo) $ attachPromptlyDyn valD holoIOE

mkTextEntryValidatedD ∷ Port → Behavior t (Style T.Text) → Event t WorldEvent → T.Text → (T.Text → Bool) → ReflexGLFW t m (Dynamic t (T.Text, Holo.Item Holo.PLayout))
mkTextEntryValidatedD portV styleB editE' initialV testF = do
  unless (testF initialV) $
    error $ "Initial value not accepted by test: " <> T.unpack initialV
  textD ← mkTextEntryD portV styleB editE' initialV
  initial ← sample $ current textD
  foldDyn (\(new, newHoloi) (oldValid, _)→
              (if testF new then new else oldValid, newHoloi))
    initial $ updated textD

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

-- * Top level network
--
holotype ∷ ∀ t m. ReflexGLFWGuest t m
holotype win _evCtl windowFrameE inputE = mdo
  HOS.unbufferStdout

  settingsV@Settings{..} ← defaultSettings
  portV@Port{..}         ← portCreate win settingsV
  -- End of init-time IO.
  --
  -- Constructing the FRP network:

  -- EXTERNAL INPUTS
  worldE ∷ Event t WorldEvent
                   ← performEvent $ inputE <&> translateEvent

  let editE         = ffilter (\case Edit{..}                     → True; _ → False) worldE
      clickE        = ffilter (\case (Click GLFW.MouseButton'1 _) → True; _ → False) worldE
  frameE           ← newPortFrame $ portV <$ windowFrameE

  fpsValueD        ← fpsCounterD frameE
  frameNoD ∷ Dynamic t Int
                   ← count       frameE
  statsValE        ← performEvent $ frameE <&>
                     \(_)→ liftIO $ do
                         mem ← HOS.gcKBytesUsed
                         pure (mem)
  statsValD        ← holdDyn 0 statsValE

  fpsD             ← mkTextD portV (constDyn defStyle) (T.pack ∘ printf "%3d fps" ∘ (floor ∷ Double → Integer) <$> fpsValueD)
  statsD           ← mkTextD portV (constDyn defStyle) $ statsValD <&>
                     \(mem)→ T.pack $ printf "mem: %d" mem

  let rectDiD       = (PUs <$>) ∘ join unsafe'di ∘ fromIntegral ∘ max 1 ∘ flip mod 200 <$> frameNoD
  rectD            ← mkColorRectD portV rectDiD (constDyn $ co 1 0 0 1)
  frameCountD      ← mkTextD portV (constDyn defStyle) (T.pack ∘ printf "frame #%04d" <$> frameNoD)
  -- varlenTextD      ← mkTextD portV (constDyn defStyle) (constDyn $ T.pack $ printf "even: %s" $ show True) --(T.pack ∘ printf "even: %s" ∘ show ∘ even <$> frameNoD)
  varlenTextD      ← mkTextD portV (constDyn defStyle)               (T.pack ∘ printf "even: %s" ∘ show ∘ even <$> frameNoD)

  longStaticTextD  ← mkTextD portV (constDyn defStyle) (constDyn "0....5...10...15...20...25...30...35...40...45...50...55...60...65...70...75...80...85...90...95..100")

  let fontNameStyle name = defStyleOf & tsFontKey .~ Cr.FK name

  styleEntryD      ← mkTextEntryValidatedD portV styleB editE "defaultSans" $
                     (\x→ x ≡ "defaultMono" ∨ x ≡ "defaultSans")
  let styleOfD'     = fontNameStyle ∘ fst <$> (traceDynWith (show ∘ fst) styleEntryD)
  styleD'          ← trackStyle styleOfD'
  let styleB        = current styleD'
  -- styleD'          ← delayDyn 0 styleD
  text2HoloQD      ← mkTextEntryD portV styleB editE "watch me"

  -- * SCENE
  let sceneD        = zipDynWith -- <&>
        (\(_, entry) [driven
                     , varlen
                     , stats, lostt, fps, rect, counter]→
          Holo.vbox [
                      varlen
                    , counter
                    , fps
                    , stats
                    , entry
                    , driven
                    , rect
                    , lostt
                    ]
        )
        styleEntryD
        $ zipDynWith (:) (snd <$> text2HoloQD)
        $ zipDynWith (:) varlenTextD
        $ zipDynWith (:) statsD
        $ zipDynWith (:) longStaticTextD
        $ zipDynWith (:) fpsD
        $ zipDynWith (:) rectD
        ((:[]) <$> frameCountD)
      scenePlacedTreeD = layout (Size $ fromPU <$> di 800 600) <$> sceneD
  -- * At every frame
  let drawE         = attachPromptlyDyn scenePlacedTreeD frameE
  -- _                ← performEvent $ clickE <&>
  --                    \(Click GLFW.MouseButton'1 (Po (V2 x y))) →
  --                      liftIO $ printf "click at %f:%f\n" x y
  drawnE           ← performEvent $ drawE <&>
                     \(tree, f@Frame{..}) → do
                       let leaves = Holo.holotreeLeaves tree
                       -- liftIO $ printf "   leaves: %d\n" $ M.size leaves
                       portGarbageCollectVisuals portV leaves
                       tree' ← Holo.visualiseHolotree portV tree
                       Holo.renderHolotreeVisuals portV tree'
                       Holo.drawHolotreeVisuals f tree'
                       pure ()
  drawnD           ← holdDyn () drawnE
  let pickE         = attachPromptlyDyn drawnD clickE
  pickedE          ← performEvent $ pickE <&>
                     \((), Click GLFW.MouseButton'1 (Po (V2 x y)))→ do
                       -- liftIO $ B.writeFile "screenshot.png" =<< Juicy.imageToPng <$> snapFrameBuffer (di 800 600)
                       p ← liftIO $ pickFrameBuffer (di 800 600) $ floor <$> po x y
                       liftIO $ printf "%d:%d: %x\n" (floor x ∷ Int) (floor y ∷ Int) p

  -- * Limit frame rate to vsync.  XXX:  also, flicker.
  waitForVSyncD    ← toggle True $ ffilter (\case VSyncToggle → True; _ → False) worldE
  _                ← performEvent $ portSetVSync <$> updated waitForVSyncD

  hold False ((\case Shutdown → True; _ → False)
               <$> worldE)


data WorldEvent where
  Move ∷
    { weΔ ∷ Po Double
    } → WorldEvent
  Edit ∷
    { weEdit ∷ T.TextZipper T.Text → T.TextZipper T.Text
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
translateEvent (U (EventMouseButton w button GLFW.MouseButtonState'Pressed _)) = do
  (,) x y ← liftIO $ GLFW.getCursorPos w
  pure $ Click button (po x y)
translateEvent (U (EventChar _ c))                                              = pure $ Edit $ T.insertChar c
translateEvent (U (EventKey  _ GLFW.Key'Enter     _ GLFW.KeyState'Pressed   _)) = pure $ Edit $ T.breakLine
translateEvent (U (EventKey  _ GLFW.Key'Backspace _ GLFW.KeyState'Pressed   _)) = pure $ Edit $ T.deletePrevChar
translateEvent (U (EventKey  _ GLFW.Key'Delete    _ GLFW.KeyState'Pressed   _)) = pure $ Edit $ T.deleteChar
translateEvent (U (EventKey  _ GLFW.Key'Left      _ GLFW.KeyState'Pressed   _)) = pure $ Edit $ T.moveLeft
translateEvent (U (EventKey  _ GLFW.Key'Up        _ GLFW.KeyState'Pressed   _)) = pure $ Edit $ T.moveUp
translateEvent (U (EventKey  _ GLFW.Key'Right     _ GLFW.KeyState'Pressed   _)) = pure $ Edit $ T.moveRight
translateEvent (U (EventKey  _ GLFW.Key'Down      _ GLFW.KeyState'Pressed   _)) = pure $ Edit $ T.moveDown
translateEvent (U (EventKey  _ GLFW.Key'Home      _ GLFW.KeyState'Pressed   _)) = pure $ Edit $ T.gotoBOL
translateEvent (U (EventKey  _ GLFW.Key'End       _ GLFW.KeyState'Pressed   _)) = pure $ Edit $ T.gotoEOL
translateEvent (U (EventKey  _ GLFW.Key'Enter     _ GLFW.KeyState'Repeating _)) = pure $ Edit $ T.breakLine
translateEvent (U (EventKey  _ GLFW.Key'Backspace _ GLFW.KeyState'Repeating _)) = pure $ Edit $ T.deletePrevChar
translateEvent (U (EventKey  _ GLFW.Key'Delete    _ GLFW.KeyState'Repeating _)) = pure $ Edit $ T.deleteChar
translateEvent (U (EventKey  _ GLFW.Key'Left      _ GLFW.KeyState'Repeating _)) = pure $ Edit $ T.moveLeft
translateEvent (U (EventKey  _ GLFW.Key'Up        _ GLFW.KeyState'Repeating _)) = pure $ Edit $ T.moveUp
translateEvent (U (EventKey  _ GLFW.Key'Right     _ GLFW.KeyState'Repeating _)) = pure $ Edit $ T.moveRight
translateEvent (U (EventKey  _ GLFW.Key'Down      _ GLFW.KeyState'Repeating _)) = pure $ Edit $ T.moveDown
translateEvent (U (EventKey  _ GLFW.Key'Home      _ GLFW.KeyState'Repeating _)) = pure $ Edit $ T.gotoBOL
translateEvent (U (EventKey  _ GLFW.Key'End       _ GLFW.KeyState'Repeating _)) = pure $ Edit $ T.gotoEOL
-- how to process key chords?
translateEvent (U (EventKey  _ GLFW.Key'F1        _ GLFW.KeyState'Pressed   _)) = pure $ ObjStream
translateEvent (U (EventKey  _ GLFW.Key'F2        _ GLFW.KeyState'Pressed   _)) = pure $ GCing
translateEvent (U (EventKey  _ GLFW.Key'F3        _ GLFW.KeyState'Pressed   _)) = pure $ VSyncToggle
translateEvent (U (EventKey  _ GLFW.Key'Insert    _ GLFW.KeyState'Pressed   _)) = pure $ Spawn
translateEvent (U (EventKey  _ GLFW.Key'Escape    _ GLFW.KeyState'Pressed   _)) = pure $ Shutdown
translateEvent _                                                                = pure $ NonEvent

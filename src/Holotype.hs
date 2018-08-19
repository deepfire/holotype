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
{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors -Wno-unused-imports #-}
module Holotype where

-- Basis
import           HoloPrelude                       hiding ((<>))
import           Prelude                           hiding (id, Word)

-- Generic
import           Control.Monad
import           Data.Semigroup
import           Data.Tuple

-- Algebra
import           Linear

-- Dirty stuff
import qualified Control.Concurrent.STM            as STM
import qualified Data.Unique                       as U

-- Reflex
import           Reflex                            hiding (Query, Query(..))
import           Reflex.GLFW

-- Text parsing & editing
import qualified Data.Map.Strict                   as M
import           Data.Singletons
import qualified Data.Set                          as S
import qualified Data.Text                         as T
import qualified Data.Text.Zipper                  as T
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

import           Holo                                     (StyleOf)
import qualified Holo
import           HoloCube
import           HoloFont
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
wordInterpStyle x = mempty
  & Holo.tsFontKey .~ "defaultSans"
  & Holo.tsColor   .~ case x of
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



mkTextWidgetD ∷ (FromUnit u, SingI u, a ~ T.Text, u ~ PU) ⇒ Port → Dynamic t (StyleOf T.Text) → Event t WorldEvent → T.Text → ReflexGLFW t m (Dynamic t (T.Text, Holo.HoloItem Holo.Layout))
mkTextWidgetD portV styleD editE' initialV = do
  -- the dynamic here is needed for state accumulation
  valD         ← (zipperText <$>) <$> foldDyn (\Edit{..} tz → weEdit tz) (textZipper [initialV]) editE'
  setupE       ← getPostBuild
  tokenV       ← newId
  --  holoE fires whenever either style is updated, or the textZipper is
  let holoD     = zipDynWith (Holo.leaf tokenV) valD styleD
  initHoloV    ← sample $ current holoD
  holoIOE      ← (performEvent $ (flip $ Holo.queryHoloitem portV) [] <$> leftmost [updated holoD, initHoloV <$ setupE])
  -- let holoE     = attachPromptlyDyn styleD (leftmost [initialV <$ setupE, updated valD])
  --                 <&> uncurry (holoLeaf tokenV) ∘ swap
  -- -- XXX: why query now?
  -- holoIOE      ← (performEvent $ (flip $ queryHoloitem portV) [] <$> holoE)
  holdDyn (initialV, Holo.emptyLayoutHolo) $ attachPromptlyDyn valD holoIOE

mkTextValidatedWidgetD ∷ (FromUnit u, SingI u, a ~ T.Text, u ~ PU) ⇒ Port → Dynamic t (StyleOf T.Text) → Event t WorldEvent → T.Text → (T.Text → Bool) → ReflexGLFW t m (Dynamic t (T.Text, Holo.HoloItem Holo.Layout))
mkTextValidatedWidgetD portV styleD editE' initialV testF = do
  unless (testF initialV) $
    error $ "Initial value not accepted by test: " <> T.unpack initialV
  textD ← mkTextWidgetD portV styleD editE' initialV
  initial ← sample $ current textD
  foldDyn (\(new, newHoloi) (oldValid, _)→
              (if testF new then new else oldValid, newHoloi))
    initial $ updated textD

-- * Top level network
--
holotype ∷ ∀ t m. ReflexGLFWGuest t m
holotype win _evCtl _setupE windowFrameE inputE = mdo
  HOS.unbufferStdout

  settingsV@Settings{..} ← defaultSettings
  portV@Port{..}         ← portCreate win settingsV
  -- End of init-time IO.
  --
  -- Constructing the FRP network:

  -- EXTERNAL INPUTS
  let worldE        ∷ Event t WorldEvent
      worldE        = translateEvent <$> inputE
      editE         = ffilter (\case Edit{..}  → True; _ → False) worldE
  frameE           ← newPortFrame $ portV <$ windowFrameE

  -- text1HoloQD ← textWidgetD "defaultSans" editE $ constant mempty { _tsFontKey = "defaultSans" }
  -- text2HoloQD ← textWidgetD "woo hoo"     editE $ constant mempty { _tsFontKey = "defaultMono" }
  styleEntryD      ← mkTextValidatedWidgetD portV (constDyn mempty { Holo._tsFontKey = "defaultMono" }) editE "defaultSans" $
                     (\x→ x ≡ "defaultMono" ∨ x ≡ "defaultSans")
  let styleD        = (\name→ mempty { Holo._tsFontKey = HoloFont.FK name }) ∘ fst <$> (traceDynWith (show ∘ fst) styleEntryD) 
  text2HoloQD      ← mkTextWidgetD portV styleD editE "watch me"

  -- * Thoughts
  --
  -- 1. flicker

  -- * SCENE
  let sceneD        = zipDynWith -- <&>
        (\(_, a) (_, b)→
          Holo.vbox [a, b])
        styleEntryD text2HoloQD
      scenePlacedTreeE  = layout (Size $ di 400 200) <$> updated sceneD

  -- * At every scene update
  sceneVisualTreeE     ← performEvent $ scenePlacedTreeE <&>
    \(tree ∷ Holo.HoloItem Holo.Layout) → liftIO $ do
      drwMap ← liftIO $ STM.readTVarIO (iomap $ fromDT portDrawableTracker)

      let leaves     ∷ M.Map IdToken (Holo.HoloItem 'Holo.Layout)
          leaves     = Holo.holotreeLeaves tree -- this shouldn't contain nodes!
          unusedDrws ∷ M.Map IdToken Drawable
          unusedDrws = M.filterWithKey (flip $ const (not ∘ flip M.member leaves)) drwMap
      forM_ (M.toList unusedDrws) $ \(idt, drv@Drawable{..})→ do
        trev FREE DRW (dDi^.di'v._x, dDi^.di'v._y) (tokenHash idt)
        disposeDrawable portObjectStream drv

      tree' ← Holo.visualiseHolotree portV tree
      Holo.renderHolotreeVisuals portV tree'
      pure tree'
  sceneVisualTreeD ← holdDyn Holo.emptyVisualHolo sceneVisualTreeE

  -- * At every frame
  let drawE         = attachPromptlyDyn sceneVisualTreeD frameE
  _                ← performEvent $ drawE <&>
                     \(tree, f@Frame{..}) → do
                       Holo.drawHolotreeVisuals portV f tree

  hold False ((\case Shutdown → True; _ → False)
               <$> worldE)


data WorldEvent where
  Move ∷
    { weΔ ∷ Po Double
    } → WorldEvent
  Edit ∷
    { weEdit ∷ T.TextZipper T.Text → T.TextZipper T.Text
    } → WorldEvent
  ObjStream   ∷ WorldEvent
  GCing       ∷ WorldEvent
  Spawn       ∷ WorldEvent
  Shutdown    ∷ WorldEvent
  NonEvent    ∷ WorldEvent

translateEvent ∷ InputU → WorldEvent
translateEvent (U (EventChar _ c))                                              = Edit $ T.insertChar c
translateEvent (U (EventKey  _ GLFW.Key'Enter     _ GLFW.KeyState'Pressed   _)) = Edit $ T.breakLine
translateEvent (U (EventKey  _ GLFW.Key'Backspace _ GLFW.KeyState'Pressed   _)) = Edit $ T.deletePrevChar
translateEvent (U (EventKey  _ GLFW.Key'Delete    _ GLFW.KeyState'Pressed   _)) = Edit $ T.deleteChar
translateEvent (U (EventKey  _ GLFW.Key'Left      _ GLFW.KeyState'Pressed   _)) = Edit $ T.moveLeft
translateEvent (U (EventKey  _ GLFW.Key'Up        _ GLFW.KeyState'Pressed   _)) = Edit $ T.moveUp
translateEvent (U (EventKey  _ GLFW.Key'Right     _ GLFW.KeyState'Pressed   _)) = Edit $ T.moveRight
translateEvent (U (EventKey  _ GLFW.Key'Down      _ GLFW.KeyState'Pressed   _)) = Edit $ T.moveDown
translateEvent (U (EventKey  _ GLFW.Key'Home      _ GLFW.KeyState'Pressed   _)) = Edit $ T.gotoBOL
translateEvent (U (EventKey  _ GLFW.Key'End       _ GLFW.KeyState'Pressed   _)) = Edit $ T.gotoEOL
translateEvent (U (EventKey  _ GLFW.Key'Enter     _ GLFW.KeyState'Repeating _)) = Edit $ T.breakLine
translateEvent (U (EventKey  _ GLFW.Key'Backspace _ GLFW.KeyState'Repeating _)) = Edit $ T.deletePrevChar
translateEvent (U (EventKey  _ GLFW.Key'Delete    _ GLFW.KeyState'Repeating _)) = Edit $ T.deleteChar
translateEvent (U (EventKey  _ GLFW.Key'Left      _ GLFW.KeyState'Repeating _)) = Edit $ T.moveLeft
translateEvent (U (EventKey  _ GLFW.Key'Up        _ GLFW.KeyState'Repeating _)) = Edit $ T.moveUp
translateEvent (U (EventKey  _ GLFW.Key'Right     _ GLFW.KeyState'Repeating _)) = Edit $ T.moveRight
translateEvent (U (EventKey  _ GLFW.Key'Down      _ GLFW.KeyState'Repeating _)) = Edit $ T.moveDown
translateEvent (U (EventKey  _ GLFW.Key'Home      _ GLFW.KeyState'Repeating _)) = Edit $ T.gotoBOL
translateEvent (U (EventKey  _ GLFW.Key'End       _ GLFW.KeyState'Repeating _)) = Edit $ T.gotoEOL
-- how to process key chords?
translateEvent (U (EventKey  _ GLFW.Key'F1        _ GLFW.KeyState'Pressed   _)) = ObjStream
translateEvent (U (EventKey  _ GLFW.Key'F2        _ GLFW.KeyState'Pressed   _)) = GCing
translateEvent (U (EventKey  _ GLFW.Key'Insert    _ GLFW.KeyState'Pressed   _)) = Spawn
translateEvent (U (EventKey  _ GLFW.Key'Escape    _ GLFW.KeyState'Pressed   _)) = Shutdown
translateEvent _                                                                = NonEvent

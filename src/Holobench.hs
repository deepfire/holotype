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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors #-}
module Holotype where

-- Basis
import           HoloPrelude                       hiding ((<>))
import           Prelude                           hiding (id, Word)
import           Control.Concurrent                       (forkIO, threadDelay)

-- Generic
import           Data.Semigroup

-- Algebra
import           Linear

-- Dirty stuff
-- import qualified Data.IORef                        as IO

-- Reflex
import           Reflex                            hiding (Query, Query(..))
import           Reflex.GLFW
import           Reflex.Random

-- Text parsing & editing
import qualified Data.Map                          as M
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
import           Holo
import           HoloFont
import           HoloCube
import           HoloPort
import qualified HoloOS                            as HOS
import           Types

-- TEMPORARY
import qualified GI.Pango                          as GIP
import           Numeric.Extra                            (doubleToFloat)
import qualified System.FSNotify                   as FS
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

wordInterpStyle ∷ (FromUnit u) ⇒ Word → TextStyle u
wordInterpStyle x = mempty
  & tsFontKey .~ "defaultMono"
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

composeScene ∷ ∀ m u. (MonadIO m, FromUnit u, u ~ PU) ⇒ Port → Di (Unit u) → T.TextZipper T.Text → m (HoloItem Visual)
composeScene port@Port{..} dim queryTextZipper = do
  let Settings{..} = portSettings
      text         = zipperText queryTextZipper
      words        = case parseQuery text of
                       Left err → [WError err]
                       Right ws → ws
      wordItem ∷ Word → HoloItem Blank
      wordItem word =
        holoLeaf port (wText word) (wordInterpStyle word ∷ TextStyle PU)
  let entryItem = holoLeaf port (textZipper [text])
                  (mempty & tesTSStyle.tsSizeSpec.tssWidth .~ (Just $ dim^.di'w))
                  -- Problem:
                  -- 1. We have size for top entry, want to record it
                  -- 2. The tree is type-coherent, and children need have the same type.
                  & size.di'v._x                           .~ (Just ∘ fromPU $ dim^.di'v._x)
      wordItems = wordItem <$> words
  let box color sz = holoLeaf port (Rect sz color ∷ Rect PU) (mempty ∷ RectStyle PU)
      -- scene = box red (di 100 100)
      scene = holoVBox
              [ box blue          (di 50 50)
              , holoHBox
                [ box (gray 0.6 0.5) (di 50 100)
                , box green          (di 100 50) ]
              , box (gray 0.5 0.5)   (di 200 50)
              ] & size .~ (Just ∘ fromPU <$> dim)
  sized ∷ HoloItem Layout ← queryHolotree port scene
  liftIO $ putStrLn "----------( sized"
  dump ppItemSize sized
  let placed = layout (sized & size .~ (Just ∘ fromPU <$> dim))
  liftIO $ putStrLn "----------( placed"
  dump ppItemArea placed
  visual ← visualiseHolotree port placed
  pure visual

updateScene ∷ PerformEvent t m ⇒ Port → QueryParseState → HoloItem Visual → m (HoloItem Visual)
updateScene = (⊥)


-- * Plan of network
--
-- Acting entities:
--
--  1. Edit events
--  2. Holo tree:
--    - containers to be resized/re-laid
--      - pure computation
--    - unaffected leaves
--      - retain drawables
--    - resized leaves
--      - communicate changes
--        - drawable identity
--          - new ones shouldn't pre-exist
--          - sameness to be preserved, across size/content changes
--        - if change detection
--          - explicit creation, teardown, resize and redraw notifictions
--        - if none:
--          - 
--      - find the old drawable, or create anew
--      - reallocate the drawable (mesh, texture, cairo context)
--      - execute the redraw
--      - upload
--    - new leaves
holotype ∷ ∀ t m. ReflexGLFWGuest t m
holotype win _evCtl setupE windowFrameE inputE = do
  HOS.unbufferStdout

  settingsV@Settings{..} ← defaultSettings
  portV@Port{..}         ← portCreate win settingsV

  -- let tfstyle = mempty
  --               & tsColor    .~ co 0.710 0.537 0.000 1
  --               & tsSizeSpec .~ (mempty & tssHeight .~ ParaLines 5)

  -- textfield0 ← mkText portV tfstyle (Left $ di 200 30)
  -- textfield1 ← mkText portV tfstyle (Left $ di 200 30)
  -- textfield2 ← mkText portV tfstyle (Left $ di 200 30)
  -- textfield3 ← mkText portV tfstyle (Left $ di 200 30)
  px0 ← mkRectDrawable portV (di (Wi $ PUs 2) 2) red
  px1 ← mkRectDrawable portV (di (Wi $ PUs 2) 2) green
  px2 ← mkRectDrawable portV (di (Wi $ PUs 2) 2) blue

  let queryInitialTextV       = "we do it for lulz"
      queryInitialTextZipperV = textZipper [queryInitialTextV]
      queryInitialParseStateV = updateQueryParseState queryInitialTextV Nothing
  initialSceneV ← composeScene portV (di 400 200) queryInitialTextZipperV
  --
  -- End of init-time IO.
  --
  -- Constructing the FRP network:
  --

  -- INPUT
  let worldE        ∷ Event t WorldEvent
      worldE        = translateEvent <$> inputE
      gcTogE        = ffilter (\case GCing     → True; _ → False) worldE
      objsTogE      = ffilter (\case ObjStream → True; _ → False) worldE
      spawnReqE     = ffilter (\case Spawn     → True; _ → False) worldE
      editE         = ffilter (\case Edit{..}  → True; _ → False) worldE
  frameE           ← newPortFrame $ portV <$ windowFrameE

  -- DATA
  let zipperD ∷ ReflexGLFWCtx t m ⇒ T.TextZipper T.Text → Event t WorldEvent → m (Dynamic t T.Text)
      zipperD initial editE =
        (zipperText <$>) <$> foldDyn (\Edit{..} tz → weEdit tz) initial editE
  -- let queryTextD = zipperText <$> queryTextZipperD
  --     queryTextE = updated queryTextD
  -- queryPSD         ← foldDyn (\tE qPS → updateQueryParseState tE (Just qPS)) queryInitialParseStateV queryTextE
  -- let queryPSE   = updated queryPSD ∷ Event t QueryParseState

  let editInPlace ∷ ReflexGLFWCtx t m
                  ⇒ Behavior t Bool     -- ^ Whether or not click-to-edit is enabled
                  → Dynamic t String    -- ^ The definitive value of the thing being edited
                  → m (Event t String)  -- ^ Event that fires when the text is edited
      editInPlace clickToEdit val = do
        pure $ "" <$ updated val
      box color sz = holoLeaf portV (Rect sz color ∷ Rect PU) (mempty ∷ RectStyle PU)
      textEntry ∷ ReflexGLFWCtx t m
                ⇒ Event t WorldEvent
                → m (Event t (HoloItem Blank))
      textEntry edit =
        pure $ box blue (di 50 50) <$ edit

  -- SCENE
  -- let sceneE        = const initialSceneV <$> frameE
  -- sceneD           ← holdDyn initialSceneV sceneE
  -- sceneD           ← (foldDynM ∷ ()) (updateScene portV) initialSceneV queryPSE
  -- sceneE           ← performEvent $ queryPSE <&>
  --                    -- XXX: need to get at the old scene somehow
  --                    \(qps, oldScene) →
  --                      updateScene portV qps oldScene

  -- SCREEN
  -- let drawReqE      = attachPromptlyDyn sceneD frameE
  -- let screenArea    = Parea (di 1.5 1.5) (po (-0.85) (-0.5))
  --     widgetLim     = Parea (di 0.2 0.2) (po 0 0)
  -- -- randomPreHoloE   ← foldRandomRs 0 ((screenArea, An 0.005)
  -- --                                   ,(widgetLim,  An 0.01)) $ () <$ driverE
  -- _                ← performEvent $ drawReqE <&>
  --                    \(scene, f@Frame{..}) → do
  --                      drawHolotree f scene
  --                      framePutDrawable f px0 (doubleToFloat <$> po  0    0)
  --                      framePutDrawable f px1 (doubleToFloat <$> po  0.3  0.3)
  --                      framePutDrawable f px2 (doubleToFloat <$> po 30   30)
  -- _                ← performEvent $ drawReqE <&>
  --                    \((mfps, (_, cs)), f@Frame{..}) → do
  --                      case mfps of
  --                        Nothing  → pure ()
  --                        Just (Holosome{..}, Parea{..}) → placeCanvas holoVisual f _paNWp
  --                      forM_ cs $ \(n, (Holosome{..}
  --                                      ,(Parea{..}  ∷ S Area True Double
  --                                       ,_angVel    ∷ An Double))) → do
  --                        placeCanvas holoVisual f _paNWp

  -- UI & DATA MUTATION
  -- let topsomD       = ffor holosomD (\case (_,[]) → Nothing
  --                                          (_,(_,h):_) → Just h)
  --     editReqE      = attachPromptlyDyn topsomD editE
                         -- update settingsV h weEdit
  -- let fpsUpdateE    = attachPromptlyDyn holosomFPSD holoFPSDataE
  -- _                ← performEvent $ fpsUpdateE <&>
  --                    \case (Nothing, _)→ pure ()
  --                          (Just (h, _), fps)→
  --                            update settingsV h (const fps)

  hold False ((\case Shutdown → True; _ → False)
              <$> worldE)

  -- ≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡
  -- (fileE ∷ Event t FS.Event) ← performEventAsync $
  --   let -- foo ∷ Event t ((FS.Event → IO ()) → Performable m ())
  --       -- foo ∷ ??? (FS.Event -> IO ()) -> Reflex.Host.Class.HostFrame t ()
  --       foo cb = do
  --           _ ← liftIO $ forkIO $
  --               FS.withManager $ \mgr → do
  --                 _ ← FS.watchDir
  --                   mgr          -- manager
  --                   "/tmp"          -- directory to watch
  --                   (const True) -- predicate
  --                   cb        -- action
  --                 forever $ threadDelay 1000000
  --           pure ()
  --   in foo <$ setupE

  -- _                ← performEvent $ fileE <&>
  --                    \f→ liftIO $ do
  --                      putStrLn ("file: " <> show f)
  -- Data ∷ Dynamic t (Integer, [(Integer, (Holosome (T.TextZipper T.Text), (S 'Area 'True Double, An Double)))])
  -- frameGateD       ← toggle False objsTogE
  -- gcingD           ← toggle False gcTogE
  -- let driverE       = simpler spawnReqE <> simpler (gate (current $ frameGateD) frameE)
  --     text n        = [ printf "Object #%d:" n
  --                     , "  Esc:           quit"
  --                     , "  F1:            toggle per-frame object stream"
  --                     , "  F2:            toggle per-frame GC"
  --                     , "  Editing keys:  edit"
  --                     , ""
  --                     , "Yay!"]
  -- holosomCountD    ← count driverE
  -- let preHoloE      = attachPromptlyDyn holosomCountD driverE <&>
  --                      (\(n, pre') → (,pre') ∘ textZipper $ T.pack <$> text n)
  -- holosomE         ← visual settingsV streamV dasStyle preHoloE
  -- holosomD         ← foldDyn (\x (n, xs)→ (n+1, (n,x):xs)) (0, []) $ holosomE

  -- Port ∷ IO FPS → Dynamic t (Maybe (Holosome (T.TextZipper T.Text), S 'Area 'True Double))
  -- kilobytesE       ← performEvent $ frameE <&>
  --                      (const $ do
  --                          -- when (sample gcingD) $ HS.gc
  --                          HOS.gcKBytesUsed)
  -- kilobytesD       ← holdDyn 0 kilobytesE

  -- frameMomentE     ← performEvent $ fmap (\_ → HOS.getTime) frameE
  -- frameΔD          ← (fst <$>) <$> foldDyn (\y (_,x)->(y-x,y)) (0,0) frameMomentE
  -- avgFrameΔD       ← average 20 $ updated frameΔD
  -- let fpsD          = (floor ∘ recip) <$> avgFrameΔD
  --     fpsArea       = Parea (di 256 256) (po (-1) (1))
  -- let holoFPSDataE  = attachPromptlyDyn (zipDyn (zipDyn fpsD holosomCountD) kilobytesD) frameE <&>
  --                     \(((fps ∷ Int, objects ∷ Int), kilobytes ∷ Integer),_) →
  --                       textZipper [T.pack $ printf "%3d fps, %5d objects, %8d KB used" fps objects kilobytes]
  -- holosomFPSE      ← visual settingsV streamV dasStyle
  --                    (setupE <&> const (textZipper ["1000 fps, 10000 objects, 10000000 KB used"], fpsArea))
  -- holosomFPSD      ← foldDyn (const ∘ Just) Nothing holosomFPSE
  -- ≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡

-- instance Holo (T.TextZipper T.Text) where
--   type Visual (T.TextZipper T.Text) = Canvas (RRect H.Text)
--   visualise stts strea sty tzip = do
--     vis ← assemble stts strea sty $ zipperText tzip
--     render vis
--     pure vis
--   updateVisual _ _ vis tzip = do
--     let H.Text{..} = (innerOf ∘ innerOf) vis
--     liftIO $ IO.writeIORef tTextRef $ zipperText tzip -- this will go away
--     render vis


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


data World where
  Void ∷ World
  Singleton ∷
    { posn   ∷ Po Double
    , tz     ∷ T.TextZipper T.Text
    } → World
instance Monoid World where
  mempty = Void
  mappend Void x = x
  mappend x Void = x
  mappend x _    = x -- XXX: a violation, indeed


-- Time & math
-- Average of the given event's payload over the last given number of
-- occurrences.
-- averageEWE ∷ (Fractional a, Monad m) ⇒ Int → Event t a → m (Event t a)
-- averageEWE n = lmap (fmap go) (unfoldE Seq.empty)
--     where
--     go x xs' =
--         let xs = Seq.take n (x Seq.<| xs')
--         in (foldl' (+) 0 xs / fromIntegral (Seq.length xs),
--             xs)


-- GLFW tips & tricks:
--
-- getClipboardString :: Window -> IO (Maybe String)
-- GLFW.windowShouldClose win

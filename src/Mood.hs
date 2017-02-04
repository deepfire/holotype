{-# LANGUAGE Arrows #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main (main) where

import Data.IORef
import Data.Maybe
import Data.Char (toLower)
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map as Map
import Data.Time.Clock
import Control.Concurrent
import Control.Monad
import Control.Arrow.Unicode
import Prelude.Unicode

import System.Environment
import System.FilePath
import System.Directory
import System.IO
import System.Exit

import Data.Aeson (encode,eitherDecode)
import "GLFW-b" Graphics.UI.GLFW as GLFW
import Graphics.GL.Core33
import FRP.Elerea.Param
import LambdaCube.GL as GL

import Sound.ProteaAudio
import Text.XML.HXT.Core hiding (when, moveLeft, moveRight)
import qualified Data.Graph as DataGraph

import Camera
import Engine
import GameEngine.Loader.Zip
import qualified Data.ByteString.Char8 as SB8

import Text.Printf

import Data.String.Utils hiding (join)
-- Example of using a PangoLayout

import Data.IORef
import Data.Monoid ((<>))
import qualified Data.Text as T

import Graphics.Rendering.Cairo (moveTo, Render)
import qualified GI.Gtk as Gtk (main, init)
import GI.Gtk
       (DrawingArea, widgetShowAll, onWidgetKeyPressEvent,
        iMContextFilterKeypress, onWidgetKeyReleaseEvent,
        iMContextFocusOut, onWidgetFocusOutEvent, iMContextFocusIn,
        onWidgetFocusInEvent, widgetGetWindow, iMContextSetClientWindow,
        onWidgetRealize, onIMContextDeleteSurrounding,
        iMContextSetSurrounding, onIMContextRetrieveSurrounding,
        onIMContextCommit, iMContextGetPreeditString,
        onIMContextPreeditChanged, onIMContextPreeditEnd,
        onIMContextPreeditStart, iMMulticontextNew, onWidgetDraw,
        onWidgetSizeAllocate, widgetQueueDraw, widgetSetSizeRequest,
        containerAdd, drawingAreaNew, mainQuit, onWidgetDestroy, windowNew)
import GI.Gtk.Enums (WrapMode(..), WindowType(..))
import GI.Pango
       (AttrList, Attribute, attrListInsert, attrListNew, Layout,
        layoutSetWidth, layoutNew, layoutSetAttributes, layoutSetText,
        layoutSetWrap)
import GI.PangoCairo.Interfaces.FontMap (fontMapGetDefault)
import GI.PangoCairo.Functions (showLayout)
import GI.Gdk.Structs.Rectangle (getRectangleWidth)
import GI.Gdk.Structs.EventKey (getEventKeyState, getEventKeyKeyval)
import GI.Gdk (keyvalToUnicode, keyvalName, EventKey)
import GI.Cairo.Structs.Context (Context(..))
import Foreign.ForeignPtr (withForeignPtr)
import Control.Monad.Trans.Reader (ReaderT(..))
import Graphics.Rendering.Cairo.Types (Cairo(..))
import Foreign.Ptr (castPtr)
import Graphics.Rendering.Cairo.Internal (Render(..))
import Control.Monad.IO.Class (MonadIO(..))

loremIpsum = "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna\ \ aliqua. Ut enim ad minim veniam, quis nostrud exercitation\ \ ullamco laboris nisi ut aliquip ex ea commodo consequat.\ \ Duis aute irure dolor in reprehenderit in voluptate\ \ velit esse cillum dolore eu fugiat nulla pariatur.\ \ Excepteur sint occaecat cupidatat non proident, sunt in culpa\ \ qui officia deserunt mollit anim id est laborum."

data Buffer = Buffer T.Text Int

defaultBuffer = Buffer loremIpsum (T.length loremIpsum)

displayBuffer (Buffer str pos) =
  before <> "<CURSOR>" <> after
  where (before,after) = T.splitAt pos str

displayBufferPreedit (Buffer str pos) preeditStr preeditPos =
  before <> "[" <> prebefore <> "<CURSOR>" <> preafter <> "]" <> after
  where (before,after) = T.splitAt pos str
        (prebefore, preafter) = T.splitAt preeditPos preeditStr

insertStr new (Buffer str pos) = Buffer (before<>new<>after) (pos+T.length new)
  where (before,after) = T.splitAt pos str

deleteChar b@(Buffer str 0) = b
deleteChar (Buffer str pos) = Buffer (T.init before <> after) (pos-1)
  where (before,after) = T.splitAt pos str

moveLeft b@(Buffer str pos) | pos==0 = b
                            | otherwise = Buffer str (pos-1)

moveRight b@(Buffer str pos) | pos==T.length str = b
                             | otherwise = Buffer str (pos+1)

attrListNewFromList :: MonadIO m => [Attribute] -> m AttrList
attrListNewFromList list = do
    al <- attrListNew
    mapM_ (attrListInsert al) list
    return al

data Graph = Graph
  { nodes ∷ [(String, [(String, (String, String))])],
    edges ∷ [(String, String)] -- (Source, target)
  }
  deriving (Show, Eq)

cleanup_label ∷ [String] → String
cleanup_label [] = ""
cleanup_label xs = concat $ fmap strip xs

atTag tag = deep (isElem ⋙ hasName tag)

parseEdges = atTag "edge" ⋙
  proc e → do
    source ← getAttrValue "source" -< e
    target ← getAttrValue "target" -< e
    returnA -< (source, target)

parseDatas = atTag "data" ⋙
  proc v → do
    subs ← (atTag "y:GenericNode" ⋙
            proc gn → do
              geom  ←       (atTag "y:Geometry"  ⋙ proc g  → do; x ← getAttrValue "x" -< g; y ← getAttrValue "y" -< g; returnA -< (x, y)) -< gn
              label ← listA (atTag "y:NodeLabel" ⋙ proc tx → do; t ← getText ⋘ getChildren -< tx; returnA -< t) -< gn
              returnA -< (cleanup_label $ label, geom)) -< v
    returnA -< subs

parseNodes = atTag "node" ⋙
  proc n → do
    nodeId ← getAttrValue "id" -< n
    subs ← listA parseDatas -< n
    returnA -< (nodeId, subs)

parseGraph = atTag "graph" ⋙
  proc g → do
    nodes ← listA parseNodes -< g
    edges ← listA parseEdges -< g
    returnA -< Graph{nodes=nodes, edges=edges}

getEdges = atTag "edge" ⋙ getAttrValue "source"

-- Get targets for a single node in a Graph
getTargets :: String -> Graph -> [String]
getTargets source graph = map snd $ filter ((==source).fst) $ edges graph

-- Convert a graph node into a Data.Graph-usable
getDataGraphNode :: Graph -> String -> (String, String, [String])
getDataGraphNode graph node = (node, node, getTargets node graph)

-- Convert a Graph instance into a Data.Graph list of (node, nodeid, edge) tuples
getDataGraphNodeList :: Graph -> [(String, String, [String])]
getDataGraphNodeList graph = map (getDataGraphNode graph ∘ fst) (nodes graph)

type Sink a = a -> IO ()

lc_q3_cache = ".lc_q3.cache" -- local cache: generated files, compiled pipelines are stored here
q3shader_cache = lc_q3_cache </> "q3shader.cache"
timeDiff m = (\s x e -> (diffUTCTime e s, x))
  <$> getCurrentTime
  <*> m
  <*> getCurrentTime
showTime delta
    | t > 1e-1  = printf "%.3fs" t
    | t > 1e-3  = printf "%.1fms" (t/1e-3)
    | otherwise = printf "%.0fus" (t/1e-6)
  where
    t = realToFrac delta :: Double
printTimeDiff message m = do
  (t,r) <- timeDiff m
  putStr message
  putStrLn $ showTime t
  return r

loadMoodGraphics :: GLStorage -> String -> IO (Maybe GLRenderer)
loadMoodGraphics storage name = do
    putStrLn $ "load " ++ name
    let localName  = "lc" </> name
        paths = [lc_q3_cache </> name,localName]
    validPaths <- filterM doesFileExist paths
    when (null validPaths) $ fail $ name ++ " is not found in " ++ show paths
    renderer <- printTimeDiff "allocate pipeline..." $ do
      eitherDecode <$> LB.readFile (head validPaths) >>= \case
        Left err -> fail err
        Right ppl -> allocRenderer ppl
    printTimeDiff "setStorage..." $ setStorage renderer storage
    return $ Just renderer

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    --hSetBuffering stdin NoBuffering
    let fileName = "/home/deepfire/src/virt-doc/Gvandra I initialisation.graphml"

    graphs ← runX (readDocument [withValidate no] fileName ⋙ parseGraph)
    --  Convert Graph structure to Data.Graph-importable tuple list
    let graphEdges = getDataGraphNodeList $ head graphs
    -- Convert to a Data.Graph
    let (graph, vertexMap) = DataGraph.graphFromEdges' graphEdges
    -- Example of what to do with the Graph: Print vertices
    -- print $ map ((\ (vid, _, _) → vid) . vertexMap) (DataGraph.vertices graph)
    print $ graphs

    noPak0_pk3 <- null . filter (\n -> "pak0.pk3" == map toLower n) <$> getDirectoryContents "."
    when noPak0_pk3 $ die "Could not find pak0.pk3. See how to run: https://github.com/lambdacube3d/lambdacube-quake3/blob/master/README.md"

    pk3Data <- loadPK3
    args <- getArgs
    let bspNames = [n | n <- Map.keys pk3Data, ".bsp" == takeExtension n]
    fullBSPName <- head <$> case args of
      (n:xs) -> return $ filter ((== n) . takeBaseName) bspNames
      _ -> do
            let maps = map takeBaseName bspNames
            putStrLn $ "Available maps:"
            putStrLn $ unwords maps
            putStrLn "Enter map name:"
            name <- getLine
            return $ filter ((name ==) . takeBaseName) bspNames
    let bspName = takeBaseName fullBSPName

    win <- initWindow "LC DSL Quake 3 Demo" 800 600

    -- loading screen
    loadingScreen <- createLoadingScreen
    (w,h) <- getFramebufferSize win
    drawLoadingScreen w h loadingScreen pk3Data bspName
    swapBuffers win
    pollEvents

    -- initAudio 64 44100 1024

    (inputSchema,levelData) <- engineInit pk3Data fullBSPName

    -- compile graphics pipeline
    let pplName = bspName ++ "_ppl.json"
    compileRequest <- newIORef False
    compileReady <- newIORef False
    _ <- forkIO $ forever $ do -- start compile thread
      putStrLn "start to compile"
      writeIORef compileRequest False
      writeIORef compileReady False
      compileQuake3GraphicsCached pplName >>= writeIORef compileReady
      putStrLn "compile finished"
      let loop = do
            req <- readIORef compileRequest
            threadDelay 100000 -- 10 / sec
            unless req loop
      loop

    -- upload graphics data to GPU
    storage <- allocStorage inputSchema
    graphicsData <- setupStorage pk3Data levelData storage
    putStrLn "storage created"

    simpleRenderer <- fromJust <$> loadMoodGraphics storage "SimpleGraphics.json"
    setStorage simpleRenderer storage
    rendererRef <- newIORef =<< fromJust <$> loadMoodGraphics storage "SimpleGraphics.json"

    -- play level music
    -- case getMusicFile levelData of
    -- let musicFName = "assets/audio/erokia-ambient-wave-12-stretched.wav" :: String
    -- buf <- SB8.readFile musicFName
    -- -- load from memory buffer
    -- smp' <- case takeExtension musicFName of
    --  ".ogg" -> SB8.useAsCStringLen buf $ \(p,i) -> sampleFromMemoryOgg p i 1
    --  ".wav" -> do
    --    printf "read music WAV %s\n" musicFName
    --    SB8.useAsCStringLen buf $ \(p,i) -> sampleFromMemoryWav p i 1
    -- soundPlay smp' 1.0 1.0 0.0 1.0

    (mousePosition,mousePositionSink) <- external (0,0)
    (fblrPress,fblrPressSink)     <- external (False,False,False,False,False,False)
    (canvasPress,canvasPressSink) <- external (False,False,False,False,False)

    let draw debugRender = do
          if debugRender
            then renderFrame simpleRenderer
            else readIORef rendererRef >>= renderFrame
          swapBuffers win
          pollEvents

    sc <- start $ do
        u <- scene win levelData graphicsData mousePosition fblrPress canvasPress
        return $ (draw <$> u)
    s <- fpsState
    setTime 0
    driveNetwork sc (readInput compileRequest compileReady pplName rendererRef storage win s mousePositionSink fblrPressSink canvasPressSink)

    disposeRenderer =<< readIORef rendererRef
    putStrLn "storage destroyed"

    finishAudio
    destroyWindow win

edge :: Signal Bool -> SignalGen p (Signal Bool)
edge s = transfer2 False (\_ cur prev _ -> cur && not prev) s =<< delay False s

upEdge :: Signal Bool -> SignalGen p (Signal Bool)
upEdge s = transfer2 False (\_ cur prev _ -> cur && prev == False) s =<< delay False s

scene :: Window -> Engine.EngineContent -> Engine.EngineGraphics
      -> Signal (Float, Float) -> Signal (Bool, Bool, Bool, Bool, Bool, Bool) -> Signal (Bool, Bool, Bool, Bool, Bool)
      -> SignalGen Float (Signal Bool)
scene win levelData graphicsData mousePosition fblrPress canvasPress = do
    time <- stateful 0 (+)
    last2           <- transfer ((0,0),(0,0))  (\_ n (_,b) ->                (b,n)) mousePosition
    lastCanvasPress <- transfer ((False, False, False, False, False), (False, False, False, False, False))
                                (\_ kn (_,ko) -> (ko,kn)) canvasPress
    let mouseMove = (\((ox,oy),(nx,ny)) -> (nx-ox,ny-oy)) <$> last2
        bsp = getBSP levelData
        p0 = head . drop 1 . cycle $ getSpawnPoints levelData
    fblrPress' <- do
      j' <- upEdge $ (\(w,a,s,d,t,j) -> j) <$> fblrPress
      return $ (\(w,a,s,d,t,_) j' -> (w,a,s,d,t,j')) <$> fblrPress <*> j'
    controlledCamera <- userCamera (getTeleportFun levelData) bsp p0 mouseMove fblrPress'
    canvas           <- canvas canvasPress

    frameCount <- stateful (0 :: Int) (\_ c -> c + 1)
    
    let activeCamera = controlledCamera
    let setupGFX (camPos,camTarget,camUp,brushIndex) canvasSitu@(cvori, cvpos) time ((ko1,ko2,ko3,ko4,ko5),(kn1,kn2,kn3,kn4,kn5)) = do
            (w,h) <- getFramebufferSize win
            -- hack
            let keyIsPressed k = fmap (==KeyState'Pressed) $ getKey win k
                kdown ko kn = ko && not kn
            -- noBSPCull <- keyIsPressed (Key'X)
            debugRender <- keyIsPressed (Key'C)
            updateRenderInput graphicsData (camPos,camTarget,camUp) w h time canvasSitu
            when (kdown ko1 kn1 || kdown ko2 kn2 || kdown ko3 kn3 || kdown ko4 kn4 || kdown ko5 kn5) $ do
              printf "orient: %s, pos %s\n" (show cvori) (show cvpos)
            return debugRender
    r <- effectful4 setupGFX activeCamera canvas time lastCanvasPress
    return r


readInput compileRequest compileReady pplName rendererRef storage win s mousePos fblrPress canvasPress = do
    let keyIsPressed k = fmap (==KeyState'Pressed) $ getKey win k
    t <- maybe 0 id <$> getTime
    setTime 0

    (x,y) <- getCursorPos win
    mousePos (realToFrac x,realToFrac y)

    fblrPress =<< ((,,,,,) <$> keyIsPressed Key'A <*> keyIsPressed Key'W <*> keyIsPressed Key'S <*> keyIsPressed Key'D
                           <*> keyIsPressed Key'RightShift <*> keyIsPressed Key'Space)

    canvasPress =<< ((,,,,) <$> keyIsPressed Key'X <*> keyIsPressed Key'Y <*> keyIsPressed Key'Z <*> keyIsPressed Key'LeftShift <*> keyIsPressed Key'LeftAlt)

    let dt = realToFrac t

    updateFPS s dt

    reload <- keyIsPressed Key'L
    when reload $ writeIORef compileRequest True
    readIORef compileReady >>= \case
      False -> return ()
      True -> do
        writeIORef compileReady False
        loadQuake3Graphics storage pplName >>= \case
          Nothing -> return ()
          Just a  -> do
            readIORef rendererRef >>= disposeRenderer
            writeIORef rendererRef a
    k <- keyIsPressed Key'Escape
    return $ if k then Nothing else Just (min 0.1 $ realToFrac dt) -- simulation must run at least 10 FPS, under 10 FPS won't be realtime

-- FRP boilerplate
driveNetwork :: (p -> IO (IO a)) -> IO (Maybe p) -> IO ()
driveNetwork network driver = do
    dt <- driver
    case dt of
        Just dt -> do
            join $ network dt
            driveNetwork network driver
        Nothing -> return ()

-- OpenGL/GLFW boilerplate

initWindow :: String -> Int -> Int -> IO Window
initWindow title width height = do
    GLFW.init
    defaultWindowHints
    mapM_ windowHint
      [ WindowHint'ContextVersionMajor 3
      , WindowHint'ContextVersionMinor 3
      , WindowHint'OpenGLProfile OpenGLProfile'Core
      , WindowHint'OpenGLForwardCompat True
      ]
    Just win <- createWindow width height title Nothing Nothing
    makeContextCurrent $ Just win
    glEnable GL_FRAMEBUFFER_SRGB
    swapInterval 0
    return win

-- FPS tracking

data State = State { frames :: IORef Int, t0 :: IORef Double }

fpsState :: IO State
fpsState = State <$> newIORef 0 <*> newIORef 0

updateFPS :: State -> Double -> IO ()
updateFPS state t1 = do
  let t = 1000*t1
      fR = frames state
      tR = t0 state
  modifyIORef fR (+1)
  t0' <- readIORef tR
  writeIORef tR $ t0' + t
  when (t + t0' >= 5000) $ do
    f <- readIORef fR
    let seconds = (t + t0') / 1000
        fps = fromIntegral f / seconds
    putStrLn (show (round fps) ++ " FPS - " ++ show f ++ " frames in " ++ show seconds)
    writeIORef tR 0
    writeIORef fR 0

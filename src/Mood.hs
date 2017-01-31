{-# LANGUAGE Arrows #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}

module Mood ()
where

import Data.IORef
import Data.Maybe
import Data.Char (toLower)
import qualified Data.Map as Map
import Control.Concurrent
import Control.Monad
import Control.Arrow.Unicode
import Prelude.Unicode

import System.Environment
import System.FilePath
import System.Directory
import System.IO
import System.Exit

import "GLFW-b" Graphics.UI.GLFW as GLFW
import Graphics.GL.Core33
import FRP.Elerea.Param
import LambdaCube.GL as GL

import Sound.ProteaAudio
import Text.XML.HXT.Core hiding (when)
import qualified Data.Graph as DataGraph

import Camera
import Engine
import GameEngine.Loader.Zip
import qualified Data.ByteString.Char8 as SB8

#ifdef CAPTURE
import Codec.Image.DevIL
import Text.Printf
import Foreign
#endif

import Data.String.Utils hiding (join)

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

#ifdef CAPTURE
-- framebuffer capture function
withFrameBuffer :: Int -> Int -> Int -> Int -> (Ptr Word8 -> IO ()) -> IO ()
withFrameBuffer x y w h fn = allocaBytes (w*h*4) $ \p -> do
    glReadPixels (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h) GL_RGBA GL_UNSIGNED_BYTE $ castPtr p
    fn p
#endif

captureRate :: Double
captureRate = 30

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

#ifdef CAPTURE
    ilInit
#endif

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

    simpleRenderer <- fromJust <$> loadQuake3Graphics storage "SimpleGraphics.json"
    setStorage simpleRenderer storage
    rendererRef <- newIORef =<< fromJust <$> loadQuake3Graphics storage "SimpleGraphics.json"

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
    (fblrPress,fblrPressSink) <- external (False,False,False,False,False,False)
    (capturePress,capturePressSink) <- external False
    (waypointPress,waypointPressSink) <- external []

    let draw (captureA,debugRender) = do
          if debugRender
            then renderFrame simpleRenderer
            else readIORef rendererRef >>= renderFrame
          captureA
          swapBuffers win
          pollEvents

    capRef <- newIORef False
    sc <- start $ do
        u <- scene win levelData graphicsData mousePosition fblrPress capturePress waypointPress capRef
        return $ (draw <$> u)
    s <- fpsState
    setTime 0
    driveNetwork sc (readInput compileRequest compileReady pplName rendererRef storage win s mousePositionSink fblrPressSink capturePressSink waypointPressSink capRef)

    disposeRenderer =<< readIORef rendererRef
    putStrLn "storage destroyed"

    finishAudio
    destroyWindow win

edge :: Signal Bool -> SignalGen p (Signal Bool)
edge s = transfer2 False (\_ cur prev _ -> cur && not prev) s =<< delay False s

upEdge :: Signal Bool -> SignalGen p (Signal Bool)
upEdge s = transfer2 False (\_ cur prev _ -> cur && prev == False) s =<< delay False s

scene win levelData graphicsData mousePosition fblrPress capturePress waypointPress capRef = do
    time <- stateful 0 (+)
    last2 <- transfer ((0,0),(0,0)) (\_ n (_,b) -> (b,n)) mousePosition
    let mouseMove = (\((ox,oy),(nx,ny)) -> (nx-ox,ny-oy)) <$> last2
        bsp = getBSP levelData
        p0 = head . drop 1 . cycle $ getSpawnPoints levelData
    fblrPress' <- do
      j' <- upEdge $ (\(w,a,s,d,t,j) -> j) <$> fblrPress
      return $ (\(w,a,s,d,t,_) j' -> (w,a,s,d,t,j')) <$> fblrPress <*> j'
    controlledCamera <- userCamera (getTeleportFun levelData) bsp p0 mouseMove fblrPress'

    frameCount <- stateful (0 :: Int) (\_ c -> c + 1)
    capture <- transfer2 False (\_ cap cap' on -> on /= (cap && not cap')) capturePress =<< delay False capturePress
    
    {-
    [clearWaypoints, setWaypoint, stopPlayback, startPlayback, incPlaybackSpeed, decPlaybackSpeed] <-
        forM (zip [edge, edge, edge, edge, return, return] [0..]) $ \(process, i) -> process (fmap (!! i) waypointPress)

    waypoints <- recordSignalSamples setWaypoint clearWaypoints ((\(camPos, targetPos, _, _) -> (camPos, targetPos)) <$> controlledCamera)
    playbackSpeed <- transfer2 100 (\dt inc dec speed -> speed + 10*dt*(if inc then 1 else if dec then -1 else 0)) incPlaybackSpeed decPlaybackSpeed
    splineCamera <- playbackCamera startPlayback stopPlayback playbackSpeed waypoints
    let activeCamera = do
            camData <- splineCamera
            case camData of
                Nothing -> controlledCamera
                Just camData -> return camData
    -}
    let activeCamera = controlledCamera
    let setupGFX (camPos,camTarget,camUp,brushIndex) time (capturing,frameCount) = do
            (w,h) <- getFramebufferSize win
            -- hack
            let keyIsPressed k = fmap (==KeyState'Pressed) $ getKey win k
            noBSPCull <- keyIsPressed (Key'X)
            debugRender <- keyIsPressed (Key'C)
            updateRenderInput graphicsData (camPos,camTarget,camUp) w h time noBSPCull
            {-
            when (not $ null brushIndex) $ do
              putStrLn $ "brush collision: " ++ show (map (getModelIndexFromBrushIndex levelData) brushIndex)
            -}
            let captureA = do
#ifdef CAPTURE
                  when capturing $ do
                      glFinish
                      withFrameBuffer 0 0 w h $ \p -> writeImageFromPtr (printf "frame%08d.jpg" frameCount) (h,w) p
                  writeIORef capRef capturing
#endif
                  return ()
            return (captureA,debugRender)
    r <- effectful3 setupGFX activeCamera time ((,) <$> capture <*> frameCount)
    return r

readInput compileRequest compileReady pplName rendererRef storage win s mousePos fblrPress capturePress waypointPress capRef = do
    let keyIsPressed k = fmap (==KeyState'Pressed) $ getKey win k
    t <- maybe 0 id <$> getTime
    setTime 0

    (x,y) <- getCursorPos win
    mousePos (realToFrac x,realToFrac y)

    fblrPress =<< ((,,,,,) <$> keyIsPressed Key'A <*> keyIsPressed Key'W <*> keyIsPressed Key'S <*> keyIsPressed Key'D
                           <*> keyIsPressed Key'RightShift <*> keyIsPressed Key'Space)
    capturePress =<< keyIsPressed Key'P
    waypointPress =<< mapM keyIsPressed [Key'R,Key'E,Key'1,Key'2,Key'F,Key'G]

    isCapturing <- readIORef capRef
    let dt = if isCapturing then recip captureRate else realToFrac t

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

{-# LANGUAGE LambdaCase, ViewPatterns, RecordWildCards, TupleSections, PackageImports, OverloadedStrings, GADTs #-}
module Engine
  ( loadPK3
  , createLoadingScreen
  , drawLoadingScreen
  , engineInit
  , setupStorage
  , updateRenderInput
  -- temp
  , loadQuake3Graphics
  , compileQuake3GraphicsCached
  , getSpawnPoints
  , getBSP
  , getModelIndexFromBrushIndex
  , getTeleportFun
  , getMusicFile
  --
  , EngineContent, EngineGraphics
  ) where

import Control.Monad
import Data.Char
import Data.Digest.CRC32
import Data.HashSet (HashSet)
import qualified Data.ByteString.Char8 as SB8
import qualified Data.HashSet as HashSet
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import Data.List (isPrefixOf,partition,isInfixOf,elemIndex)
import Data.Maybe
import Data.Vect
import Data.Set (Set)
import Data.Map (Map)
import Data.Vector (Vector)
import qualified Data.Text as DT
import Foreign
import System.FilePath
import System.Directory
import qualified System.IO.Unsafe as UN

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import Data.Binary (encodeFile,decodeFile)
import qualified Data.ByteString.Lazy as LB
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as SB
import qualified Data.Set as Set
import qualified Data.HashSet as HashSet

import Text.Printf (printf)
import Text.Show.Pretty (ppShow)

import qualified Graphics.Rendering.Cairo          as GRC -- (moveTo, Render, Format(..))
import qualified Graphics.Rendering.Cairo.Internal as GRC (Render(..), create, imageSurfaceCreate)
import qualified Graphics.Rendering.Cairo.Types    as GRC -- (Cairo(..))
import qualified Graphics.Rendering.Cairo.Internal as GRCI-- (Cairo(..))

import qualified Data.GI.Base                      as GI
import qualified GI.Cairo                          as GIC
import qualified GI.Cairo.Structs.Context          as GIC -- (Context(..))
import qualified GI.Pango                          as GIP
       -- (AttrList, GL.Attribute, attrListInsert, attrListNew, Layout,
       --  layoutSetWidth, layoutNew, layoutSetAttributes, layoutSetText,
       --  layoutSetWrap)
import qualified GI.PangoCairo.Interfaces.FontMap  as GIPC -- (fontMapGetDefault)
import qualified GI.PangoCairo.Functions           as GIPC -- (createLayout, showLayout)

import Foreign.ForeignPtr (withForeignPtr)
import Control.Monad.Trans.Reader (ReaderT(..))
import Foreign.Ptr (castPtr)
import Foreign.C.Types (CUChar)
import Control.Monad.IO.Class (MonadIO(..))

import Codec.Picture
import Codec.Picture.Types
import Data.Vector.Mutable

import qualified LambdaCube.GL     as GL
import LambdaCube.GL.Mesh
import LambdaCube.GL.Type (TextureData(..))
import Graphics.GL.Core33

import GameEngine.Data.BSP
import GameEngine.Data.MD3
import GameEngine.Data.GameCharacter
import GameEngine.Data.Material hiding (Vec3,Entity)
import qualified GameEngine.Data.Material as GE
import GameEngine.Content
import GameEngine.Graphics.Culling
import GameEngine.Graphics.Frustum
import GameEngine.Graphics.Storage
import GameEngine.Graphics.BSP
import GameEngine.Graphics.MD3
import GameEngine.Loader.MD3
import GameEngine.Loader.BSP
import GameEngine.Loader.Zip
import GameEngine.Utils
import qualified GameEngine.Data.MD3 as MD3
import qualified GameEngine.Loader.Entity as E

import Entity
import Content

type EngineContent =
  ( BSPLevel
  , Map ByteString MD3.MD3Model
  , [(Proj4, (Map String String, String))]
  , [[(Proj4, (Map String String, [Char]))]]
  , [Character]
  , Map String CommonAttrs
  , [Vec3]
  , V.Vector Int
  , ([Entity], Map ByteString Entity)
  , Maybe String
  )

type EngineGraphics =
  ( GL.GLStorage
  , [(Proj4, MD3Instance)]
  , [Character]
  , [(Proj4, (MD3.MD3Model, MD3Instance), (MD3.MD3Model, MD3Instance),(MD3.MD3Model, MD3Instance))]
  , V.Vector [GL.Object]
  , BSPLevel
  , MD3Instance, Canvas
  , [(Float, GL.SetterFun TextureData, V.Vector TextureData)]
  )

loremIpsum = "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna\ \ aliqua. Ut enim ad minim veniam, quis nostrud exercitation\ \ ullamco laboris nisi ut aliquip ex ea commodo consequat.\ \ Duis aute irure dolor in reprehenderit in voluptate\ \ velit esse cillum dolore eu fugiat nulla pariatur.\ \ Excepteur sint occaecat cupidatat non proident, sunt in culpa\ \ qui officia deserunt mollit anim id est laborum."

createMoodRenderInfo :: Map FilePath CommonAttrs -> HashSet FilePath -> HashSet FilePath -> (GL.PipelineSchema, Map FilePath CommonAttrs)
createMoodRenderInfo shMap' levelMaterials modelMaterials = (inputSchema,shMapTexSlot) where
  mkShader hasLightmap n = case Map.lookup n shMap' of
    Just s -> (n,s)
    Nothing -> let alias = dropExtension n in case Map.lookup alias shMap' of
      Just s -> (alias,s)
      Nothing -> (n,imageShader hasLightmap n)

  imageShader hasLightmap txName = defaultCommonAttrs {caStages = sa:if hasLightmap then saLM:[] else []} where
    sa = defaultStageAttrs
        { saTexture     = ST_Map txName
        , saBlend       = Nothing
        , saTCGen       = TG_Base
        , saDepthWrite  = True
        , saRGBGen      = RGB_IdentityLighting
        }
    saLM = defaultStageAttrs
        { saTexture = ST_Lightmap
        , saBlend   = Just (B_DstColor,B_Zero)
        , saTCGen   = TG_Lightmap
        , saRGBGen  = RGB_IdentityLighting
        }

  shMap = Map.fromList [mkShader True n | n <- HashSet.toList levelMaterials]  `Map.union`
          Map.fromList [mkShader False n | n <- HashSet.toList modelMaterials] `Map.union`
          Map.fromList [("canvasMaterial"
                        ,defaultCommonAttrs
                         { caSort   = 9.0
                         , caStages = [defaultStageAttrs
                                        { saTexture     = ST_ClampMap "canvasMaterial"
                                        , saBlend       = Just ( B_SrcAlpha , B_OneMinusSrcAlpha )
                                        , saTCGen       = TG_Base
                                        , saDepthWrite  = True
                                        , saRGBGen      = RGB_IdentityLighting
                                        }]})]

  shMapTexSlot = mangleCA <$> shMap
    where
      mangleStageTex stageTex = "Tex_" ++ show (crc32 $ SB.pack $ show stageTex)
      mangleCA ca = ca {caStages = mangleSA <$> caStages ca}
      mangleSA sa = sa {saTextureUniform = mangleStageTex sa}

  textureUniforms = Set.toList . Set.fromList . concat . map name . concat . map caStages $ Map.elems shMapTexSlot
    where
      name s = [saTextureUniform s]
      {-
      name s = case saTexture s of
        ST_Map n        -> [n]
        ST_ClampMap n   -> [n]
        ST_AnimMap _ n  -> [head n]
        ST_Lightmap     -> ["LightMap"]
        ST_WhiteImage   -> []

      -}
  quake3SlotSchema =
    GL.ObjectArraySchema GL.Triangles $ Map.fromList
      [ ("color",       GL.Attribute_V4F)
      , ("diffuseUV",   GL.Attribute_V2F)
      , ("normal",      GL.Attribute_V3F)
      , ("position",    GL.Attribute_V3F)
      , ("lightmapUV",  GL.Attribute_V2F)
      ]

  debugSlotSchema =
    GL.ObjectArraySchema GL.Triangles $ Map.fromList
      [ ("position",    GL.Attribute_V3F)
      , ("color",       GL.Attribute_V4F)
      ]

  inputSchema = {-TODO-}
    GL.PipelineSchema
    { objectArrays = Map.fromList $ ("CollisionShape",debugSlotSchema) : zip ("LightMapOnly":"missing shader": Map.keys shMap) (repeat quake3SlotSchema)
    , uniforms = Map.fromList $ [ ("viewProj",      GL.M44F)
                                , ("worldMat",      GL.M44F)
                                , ("viewMat",       GL.M44F)
                                , ("orientation",   GL.M44F)
                                , ("viewOrigin",    GL.V3F)
                                , ("entityRGB",     GL.V3F)
                                , ("entityAlpha",   GL.Float)
                                , ("identityLight", GL.Float)
                                , ("time",          GL.Float)
                                , ("LightMap",      GL.FTexture2D)
                                , ("Noise",                GL.FTexture2D)
                                , ("SinTable",             GL.FTexture2D)
                                , ("SquareTable",          GL.FTexture2D)
                                , ("SawToothTable",        GL.FTexture2D)
                                , ("InverseSawToothTable", GL.FTexture2D)
                                , ("TriangleTable",        GL.FTexture2D)
                                , ("origin",    GL.V3F)
                                ] ++ zip textureUniforms (repeat GL.FTexture2D)
    }

-- TODO
engineInit :: Map String Entry -> FilePath -> IO (GL.PipelineSchema, EngineContent)
engineInit pk3Data fullBSPName = do
    let bspName = takeBaseName fullBSPName
        bspEntry = case Map.lookup fullBSPName pk3Data of
            Nothing -> error "You need to put pk3 file into your current directory"
            Just bspd -> bspd

    putStrLn $ "loading: " ++ show bspName
    -- load bsp data
    bsp <- readBSP . LB.fromStrict <$> readEntry bspEntry

    createDirectoryIfMissing True lc_q3_cache -- create cache

    SB.writeFile (lc_q3_cache </> bspName ++ ".entities") $ blEntities bsp

    -- extract spawn points
    let ents = case E.parseEntities bspName $ blEntities bsp of
            Left err -> error err
            Right x -> x
        spawnPoint E.EntityData{..}
          | classname `elem` [ "info_player_deathmatch"
                             , "info_player_start"
                             , "team_CTF_bluespawn"
                             , "team_CTF_redspawn"
                             , "team_CTF_blueplayer"
                             , "team_CTF_redplayer"
                             ] = [origin]
          | otherwise = []
        spawnPoints = concatMap spawnPoint ents
        p0 = head spawnPoints
        teleportData = loadTeleports ents
        music = (head . words) <$> (E.music $ head ents)

    -- MD3 related code
    (characterSkinMaterials,characterObjs,characters) <- readCharacters pk3Data p0
    (md3Materials,md3Map,md3Objs) <- readMD3Objects characterObjs ents pk3Data
    printf "---------\nMD3 materials:\n%s\n" $ show md3Materials
    printf "---------\nBSP materials:\n%s\n" $ show $ map (SB.unpack . GameEngine.Data.BSP.shName) $ V.toList $ blShaders bsp
    --putStrLn $ "level materials"
    --mapM_ SB.putStrLn $ map shName $ V.toList $ blShaders bsp
    shMap <- do
      let q3shader_cache = "foo.shc"
      hasShaderCache <- doesFileExist q3shader_cache
      case hasShaderCache of
        True -> putStrLn "load shader cache" >> decodeFile q3shader_cache
        False -> do
                  putStrLn "create shader cache"
                  sm <- loadShaderMap pk3Data
                  encodeFile q3shader_cache sm
                  return sm
    let
        maxMaterial = 20 -- TODO: remove if we will have fast reducer
        shNames = Set.fromList $ {-Prelude.take maxMaterial $ -}selectedMaterials ++ ignoredMaterials
        allShName = map GameEngine.Data.BSP.shName $ V.toList $ blShaders bsp
        (selectedMaterials,ignoredMaterials) = partition (\n -> or $ [SB.isInfixOf k n | k <- ["floor","wall","door","trim","block"]]) allShName

    let levelMaterials = HashSet.fromList . Set.toList $ Set.map SB.unpack shNames
        modelMaterials = HashSet.fromList . Set.toList $ Set.map SB.unpack (md3Materials `Set.union` characterSkinMaterials)
        (inputSchema,shMapTexSlot) = createMoodRenderInfo shMap levelMaterials modelMaterials
    --putStrLn $ "all materials:  " ++ show (Map.size shMap')
    --putStrLn $ "used materials: " ++ show (Map.size shMap)
    --putStrLn $ "texture uniforms: \n" ++ ppShow textureUniforms
    --putStrLn $ "used materials: " ++ show (Map.size shMapTexSlot)
    --putStrLn $ "ignored materials: " ++ show (length ignoredMaterials)
    writeSampleMaterial shMapTexSlot
    --SB.putStrLn $ SB.unlines ignoredMaterials

    let brushModelMapping = V.replicate (V.length $ blBrushes bsp) (-1) V.//
          (concat $ V.toList $ V.imap (\i Model{..} -> [(n,i) | n <- [mdFirstBrush..mdFirstBrush+mdNumBrushes-1]]) (blModels bsp))
    putStrLn $ "bsp model count: " ++ show (V.length $ blModels bsp)
    -- print brushModelMapping
    -- print teleportData
    return (inputSchema,(bsp,md3Map,md3Objs,characterObjs,characters,shMapTexSlot,spawnPoints,brushModelMapping,teleportData,music))

getMusicFile (bsp,md3Map,md3Objs,characterObjs,characters,shMapTexSlot,spawnPoints,brushModelMapping,teleportData,music) = music
getModelIndexFromBrushIndex (bsp,md3Map,md3Objs,characterObjs,characters,shMapTexSlot,spawnPoints,brushModelMapping,teleportData,music) brushIndex = brushModelMapping V.! brushIndex
getBSP (bsp,md3Map,md3Objs,characterObjs,characters,shMapTexSlot,spawnPoints,brushModelMapping,teleportData,music) = bsp
getSpawnPoints (bsp,md3Map,md3Objs,characterObjs,characters,shMapTexSlot,spawnPoints,brushModelMapping,teleportData,music) = spawnPoints
getTeleportFun levelData@(bsp,md3Map,md3Objs,characterObjs,characters,shMapTexSlot,spawnPoints,brushModelMapping,(teleport,teleportTarget),music) brushIndex p =
  let models = map (getModelIndexFromBrushIndex levelData) brushIndex
      hitModels = [tp | TriggerTeleport target model <- teleport, model `elem` models, TargetPosition _ tp <- maybeToList $ Map.lookup target teleportTarget]
  --in head $ trace (show ("hitModels",hitModels,models)) hitModels ++ [p]
  in head $ hitModels ++ [p]

--- What do we lose?
-- 1. buffer per surface vs. per model
-- 2. frame lossage is ok, since we don't intend to have frames
-- 3. shader lossage is ok, since, well, no common shaders, yay, ok, no?
--
-- Frame {frMins = Vec3 (-10.765625) (-10.921875) (-5.84375), frMaxs = Vec3 10.828125 10.671875 15.75, frOrigin = Vec3 0.0 0.0 0.0, frRadius = 22.01359, frName = "(from ase)"}
uploadMD3Surface :: MD3.Surface -> MD3.Frame -> IO GPUMD3S
uploadMD3Surface surface@MD3.Surface{..} frame = do
  let cvtSurface :: MD3.Surface -> (GL.Array, GL.Array, V.Vector (GL.Array, GL.Array))
      cvtSurface MD3.Surface{..} =
        ( GL.Array GL.ArrWord32 (SV.length srTriangles) (withV srTriangles)
        , GL.Array GL.ArrFloat (2 * SV.length srTexCoords) (withV srTexCoords)
        , V.map cvtPosNorm srXyzNormal
        )
        where
          withV a f = SV.unsafeWith a (\p -> f $ castPtr p)
          cvtPosNorm (p,n) = (f p, f n) where f sv = GL.Array GL.ArrFloat (3 * SV.length sv) $ withV sv

      addSurface sf (il,tl,pl,nl,pnl) = (i:il,t:tl,p:pl,n:nl,pn:pnl) where
        (i,t,pn) = cvtSurface sf
        (p,n)    = V.head pn

      (il,tl,pl,nl,pnl) = addSurface surface ([],[],[],[],[])

  buffer <- GL.compileBuffer (concat [il,tl,pl,nl])

  let nMdFrames   = 1
      numSurfaces = 1
      surfaceData idx MD3.Surface{..} = (index,attributes) where
        index = GL.IndexStream buffer idx 0 (SV.length srTriangles)
        countV = SV.length srTexCoords
        attributes = Map.fromList $
          [ ("diffuseUV",   GL.Stream GL.Attribute_V2F buffer (1 * numSurfaces + idx) 0 countV)
          , ("position",    GL.Stream GL.Attribute_V3F buffer (2 * numSurfaces + idx) 0 countV)
          , ("normal",      GL.Stream GL.Attribute_V3F buffer (3 * numSurfaces + idx) 0 countV)
          , ("color",       GL.ConstV4F (GL.V4 1 1 1 1))
          , ("lightmapUV",  GL.ConstV2F (GL.V2 0 0))
          ]
      frames :: Data.Vector.Vector [(Int, GL.Array)]
      frames = foldr addSurfaceFrames emptyFrame $ zip [0..] pnl where
        emptyFrame = V.replicate nMdFrames []
        addSurfaceFrames (idx,pn) f = V.zipWith (\l (p,n) -> (2 * numSurfaces + idx,p):(3 * numSurfaces + idx,n):l) f pn

  return $ GPUMD3S
    { gpumd3sBuffer    = buffer
    , gpumd3sStreams   = surfaceData 0 surface
    , gpumd3sFrames    = frames
    , gpumd3sShaders   = HashSet.fromList $ map (SB8.unpack . MD3.shName) $ V.toList srShaders
    , gpumd3sFrame     = frame
    , gpumd3sSurface   = surface
    }

data GPUMD3S
  = GPUMD3S
  { gpumd3sBuffer    :: GL.Buffer
  , gpumd3sStreams   :: (GL.IndexStream GL.Buffer,Map String (GL.Stream GL.Buffer)) -- index stream, attribute streams
  , gpumd3sFrames    :: V.Vector [(Int, GL.Array)]
  , gpumd3sShaders   :: HashSet String
  , gpumd3sSurface   :: MD3.Surface
  , gpumd3sFrame     :: MD3.Frame
  }

data MD3SInstance
  = MD3SInstance
  { md3sinstanceObject  :: [GL.Object]
  , md3sinstanceBuffer  :: GL.Buffer
  , md3sinstanceFrames  :: V.Vector [(Int, GL.Array)]
  , md3sinstanceSurface :: MD3.Surface
  }

type MD3Skin = Map String String

addGPUMD3Surface :: GL.GLStorage -> GPUMD3S -> MD3Skin -> [String] -> IO MD3SInstance
addGPUMD3Surface r GPUMD3S{..} skin unis = do
    let sf@MD3.Surface{..} = gpumd3sSurface
    let (index, attrs) = gpumd3sStreams
    objs <- do
        let materialName s = case Map.lookup (SB8.unpack $ srName) skin of
              Nothing -> SB8.unpack $ MD3.shName s
              Just a  -> a
        objList <- concat <$> forM (V.toList $ srShaders) (\s -> do
          printf "addObjectWithmaterial:\n  %s\n  %s\n\n" (show $ materialName s) (show unis) -- (ppShow attrs)
          a <- addObjectWithMaterial r (materialName s) GL.TriangleList (Just index) attrs $ setNub $ "worldMat":unis
          b <- GL.addObject          r "LightMapOnly"   GL.TriangleList (Just index) attrs $ setNub $ "worldMat":unis
          return [a,b])

        -- add collision geometry
        let Frame{..} = gpumd3sFrame
        collisionObjs <- do
            sphereObj <- uploadMeshToGPU (sphere (GL.V4 1 0 0 1) 4 frRadius) >>= addMeshToObjectArray r "CollisionShape" (setNub $ ["worldMat","origin"] ++ unis)
            boxObj <- uploadMeshToGPU (bbox (GL.V4 0 0 1 1) frMins frMaxs) >>= addMeshToObjectArray r "CollisionShape" (setNub $ ["worldMat","origin"] ++ unis)
            --when (frOrigin /= zero) $ putStrLn $ "frOrigin: " ++ show frOrigin
            return [sphereObj,boxObj]

        return $ objList ++ collisionObjs
    -- question: how will be the referred shaders loaded?
    --           general problem: should the gfx network contain all passes (every possible materials)?
    return $ MD3SInstance
        { md3sinstanceObject  = objs
        , md3sinstanceBuffer  = gpumd3sBuffer
        , md3sinstanceFrames  = gpumd3sFrames
        , md3sinstanceSurface = sf
        }

addMD3Surface :: GL.GLStorage -> MD3.Surface -> MD3.Frame -> MD3Skin -> [String] -> IO MD3SInstance
addMD3Surface r surface frame skin unis = do
    gpuMD3 <- uploadMD3Surface surface frame
    addGPUMD3Surface r gpuMD3 skin unis

grcToGIC :: GRC.Cairo -> IO GIC.Context
grcToGIC grc = GIC.Context <$> GI.newManagedPtr (castPtr $ GRC.unCairo grc) (return ())

{-# INLINE doRange #-}
doRange :: Int -> Int -> (Int -> IO ()) -> IO ()
doRange from to action =
  let loop n | n >= to   = return ()
             | otherwise = do action n
                              loop (n+1)
   in loop from

setupStorage :: Map String Entry -> EngineContent -> GL.GLStorage -> IO EngineGraphics
setupStorage pk3Data (bsp,md3Map,md3Objs,characterObjs,characters,shMapTexSlot,_,_,_,_) storage = do
    let slotU           = GL.uniformSetter storage
        entityRGB       = GL.uniformV3F "entityRGB" slotU
        entityAlpha     = GL.uniformFloat "entityAlpha" slotU
        identityLight   = GL.uniformFloat "identityLight" slotU
        worldMat        = GL.uniformM44F "worldMat" slotU
        overbrightBits  = 0
        idmtx = GL.V4 (GL.V4 1 0 0 0) (GL.V4 0 1 0 0) (GL.V4 0 0 1 0) (GL.V4 0 0 0 1)
    worldMat idmtx
    entityRGB $ GL.V3 1 1 1
    entityAlpha 1
    identityLight $ 1 / (2 ^ overbrightBits)
    initTableTextures >>= setupTableTextures slotU

    -- default texture
    let redBitmap x y = let v = if (x+y) `mod` 2 == 0 then 255 else 0 in PixelRGB8 v v 0
    defaultTexture <- GL.uploadTexture2DToGPU' False False False False $ ImageRGB8 $ generateImage redBitmap 2 2

    canvas <- renderCanvasInitial storage shMapTexSlot
              (CanvasRequest loremIpsum (256, 256) (0, 0) (1, 1, 0, 1) (0.3, 0.3, 0.3, 0) defaultFontDesc)

    putStrLn "loading textures:"
    -- load textures
    animTex <- fmap concat $ forM (Set.toList $ Set.fromList $ concatMap (\(shName,sh) -> [(shName,saTexture sa,saTextureUniform sa,caNoMipMaps sh) | sa <- caStages sh]) $ Map.toList shMapTexSlot) $
      \(shName,stageTex,texSlotName,noMip) -> do -- texSlotName :: Approx "Tex_3913048198"
        let texSetter = GL.uniformFTexture2D (SB.pack texSlotName) slotU
            setTex isClamped img = if img == cvMaterial canvas -- don't touch our canvas..
                                   then return []
                                   else (texSetter =<< loadQ3Texture (not noMip) isClamped defaultTexture pk3Data shName img) >> return []
        case stageTex of
            ST_Map img          -> setTex False img
            ST_ClampMap img     -> setTex True  img
            ST_AnimMap freq imgs   -> do
                txList <- mapM (loadQ3Texture (not noMip) False defaultTexture pk3Data shName) imgs
                let txVector = V.fromList txList
                return [(fromIntegral (V.length txVector) / freq,texSetter,txVector)]
            _ -> return []

    putStrLn "add bsp to storage"
    surfaceObjs <- bspinstanceSurfaces <$> addBSP (Map.keysSet shMapTexSlot) storage bsp

    -- add entities
    let addMD3Obj (mat,(skin,name)) = case Map.lookup (SB.pack name) md3Map of
          Nothing -> return []
          Just md3 -> do
                    putStrLn ("add model: " ++ name)
                    lcmd3 <- addMD3 storage md3 skin ["worldMat"]
                    return [(mat,lcmd3)]

    lcMD3Objs <- pure [] --concat <$> forM md3Objs addMD3Obj

    -- weapon
    chunk <- loadMD3 "./chunk.md3"
    let weapon_model = chunk -- (fromJust $ Map.lookup (SB.pack handWeapon) md3Map)
    -- lcMD3Weapon <- addMD3 storage weapon_model mempty ["worldMat","viewProj"]
    lcMD3Weapon <- addMD3 storage (fromJust $ Map.lookup (SB.pack handWeapon) md3Map) mempty ["worldMat","viewProj"]

    -- add characters
    lcCharacterObjs <- pure mempty -- forM characterObjs
      (\[(mat,(hSkin,hName)),(_,(uSkin,uName)),(_,(lSkin,lName))] -> do
        let Just hMD3 = Map.lookup (SB.pack hName) md3Map
            Just uMD3 = Map.lookup (SB.pack uName) md3Map
            Just lMD3 = Map.lookup (SB.pack lName) md3Map
        hLC <- addMD3 storage hMD3 hSkin ["worldMat"]
        uLC <- addMD3 storage uMD3 uSkin ["worldMat"]
        lLC <- addMD3 storage lMD3 lSkin ["worldMat"]
        return (mat,(hMD3,hLC),(uMD3,uLC),(lMD3,lLC))
      )
    return (storage,lcMD3Objs,characters,lcCharacterObjs,surfaceObjs,bsp,lcMD3Weapon,canvas,animTex)

uploadTexture2DToGPU''' :: Bool -> (Int, Int, Ptr Foreign.C.Types.CUChar) -> IO TextureData
uploadTexture2DToGPU''' isMip (w, h, ptr) = do
    glPixelStorei GL_UNPACK_ALIGNMENT 1
    to <- alloca $! \pto -> glGenTextures 1 pto >> peek pto
    glBindTexture GL_TEXTURE_2D to
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S $ fromIntegral GL_REPEAT
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T $ fromIntegral GL_REPEAT
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER $ fromIntegral GL_NEAREST
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER $ fromIntegral GL_NEAREST
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_BASE_LEVEL 0
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAX_LEVEL $ fromIntegral 0
    let internalFormat  = fromIntegral $ GL_RGBA8
        dataFormat      = fromIntegral $ GL_RGBA8
    glTexImage2D GL_TEXTURE_2D 0 internalFormat (fromIntegral w) (fromIntegral h) 0 dataFormat GL_UNSIGNED_BYTE $ castPtr ptr
    when isMip $ glGenerateMipmap GL_TEXTURE_2D
    return $ TextureData to

uploadTexture2DToGPU'''' :: Bool -> Bool -> Bool -> Bool -> (Int, Int, GLenum, Ptr Foreign.C.Types.CUChar) -> IO TextureData
uploadTexture2DToGPU'''' isFiltered isSRGB isMip isClamped (w, h, format, ptr) = do
    glPixelStorei GL_UNPACK_ALIGNMENT 1
    to <- alloca $! \pto -> glGenTextures 1 pto >> peek pto
    glBindTexture GL_TEXTURE_2D to
    let texFilter = if isFiltered then GL_LINEAR else GL_NEAREST
        wrapMode = case isClamped of
            True    -> GL_CLAMP_TO_EDGE
            False   -> GL_REPEAT
        (minFilter,maxLevel) = case isFiltered && isMip of
            False   -> (texFilter,0)
            True    -> (GL_LINEAR_MIPMAP_LINEAR, floor $ log (fromIntegral $ max w h) / log 2)
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S $ fromIntegral wrapMode
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T $ fromIntegral wrapMode
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER $ fromIntegral minFilter
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER $ fromIntegral texFilter
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_BASE_LEVEL 0
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAX_LEVEL $ fromIntegral maxLevel
    let internalFormat  = fromIntegral $ if isSRGB then GL_SRGB8_ALPHA8 else GL_RGBA8
    glTexImage2D GL_TEXTURE_2D 0 internalFormat (fromIntegral w) (fromIntegral h) 0 format GL_UNSIGNED_BYTE $ castPtr ptr
    when isMip $ glGenerateMipmap GL_TEXTURE_2D
    return $ TextureData to

uploadTexture2DToGPU'' :: Bool -> Bool -> TextureData -> DynamicImage -> IO ()
uploadTexture2DToGPU'' isSRGB isMip (TextureData to) bitmap' = do
    let bitmap = case bitmap' of
          ImageRGB8 i@(Image w h _)   -> bitmap'
          ImageRGBA8 i@(Image w h _)  -> bitmap'
          ImageYCbCr8 i@(Image w h _) -> error "unsupported texture pixel format!" -- ImageRGB8 $ convertImage i
          di -> ImageRGBA8 $ convertRGBA8 di
        withBitmap (ImageRGB8 (Image w h v)) f = SV.unsafeWith v $ f (w,h) 3 0
        withBitmap (ImageRGBA8 (Image w h v)) f = SV.unsafeWith v $ f (w,h) 4 0
        withBitmap _ _ = error "unsupported image type :("
    glBindTexture GL_TEXTURE_2D to
    withBitmap bitmap $ \(w,h) nchn 0 ptr -> do
        let internalFormat  = fromIntegral $ if isSRGB then (if nchn == 3 then GL_SRGB8 else GL_SRGB8_ALPHA8) else (if nchn == 3 then GL_RGB8 else GL_RGBA8)
            dataFormat      = fromIntegral $ case nchn of
                3   -> GL_RGB
                4   -> GL_RGBA
                _   -> error "unsupported texture format!"
        glTexImage2D GL_TEXTURE_2D 0 internalFormat (fromIntegral w) (fromIntegral h) 0 dataFormat GL_UNSIGNED_BYTE $ castPtr ptr
    when isMip $ glGenerateMipmap GL_TEXTURE_2D
    return ()

defaultFontDesc :: GIP.FontDescription
defaultFontDesc = UN.unsafePerformIO $ fontDescriptionFromArgs "Terminus" GIP.StyleNormal 12288

fontDescriptionFromArgs :: String -> GIP.Style -> Int -> IO GIP.FontDescription
fontDescriptionFromArgs family style pus = do
  fd <- GIP.fontDescriptionNew
  GIP.fontDescriptionSetFamily fd $ DT.pack family
  GIP.fontDescriptionSetStyle  fd style
  GIP.fontDescriptionSetSize   fd $ fromIntegral pus
  pure fd

  -- fmap        <- GIPC.fontMapGetDefault
  -- ffam        <- fromJust <$> (tryFontMapFamily  fmap $ DT.pack fontfamily)
  -- fface       <- fromJust <$> (tryFontFamilyFace ffam $ DT.pack fontface)
  -- fcsizes     <- fromJust <$> GIP.fontFaceListSizes fface
  -- unless (elem (fromIntegral fontpts) fcsizes) $
  --   error $ printf "No font size %dPU for font %s-%s.\nAvailable sizes: %s." fontpts fontfamily fontface (show fcsizes)
  -- printf "------------ got: %s-%s-%s\n" (DT.unpack $ UN.unsafePerformIO $ GIP.fontFamilyGetName ffam) (DT.unpack $ UN.unsafePerformIO $ GIP.fontFaceGetFaceName fface) (show fcsizes)

tryFontMapFamily :: GIP.FontMap -> DT.Text -> IO (Maybe GIP.FontFamily)
tryFontMapFamily fm req = loop =<< GIP.fontMapListFamilies fm
  where loop []     = pure Nothing
        loop (f:fs) = do
          name <- GIP.fontFamilyGetName f
          if name == req
          then pure $ Just f
          else loop fs
tryFontFamilyFace :: GIP.FontFamily -> DT.Text -> IO (Maybe GIP.FontFace)
tryFontFamilyFace fa req = loop =<< GIP.fontFamilyListFaces fa
  where loop []     = pure Nothing
        loop (f:fs) = do
          name <- GIP.fontFaceGetFaceName f
          if name == req
          then pure $ Just f
          else loop fs

data CanvasRequest where
  CanvasRequest    ::
    { crText       :: DT.Text
    , crDim        :: (Int, Int)
    , crPad        :: (Int, Int)
    , crFG         :: (Double, Double, Double, Double)
    , crBG         :: (Double, Double, Double, Double)
    , crFontDesc   :: GIP.FontDescription
    } -> CanvasRequest

data Canvas where
  Canvas ::
    { cvMaterial :: String
    , cvMatSlot  :: GL.GLUniformName
    , cvTexture  :: TextureData
    , cvMD3      :: MD3SInstance
    , cvGRCr     :: GRC.Cairo
    , cvGRCSurf  :: GRC.Surface
    , cvGICtx    :: GIC.Context
    , cvGIPLay   :: GIP.Layout
    } -> Canvas

renderCanvasInitial :: GLStorage -> Map String CommonAttrs -> CanvasRequest -> IO Canvas
renderCanvasInitial storage shMapTexSlot (CanvasRequest text (areaw, areah) (padx, pady) fgColor bgColor fontdesc) = do
  let (reqw, reqh) = (padx + areaw + padx, pady + areah + pady)
      (dx, dy)     = (fromIntegral reqw, fromIntegral (-reqh)) --(fromIntegral reqw, -fromIntegral reqh)
      n            = (1) -- the normal

  grcSurface  <- GRC.createImageSurface GRC.FormatARGB32 reqw reqh
  strideBytes <- GRC.imageSurfaceGetStride grcSurface
  let stridePxs = strideBytes `div` 4
  grc         <- GRC.create grcSurface
  gic         <- grcToGIC grc
  gip         <- GIPC.createLayout gic
  GIP.layoutSetFontDescription gip (Just fontdesc)
  GIP.layoutSetWrap      gip GIP.WrapModeWord
  GIP.layoutSetEllipsize gip GIP.EllipsizeModeEnd
  GIP.layoutSetText      gip text (-1)
  GIP.layoutSetWidth     gip =<< (GIP.unitsFromDouble $ fromIntegral areaw)
  GIP.layoutSetHeight    gip =<< (GIP.unitsFromDouble $ fromIntegral areah)
  ((rw, rh), ellipsized) <- (`runReaderT` grc) $ GRC.runRender $ do
    -- clear
    let (r, g, b, a) = bgColor
    GRC.setSourceRGBA r g b a
    GRC.paint
    -- padx
    GRC.moveTo (fromIntegral padx) (fromIntegral pady)
    -- layout
    let (r, g, b, a) = fgColor
    GRC.setSourceRGBA r g b a
    GIPC.showLayout gic gip
    ellipsized <- GIP.layoutIsEllipsized gip
    (, ellipsized) <$> GIP.layoutGetPixelSize gip

  pixels      <- GRCI.imageSurfaceGetData grcSurface
  cvTexture   <- uploadTexture2DToGPU'''' False False False False $ (stridePxs, 256, GL_BGRA, pixels)

  let cvMaterial = "canvasMaterial"
      cvMatSlot  = head $ concat $ concatMap (\(shName,sh) -> [if shName == cvMaterial then [saTextureUniform sa] else [] | sa <- caStages sh]) $ Map.toList shMapTexSlot
      widthOfStride = fromIntegral reqw / fromIntegral stridePxs
      cvSurface  = MD3.Surface
                   { srName      = "canvasSurface"
                   , srShaders   = V.fromList  [MD3.Shader {shIndex = 0, shName = SB.pack cvMaterial }]
                   , srTriangles = SV.fromList [0,2,1,0,1,3]
                   , srTexCoords = SV.fromList [Vec2 0 1, Vec2 widthOfStride 0, Vec2 0 0, Vec2 widthOfStride 1]
                   , srXyzNormal = V.singleton ( SV.fromList [Vec3 0 dy 0, Vec3 dx 0 0, Vec3 0 0 0, Vec3 dx dy 0]
                                               , SV.fromList [Vec3 0  0 n, Vec3  0 0 n, Vec3 0 0 n, Vec3  0  0 n] ) }
      cvFrame = MD3.Frame { frMins   = Vec3 0 0 0
                          , frMaxs   = Vec3 1 1 0
                          , frOrigin = Vec3 0 0 0
                          , frRadius = 1.42
                          , frName   = "baseframe" }
  cvMD3       <- addMD3Surface storage cvSurface cvFrame mempty ["worldMat","viewProj"]

  printf "reqw=%d, stride=%d, widthOfStride=%f\n" reqw stridePxs widthOfStride
  GL.uniformFTexture2D (SB.pack cvMatSlot) (GL.uniformSetter storage) cvTexture

  pure Canvas { cvMaterial = cvMaterial
              , cvMD3      = cvMD3
              , cvTexture  = cvTexture
              , cvMatSlot  = SB.pack cvMatSlot
              , cvGRCr     = grc
              , cvGRCSurf  = grcSurface
              , cvGICtx    = gic
              , cvGIPLay   = gip }

-- | Orthogonal transformation matrix in row major order.
ortho :: Float  -- ^ Left
      -> Float  -- ^ Right
      -> Float  -- ^ Bottom
      -> Float  -- ^ Top
      -> Float  -- ^ Near
      -> Float  -- ^ Far
      -> Mat4
ortho l r b t n f = transpose $
    Mat4 (Vec4 (2/(r-l))    0          0       (-(r+l)/(r-l)))
         (Vec4   0        (2/(t-b))    0       (-(t+b)/(t-b)))
         (Vec4   0          0       (-2/(f-n)) (-(f+n)/(f-n)))
         (Vec4   0          0          0              1)

-- TODO
updateRenderInput :: EngineGraphics
                  -> (Vec3, Vec3, Vec3)
                  -> Int -> Int -> Float -> (Vec3, Vec3) -> IO ()
updateRenderInput (storage, lcMD3Objs, characters, lcCharacterObjs, surfaceObjs, bsp, lcMD3Weapon, canvas@Canvas{..}, animTex)
                  (camPos@(Vec3 cx cy cz), camTarget, camUp)
                  w h time (Vec3 cvx cvy cvz, cvpos) = do
            let slotU = GL.uniformSetter storage

            let matSetter   = GL.uniformM44F "viewProj" slotU
                viewOrigin  = GL.uniformV3F "viewOrigin" slotU
                orientation = GL.uniformM44F "orientation" slotU
                viewMat     = GL.uniformM44F "viewMat" slotU
                timeSetter  = GL.uniformFloat "time" slotU

            let cm = fromProjective (lookat camPos camTarget camUp)                             -- camera orientation transform
                pm = GameEngine.Utils.perspective near far (fovDeg / 180 * pi) (fromIntegral w / fromIntegral h) -- perspective matrix
                sm = fromProjective (scaling $ Vec3 s s s)                                      -- scale matrix
                s  = 0.005
                near = 0.00001/s
                far  = 100/s
                fovDeg = 60
                frust = frustum fovDeg (fromIntegral w / fromIntegral h) near far camPos camTarget camUp
                cullObject obj p = GL.enableObject obj (pointInFrustum p frust)

            let idM44F = mat4ToM44F $ idmtx -- inverse cm .*. (fromProjective $ translation (Vec3 0 (0) (-30)))
            let --cvrot = fromProjective $ orthogonal $ toOrthoUnsafe $ rotMatrixZ cvz .*. rotMatrixY cvy .*. rotMatrixX cvx
                cvrot = fromProjective $ orthogonal $ toOrthoUnsafe $ rotMatrixZ cvz .*. rotMatrixY 100 .*. rotMatrixX 100
                aspect = (fromIntegral w) / (fromIntegral h) :: Float
                (vpw,vph) = (fromIntegral w/2, fromIntegral h/2)
                om    = ortho (-vpw) vpw (-vph) vph (-1) 1
            -- uploadTexture2DToGPU'' False False cvTexture $ ImageRGBA8 $ generateImage gen 256 256
            -- updateUniforms storage $ do
            --   cvMatSlot @= return cvTexture
            forM_ (md3sinstanceObject cvMD3) $ \obj -> do
              GL.uniformM44F "viewProj" (GL.objectUniformSetter obj) aspectM44F -- $ mat4ToM44F $! om -- .*. (fromProjective $! Data.Vect.translation cvpos) -- $! cvrot .*. (fromProjective $ translation cvpos) .*. om -- .*. smcanvas
              GL.uniformM44F "worldMat" (GL.objectUniformSetter obj) idM44F -- invCM

            -- set uniforms
            timeSetter $ time / 1
            viewOrigin $ GL.V3 cx cy cz
            viewMat $ mat4ToM44F cm
            matSetter $! mat4ToM44F $! cm .*. sm .*. pm

            forM_ lcMD3Objs $ \(mat,lcmd3) -> do
              forM_ (md3instanceObject lcmd3) $ \obj -> do
                let m = mat4ToM44F $ fromProjective $ (rotationEuler (Vec3 time 0 0) .*. mat)
                    p = trim . _4 $ fromProjective mat
                GL.uniformM44F "worldMat" (GL.objectUniformSetter obj) m
                cullObject obj p

            forM_ (zip characters lcCharacterObjs) $ \(Character{..},(mat,(hMD3,hLC),(uMD3,uLC),(lMD3,lLC))) -> do
              {-
typedef struct {
	vec3_t		origin;
	vec3_t		axis[3];
} orientation_t;

void _VectorCopy( const vec3_t in, vec3_t out );
void _VectorMA( const vec3_t veca, float scale, const vec3_t vecb, vec3_t vecc );
  = vecc[i] = veca[i] + scale*vecb[i]; i={0,1,2}
void MatrixMultiply(float in1[3][3], float in2[3][3], float out[3][3]);

                -- entity, parent, parentModel, parent_tag_name
                CG_PositionRotatedEntityOnTag( &torso, &legs, ci->legsModel, "tag_torso");
                CG_PositionRotatedEntityOnTag( &head, &torso, ci->torsoModel, "tag_head");
              -}
              -- minBound, maxBound :: a
              -- fromEnum :: a -> Int
              -- toEnum :: Int -> a

              let bothAnim = [BOTH_DEATH1, BOTH_DEAD1, BOTH_DEATH2, BOTH_DEAD2, BOTH_DEATH3, BOTH_DEAD3]
                  anims = V.fromList $
                    [ (TORSO_GESTURE,LEGS_IDLE)
                    , (TORSO_ATTACK,LEGS_IDLE)
                    , (TORSO_ATTACK2,LEGS_IDLE)
                    , (TORSO_DROP,LEGS_IDLE)
                    , (TORSO_RAISE,LEGS_IDLE)
                    , (TORSO_STAND,LEGS_IDLE)
                    , (TORSO_STAND2,LEGS_IDLE)
                    , (TORSO_GETFLAG,LEGS_IDLE)
                    , (TORSO_GUARDBASE,LEGS_IDLE)
                    , (TORSO_PATROL,LEGS_IDLE)
                    , (TORSO_FOLLOWME,LEGS_IDLE)
                    , (TORSO_AFFIRMATIVE,LEGS_IDLE)
                    , (TORSO_NEGATIVE,LEGS_IDLE)

                    , (TORSO_STAND,LEGS_WALKCR)
                    , (TORSO_STAND,LEGS_WALK)
                    , (TORSO_STAND,LEGS_RUN)
                    , (TORSO_STAND,LEGS_BACK)
                    , (TORSO_STAND,LEGS_SWIM)

                    , (TORSO_STAND,LEGS_JUMP)
                    , (TORSO_STAND,LEGS_LAND)

                    , (TORSO_STAND,LEGS_JUMPB)
                    , (TORSO_STAND,LEGS_LANDB)

                    , (TORSO_STAND,LEGS_IDLE)
                    , (TORSO_STAND,LEGS_IDLECR)

                    , (TORSO_STAND,LEGS_TURN)


                    , (TORSO_STAND,LEGS_BACKCR)
                    , (TORSO_STAND,LEGS_BACKWALK)
                    ] ++ zip bothAnim bothAnim

              let t100 = floor $ time / 4
                  (torsoAnimType,legAnimType) = anims V.! (t100 `mod` V.length anims)

              -- torso = upper
              --  transform torso to legs
              --  transform head to torso (and legs)
              let t = floor $ time * 15
                  legAnim = animationMap HashMap.! legAnimType
                  legFrame = aFirstFrame legAnim + t `mod` aNumFrames legAnim
                  torsoAnim = animationMap HashMap.! torsoAnimType
                  torsoFrame = aFirstFrame torsoAnim + t `mod` aNumFrames torsoAnim

                  tagToMat4 MD3.Tag{..} = translateAfter4 tgOrigin (orthogonal . toOrthoUnsafe $ Mat3 tgAxisX tgAxisY tgAxisZ)
                  hMat = (tagToMat4 $ (MD3.mdTags uMD3 V.! torsoFrame) HashMap.! "tag_head") .*. uMat
                  uMat = (tagToMat4 $ (MD3.mdTags lMD3 V.! legFrame) HashMap.! "tag_torso")
                  lMat = one :: Proj4
                  lcMat m = mat4ToM44F . fromProjective $ m .*. rotationEuler (Vec3 (time/5) 0 0) .*. mat
                  p = trim . _4 $ fromProjective mat
                  setup m obj = do
                    GL.uniformM44F "worldMat" (GL.objectUniformSetter obj) $ lcMat m
                    cullObject obj p
              forM_ (md3instanceObject hLC) $ setup hMat
              forM_ (md3instanceObject uLC) $ setup uMat
              forM_ (md3instanceObject lLC) $ setup lMat
              --setMD3Frame hLC frame
              setMD3Frame uLC torsoFrame
              setMD3Frame lLC legFrame

            forM_ animTex $ \(animTime,texSetter,v) -> do
              let (_,i) = properFraction (time / animTime)
                  idx = floor $ i * fromIntegral (V.length v)
              texSetter $ v V.! idx
            GL.setScreenSize storage (fromIntegral w) (fromIntegral h)
            -- TODO
            let idmtx = GL.V4 (GL.V4 1 0 0 0) (GL.V4 0 1 0 0) (GL.V4 0 0 1 0) (GL.V4 0 0 0 1)
            -- ???: why is this needed?
            V.forM_ surfaceObjs $ \objs -> forM_ objs $ \obj -> GL.uniformM44F "worldMat" (GL.objectUniformSetter obj) idmtx
            -- case noBSPCull of
            --   True  -> V.forM_ surfaceObjs $ \objs -> forM_ objs $ \obj -> enableObject obj True
            --   False -> cullSurfaces bsp camPos frust surfaceObjs
            cullSurfaces bsp camPos frust surfaceObjs
            return ()

{-# LANGUAGE LambdaCase, ViewPatterns, RecordWildCards, TupleSections, PackageImports, OverloadedStrings #-}
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
import Foreign
import System.FilePath
import System.Directory

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import Data.Binary (encodeFile,decodeFile)
import qualified Data.ByteString.Lazy as LB
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as SB
import qualified Data.Set as Set
import qualified Data.HashSet as HashSet

import Text.Show.Pretty (ppShow)

import Codec.Picture

import LambdaCube.GL as GL
import LambdaCube.GL.Mesh

import GameEngine.Data.BSP
import GameEngine.Data.MD3
import GameEngine.Data.GameCharacter
import GameEngine.Data.Material hiding (Vec3,Entity)
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
  ( GLStorage
  , [(Proj4, MD3Instance)]
  , [Character]
  , [(Proj4, (MD3.MD3Model, MD3Instance), (MD3.MD3Model, MD3Instance),(MD3.MD3Model, MD3Instance))]
  , V.Vector [Object]
  , BSPLevel
  , MD3Instance
  , [(Float, SetterFun TextureData, V.Vector TextureData)]
  )

createMoodRenderInfo :: Map FilePath CommonAttrs -> HashSet FilePath -> HashSet FilePath -> (PipelineSchema, Map FilePath CommonAttrs)
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

  shMap = Map.fromList [mkShader True n | n <- HashSet.toList levelMaterials] `Map.union`
          Map.fromList [mkShader False n | n <- HashSet.toList modelMaterials]

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
    ObjectArraySchema Triangles $ Map.fromList
      [ ("color",       Attribute_V4F)
      , ("diffuseUV",   Attribute_V2F)
      , ("normal",      Attribute_V3F)
      , ("position",    Attribute_V3F)
      , ("lightmapUV",  Attribute_V2F)
      ]

  debugSlotSchema =
    ObjectArraySchema Triangles $ Map.fromList
      [ ("position",    Attribute_V3F)
      , ("color",       Attribute_V4F)
      ]

  inputSchema = {-TODO-}
    PipelineSchema
    { objectArrays = Map.fromList $ ("CollisionShape",debugSlotSchema) : zip ("LightMapOnly":"missing shader": Map.keys shMap) (repeat quake3SlotSchema)
    , uniforms = Map.fromList $ [ ("viewProj",      M44F)
                                , ("worldMat",      M44F)
                                , ("viewMat",       M44F)
                                , ("orientation",   M44F)
                                , ("viewOrigin",    V3F)
                                , ("entityRGB",     V3F)
                                , ("entityAlpha",   Float)
                                , ("identityLight", Float)
                                , ("time",          Float)
                                , ("LightMap",      FTexture2D)
                                , ("Noise",                FTexture2D)
                                , ("SinTable",             FTexture2D)
                                , ("SquareTable",          FTexture2D)
                                , ("SawToothTable",        FTexture2D)
                                , ("InverseSawToothTable", FTexture2D)
                                , ("TriangleTable",        FTexture2D)
                                , ("origin",    V3F)
                                -- Mood
                                , ("graphNode",     FTexture2D)
                                ] ++ zip textureUniforms (repeat FTexture2D)
    }

-- TODO
engineInit :: Map String Entry -> FilePath -> IO (PipelineSchema, EngineContent)
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
    print brushModelMapping
    print teleportData
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
  let cvtSurface :: MD3.Surface -> (Array,Array,V.Vector (Array,Array))
      cvtSurface MD3.Surface{..} =
        ( Array ArrWord32 (SV.length srTriangles) (withV srTriangles)
        , Array ArrFloat (2 * SV.length srTexCoords) (withV srTexCoords)
        , V.map cvtPosNorm srXyzNormal
        )
        where
          withV a f = SV.unsafeWith a (\p -> f $ castPtr p)
          cvtPosNorm (p,n) = (f p, f n) where f sv = Array ArrFloat (3 * SV.length sv) $ withV sv

      addSurface sf (il,tl,pl,nl,pnl) = (i:il,t:tl,p:pl,n:nl,pn:pnl) where
        (i,t,pn) = cvtSurface sf
        (p,n)    = V.head pn

      (il,tl,pl,nl,pnl) = addSurface surface ([],[],[],[],[])

  buffer <- compileBuffer (concat [il,tl,pl,nl])

  let surfaceData idx MD3.Surface{..} = (index,attributes) where
        index = IndexStream buffer idx 0 (SV.length srTriangles)
        countV = SV.length srTexCoords
        attributes = Map.fromList $
          [ ("diffuseUV",   Stream Attribute_V2F buffer (1 + idx) 0 countV)
          , ("position",    Stream Attribute_V3F buffer (2 + idx) 0 countV)
          , ("normal",      Stream Attribute_V3F buffer (3 + idx) 0 countV)
          , ("color",       ConstV4F (V4 1 1 1 1))
          , ("lightmapUV",  ConstV2F (V2 0 0))
          ]

      frames :: Data.Vector.Vector [(Int, Array)]
      frames = foldr addSurfaceFrames emptyFrame $ zip [0..] pnl where
        emptyFrame = V.empty -- V.replicate (V.length mdFrames) []
        addSurfaceFrames (idx,pn) f = V.zipWith (\l (p,n) -> (2 + idx,p):(3 + idx,n):l) f pn

  return $ GPUMD3S
    { gpumd3sBuffer    = buffer
    , gpumd3sStreams   = surfaceData 0 surface
    , gpumd3sFrames    = frames
    , gpumd3sShaders   = HashSet.fromList $ map (SB8.unpack . MD3.shName) $ V.toList srShaders
    , gpumd3sSurface   = surface
    , gpumd3sFrame     = frame
    }

data GPUMD3S
  = GPUMD3S
  { gpumd3sBuffer    :: Buffer
  , gpumd3sStreams   :: (IndexStream Buffer,Map String (Stream Buffer)) -- index stream, attribute streams
  , gpumd3sFrames    :: V.Vector [(Int,Array)]
  , gpumd3sShaders   :: HashSet String
  , gpumd3sSurface   :: MD3.Surface
  , gpumd3sFrame     :: MD3.Frame
  }

data MD3SInstance
  = MD3SInstance
  { md3sinstanceObject  :: [Object]
  , md3sinstanceBuffer  :: Buffer
  , md3sinstanceFrames  :: V.Vector [(Int,Array)]
  , md3sinstanceSurface :: MD3.Surface
  }

type MD3Skin = Map String String

addGPUMD3Surface :: GLStorage -> GPUMD3S -> MD3Skin -> [String] -> IO MD3SInstance
addGPUMD3Surface r GPUMD3S{..} skin unis = do
    let sf@MD3.Surface{..} = gpumd3sSurface
    let (index, attrs) = gpumd3sStreams
    objs <- do
        let materialName s = case Map.lookup (SB8.unpack $ srName) skin of
              Nothing -> SB8.unpack $ MD3.shName s
              Just a  -> a
        objList <- concat <$> forM (V.toList $ srShaders) (\s -> do
          a <- addObjectWithMaterial r (materialName s) TriangleList (Just index) attrs $ setNub $ "worldMat":unis
          b <- addObject r "LightMapOnly" TriangleList (Just index) attrs $ setNub $ "worldMat":unis
          return [a,b])

        -- add collision geometry
        -- XXX: note -- Frame used for collision geometry
        let Frame{..} = gpumd3sFrame
        collisionObjs <- do
            sphereObj <- uploadMeshToGPU (sphere (V4 1 0 0 1) 4 frRadius) >>= addMeshToObjectArray r "CollisionShape" (setNub $ ["worldMat","origin"] ++ unis)
            boxObj <- uploadMeshToGPU (bbox (V4 0 0 1 1) frMins frMaxs) >>= addMeshToObjectArray r "CollisionShape" (setNub $ ["worldMat","origin"] ++ unis)
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

addMD3Surface :: GLStorage -> MD3.Surface -> MD3.Frame -> MD3Skin -> [String] -> IO MD3SInstance
addMD3Surface r model frame skin unis = do
    gpuMD3 <- uploadMD3Surface model frame
    addGPUMD3Surface r gpuMD3 skin unis

setupStorage :: Map String Entry -> EngineContent -> GLStorage -> IO EngineGraphics
setupStorage pk3Data (bsp,md3Map,md3Objs,characterObjs,characters,shMapTexSlot,_,_,_,_) storage = do
    let slotU           = uniformSetter storage
        entityRGB       = uniformV3F "entityRGB" slotU
        entityAlpha     = uniformFloat "entityAlpha" slotU
        identityLight   = uniformFloat "identityLight" slotU
        worldMat        = uniformM44F "worldMat" slotU
        overbrightBits  = 0
        idmtx = V4 (V4 1 0 0 0) (V4 0 1 0 0) (V4 0 0 1 0) (V4 0 0 0 1)
    worldMat idmtx
    entityRGB $ V3 1 1 1
    entityAlpha 1
    identityLight $ 1 / (2 ^ overbrightBits)
    initTableTextures >>= setupTableTextures slotU

    -- default texture
    let redBitmap x y = let v = if (x+y) `mod` 2 == 0 then 255 else 0 in PixelRGB8 v v 0
    defaultTexture <- uploadTexture2DToGPU' False False False False $ ImageRGB8 $ generateImage redBitmap 2 2

    putStrLn "loading textures:"
    -- load textures
    animTex <- fmap concat $ forM (Set.toList $ Set.fromList $ concatMap (\(shName,sh) -> [(shName,saTexture sa,saTextureUniform sa,caNoMipMaps sh) | sa <- caStages sh]) $ Map.toList shMapTexSlot) $
      \(shName,stageTex,texSlotName,noMip) -> do
        let texSetter = uniformFTexture2D (SB.pack texSlotName) slotU
            setTex isClamped img = texSetter =<< loadQ3Texture (not noMip) isClamped defaultTexture pk3Data shName img
        case stageTex of
            ST_Map img          -> setTex False img >> return []
            ST_ClampMap img     -> setTex True img >> return []
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

    lcMD3Objs <- concat <$> forM md3Objs addMD3Obj

    chunk <- loadMD3 "./chunk.md3"
    let weapon_model = chunk -- (fromJust $ Map.lookup (SB.pack handWeapon) md3Map)
    lcMD3Weapon <- addMD3 storage weapon_model mempty ["worldMat","viewProj"]

    -- add characters
    lcCharacterObjs <- forM characterObjs
      (\[(mat,(hSkin,hName)),(_,(uSkin,uName)),(_,(lSkin,lName))] -> do
        let Just hMD3 = Map.lookup (SB.pack hName) md3Map
            Just uMD3 = Map.lookup (SB.pack uName) md3Map
            Just lMD3 = Map.lookup (SB.pack lName) md3Map
        hLC <- addMD3 storage hMD3 hSkin ["worldMat"]
        uLC <- addMD3 storage uMD3 uSkin ["worldMat"]
        lLC <- addMD3 storage lMD3 lSkin ["worldMat"]
        return (mat,(hMD3,hLC),(uMD3,uLC),(lMD3,lLC))
      )
    return (storage,lcMD3Objs,characters,lcCharacterObjs,surfaceObjs,bsp,lcMD3Weapon,animTex)

-- TODO
updateRenderInput :: EngineGraphics -> (Vec3, Vec3, Vec3) -> Int -> Int -> Float -> (Vec3, Vec3) -> IO ()
updateRenderInput (storage,lcMD3Objs,characters,lcCharacterObjs,surfaceObjs,bsp,lcMD3Weapon,animTex) (camPos,camTarget,camUp) w h time (Vec3 cvx cvy cvz, cvpos) = do
            let slotU = uniformSetter storage

            let matSetter   = uniformM44F "viewProj" slotU
                viewOrigin  = uniformV3F "viewOrigin" slotU
                orientation = uniformM44F "orientation" slotU
                viewMat     = uniformM44F "viewMat" slotU
                timeSetter  = uniformFloat "time" slotU

            let cm = fromProjective (lookat camPos camTarget camUp)                             -- camera orientation transform
                pm = perspective near far (fovDeg / 180 * pi) (fromIntegral w / fromIntegral h) -- perspective matrix
                sm = fromProjective (scaling $ Vec3 s s s)                                      -- scale matrix
                smcanvas = fromProjective (scaling $ Vec3 scanvas scanvas scanvas)              -- scale matrix
                s  = 0.005
                scanvas  = 0.1
                --V4 orientA orientB orientC _ = mat4ToM44F $! cm .*. sm
                Vec3 cx cy cz = camPos
                near = 0.00001/s
                far  = 100/s
                fovDeg = 60
                frust = frustum fovDeg (fromIntegral w / fromIntegral h) near far camPos camTarget camUp
                cullObject obj p = enableObject obj (pointInFrustum p frust)

            -- set uniforms
            timeSetter $ time / 1
            --putStrLn $ "time: " ++ show time ++ " " ++ show capturing
            viewOrigin $ V3 cx cy cz
            viewMat $ mat4ToM44F cm
            --orientation $ V4 orientA orientB orientC $ V4 0 0 0 1
            matSetter $! mat4ToM44F $! cm .*. sm .*. pm

            let invCM = mat4ToM44F $ idmtx -- inverse cm .*. (fromProjective $ translation (Vec3 0 (0) (-30)))
                --rot = fromProjective $ rotationEuler (Vec3 (-pi/2+30/pi*2) (pi/2) (-pi))
                -- rot = fromProjective $ orthogonal $ toOrthoUnsafe $ rotMatrixX (-pi/2) .*. rotMatrixY (pi/2) .*. rotMatrixX (10/pi*2)
                rot = fromProjective $ orthogonal $ toOrthoUnsafe $ rotMatrixZ cvz .*. rotMatrixY cvy .*. rotMatrixX cvx
            forM_ (md3instanceObject lcMD3Weapon) $ \obj -> do
              uniformM44F "viewProj" (objectUniformSetter obj) $ mat4ToM44F $! rot .*. (fromProjective $ translation cvpos) .*. smcanvas -- .*. pm
              uniformM44F "worldMat" (objectUniformSetter obj) invCM
            forM_ lcMD3Objs $ \(mat,lcmd3) -> do
              forM_ (md3instanceObject lcmd3) $ \obj -> do
                let m = mat4ToM44F $ fromProjective $ (rotationEuler (Vec3 time 0 0) .*. mat)
                    p = trim . _4 $ fromProjective mat
                uniformM44F "worldMat" (objectUniformSetter obj) m
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
                    uniformM44F "worldMat" (objectUniformSetter obj) $ lcMat m
                    cullObject obj p
              forM_ (md3instanceObject hLC) $ setup hMat
              forM_ (md3instanceObject uLC) $ setup uMat
              forM_ (md3instanceObject lLC) $ setup lMat
              --setMD3Frame hLC frame
              setMD3Frame uLC torsoFrame
              setMD3Frame lLC legFrame

            -- ???: what does this do?
            forM_ animTex $ \(animTime,texSetter,v) -> do
              let (_,i) = properFraction (time / animTime)
                  idx = floor $ i * fromIntegral (V.length v)
              texSetter $ v V.! idx
            setScreenSize storage (fromIntegral w) (fromIntegral h)
            -- TODO
            let idmtx = V4 (V4 1 0 0 0) (V4 0 1 0 0) (V4 0 0 1 0) (V4 0 0 0 1)
            -- ???: why is this needed?
            V.forM_ surfaceObjs $ \objs -> forM_ objs $ \obj -> uniformM44F "worldMat" (objectUniformSetter obj) idmtx
            -- case noBSPCull of
            --   True  -> V.forM_ surfaceObjs $ \objs -> forM_ objs $ \obj -> enableObject obj True
            --   False -> cullSurfaces bsp camPos frust surfaceObjs
            cullSurfaces bsp camPos frust surfaceObjs
            return ()

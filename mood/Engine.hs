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
  ) where

import Control.Monad
import Data.Char
import Data.List (isPrefixOf,partition,isInfixOf,elemIndex)
import Data.Maybe
import Data.Vect
import Data.Set (Set)
import Data.Map (Map)
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
import qualified Data.Vector as V

import Text.Show.Pretty (ppShow)

import Codec.Picture

import LambdaCube.GL as GL

import GameEngine.Data.BSP
import GameEngine.Data.GameCharacter
import GameEngine.Data.Material hiding (Vec3,Entity)
import GameEngine.Content
import GameEngine.Graphics.Culling
import GameEngine.Graphics.Frustum
import GameEngine.Graphics.Storage
import GameEngine.Graphics.BSP
import GameEngine.Graphics.MD3
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
        allShName = map shName $ V.toList $ blShaders bsp
        (selectedMaterials,ignoredMaterials) = partition (\n -> or $ [SB.isInfixOf k n | k <- ["floor","wall","door","trim","block"]]) allShName

    let levelMaterials = HashSet.fromList . Set.toList $ Set.map SB.unpack shNames
        modelMaterials = HashSet.fromList . Set.toList $ Set.map SB.unpack (md3Materials `Set.union` characterSkinMaterials)
        (inputSchema,shMapTexSlot) = createRenderInfo shMap levelMaterials modelMaterials
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

    lcMD3Weapon <- addMD3 storage (fromJust $ Map.lookup (SB.pack handWeapon) md3Map) mempty ["worldMat","viewProj"]

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
updateRenderInput :: EngineGraphics -> (Vec3, Vec3, Vec3) -> Int -> Int -> Float -> Bool -> IO ()
updateRenderInput (storage,lcMD3Objs,characters,lcCharacterObjs,surfaceObjs,bsp,lcMD3Weapon,animTex) (camPos,camTarget,camUp) w h time noBSPCull = do
            let slotU = uniformSetter storage

            let matSetter   = uniformM44F "viewProj" slotU
                viewOrigin  = uniformV3F "viewOrigin" slotU
                orientation = uniformM44F "orientation" slotU
                viewMat     = uniformM44F "viewMat" slotU
                timeSetter  = uniformFloat "time" slotU

            let cm = fromProjective (lookat camPos camTarget camUp)
                pm = perspective near far (fovDeg / 180 * pi) (fromIntegral w / fromIntegral h)
                sm = fromProjective (scaling $ Vec3 s s s)
                s  = 0.005
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
                rot = fromProjective $ orthogonal $ toOrthoUnsafe $ rotMatrixX (-pi/2) .*. rotMatrixY (pi/2) .*. rotMatrixX (10/pi*2)
            forM_ (md3instanceObject lcMD3Weapon) $ \obj -> do
              uniformM44F "viewProj" (objectUniformSetter obj) $ mat4ToM44F $! rot .*. (fromProjective $ translation (Vec3 3 (-10) (-5))) .*. sm .*. pm
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

            forM_ animTex $ \(animTime,texSetter,v) -> do
              let (_,i) = properFraction (time / animTime)
                  idx = floor $ i * fromIntegral (V.length v)
              texSetter $ v V.! idx
            setScreenSize storage (fromIntegral w) (fromIntegral h)
            -- TODO
            let idmtx = V4 (V4 1 0 0 0) (V4 0 1 0 0) (V4 0 0 1 0) (V4 0 0 0 1)
            V.forM_ surfaceObjs $ \objs -> forM_ objs $ \obj -> uniformM44F "worldMat" (objectUniformSetter obj) idmtx
            case noBSPCull of
              True  -> V.forM_ surfaceObjs $ \objs -> forM_ objs $ \obj -> enableObject obj True
              False -> cullSurfaces bsp camPos frust surfaceObjs
            return ()

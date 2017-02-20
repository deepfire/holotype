{-# LANGUAGE LambdaCase, ViewPatterns, RecordWildCards, TupleSections, PackageImports, OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll, FlexibleContexts, NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving, StandaloneDeriving #-}
{-# LANGUAGE DataKinds, GADTs, TypeFamilies, TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Engine
  ( loadPK3
  , createLoadingScreen
  , drawLoadingScreen
  , engineInit
  , setupStorage
  , updateRenderInput
  -- temp
  , loadQuake3Graphics
  , compileQuake3Graphics
  , compileQuake3GraphicsCached
  , getSpawnPoints
  , getBSP
  , getModelIndexFromBrushIndex
  , getTeleportFun
  , getMusicFile
  --
  , EngineContent, EngineGraphics
  ) where

import GHC.Stack
import GHC.TypeLits

import Control.Monad
import Control.Lens
import Data.Char
import Data.Digest.CRC32
import Data.HashSet (HashSet)
import qualified Data.ByteString.Char8 as SB8
import qualified Data.HashSet as HashSet
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import Data.List (delete, isPrefixOf, partition, isInfixOf, elemIndex)
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
import qualified Data.Set as Set
import qualified Data.HashSet as HashSet

import qualified Data.ByteString.Lazy as LB
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as SB

import Data.Binary (encodeFile,decodeFile)

import qualified Linear as L
import Numeric.Extra

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
import LambdaCube.Mesh
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

import Flatland
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
  , [Canvas]
  , [(Float, GL.SetterFun TextureData, V.Vector TextureData)]
  )

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
          Map.fromList [mkShader False n | n <- HashSet.toList modelMaterials]
          `Map.union`
          Map.fromList [(cvmaterial
                        ,defaultCommonAttrs
                         { caSort   = 10.0
                         , caStages = [defaultStageAttrs
                                        { saTexture     = ST_ClampMap cvmaterial
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
  debugSlotSchema =
    GL.ObjectArraySchema GL.Triangles $ Map.fromList
      [ ("position",    GL.Attribute_V3F)
      , ("color",       GL.Attribute_V4F)
      ]
  canvasSchema =
    GL.ObjectArraySchema GL.Triangles $ Map.fromList
      [ ("diffuseUV",   GL.Attribute_V2F)
      , ("normal",      GL.Attribute_V3F)
      , ("position",    GL.Attribute_V3F)
      ]
  quake3SlotSchema =
    GL.ObjectArraySchema GL.Triangles $ Map.fromList
      [ ("color",       GL.Attribute_V4F)
      , ("diffuseUV",   GL.Attribute_V2F)
      , ("normal",      GL.Attribute_V3F)
      , ("position",    GL.Attribute_V3F)
      , ("lightmapUV",  GL.Attribute_V2F)
      ]
  quakeObjectStreamNames = delete cvstream $ Map.keys shMap
  inputSchema = {-TODO-}
    GL.PipelineSchema
    { objectArrays = Map.fromList $
      ("CollisionShape",debugSlotSchema)
      : (cvstream, canvasSchema)
      : zip ("LightMapOnly":"missing shader": quakeObjectStreamNames) (repeat quake3SlotSchema)
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

    let cvUniform = SB.pack $ head $ concat $ concatMap (\(shName,sh) -> [if shName == cvmaterial then [saTextureUniform sa] else [] | sa <- caStages sh]) $ Map.toList shMapTexSlot
    cv0 <- renderCanvasInitial storage cvstream cvUniform
           (CanvasRequest (sGrowS 2 $ sGrowS 5 $ sGrowS 2 $ sGrowS 16 $ sArea $ fromIntegral . ceiling <$> di2goldX 256)
             "yayyity"
             (coGray 0.8 1) (coOpaq 0.1 0.1 0.5) (coGray 1 1) (coGray 0.5 1) (coGray 0.1 0.5) terminusFontDesc)
    cv1 <- renderCanvasInitial storage cvstream cvUniform
           (CanvasRequest (sGrowS 2 $ sGrowS 5 $ sGrowS 2 $ sGrowS 16 $ sArea $ fromIntegral . ceiling <$> di2goldX 256)
             "indeed, lollage"
             (coGray 0.8 1) (coOpaq 0.5 0.1 0.1) (coGray 1 1) (coGray 0.5 1) (coGray 0.1 0.5) terminusFontDesc)

    putStrLn "loading textures:"
    -- load textures
    animTex <- fmap concat $ forM (Set.toList $ Set.fromList $ concatMap (\(shName,sh) -> [(shName,saTexture sa,saTextureUniform sa,caNoMipMaps sh) | sa <- caStages sh]) $ Map.toList shMapTexSlot) $
      \(shName,stageTex,texSlotName,noMip) -> do -- texSlotName :: Approx "Tex_3913048198"
        let texSetter = GL.uniformFTexture2D (SB.pack texSlotName) slotU
            setTex isClamped img = if img == cvmaterial -- don't touch our canvas..
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
    return (storage,lcMD3Objs,characters,lcCharacterObjs,surfaceObjs,bsp,[cv0, cv1],animTex)

-- * To screen space conversion matrix.  From hell knows what, yes.
screenM :: Int -> Int -> Mat4
screenM w h =
  Mat4 (Vec4 (1/fw)  0     0 0)
       (Vec4  0     (1/fh) 0 0)
       (Vec4  0      0     1 0)
       (Vec4  0      0     0 0.5) -- where the f..k does that 0.5 factor COMEFROM?
  where (fw, fh) = (fromIntegral w, fromIntegral h)

-- TODO
updateRenderInput :: EngineGraphics
                  -> (Vec3, Vec3, Vec3)
                  -> Int -> Int -> Float -> (Vec3, Vec3) -> IO ()
updateRenderInput (storage, lcMD3Objs, characters, lcCharacterObjs, surfaceObjs, bsp, cvs, animTex)
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
                frust = GameEngine.Graphics.Frustum.frustum fovDeg (fromIntegral w / fromIntegral h) near far camPos camTarget camUp
                cullObject obj p = GL.enableObject obj (pointInFrustum p frust)

            let idM44F = mat4ToM44F $ idmtx -- inverse cm .*. (fromProjective $ translation (Vec3 0 (0) (-30)))
            let --cvrot = fromProjective $ orthogonal $ toOrthoUnsafe $ rotMatrixZ cvz .*. rotMatrixY cvy .*. rotMatrixX cvx
                cvrot = fromProjective $ orthogonal $ toOrthoUnsafe $ rotMatrixZ cvz .*. rotMatrixY 100 .*. rotMatrixX 100
                -- (vpw,vph) = (fromIntegral w/2.0, fromIntegral h/2.0)
                -- om    = Engine.ortho (doubleToFloat (-vpw)) (doubleToFloat vpw) (doubleToFloat (-vph)) (doubleToFloat vph) (-1) 1
                toScreen = screenM w h
                -- (fw, fh) = (fromIntegral w, fromIntegral h)
                -- aspectM44F = GL.V4 (GL.V4 (1/fw)  0     0 0)
                --                    (GL.V4  0     (1/fh) 0 0)
                --                    (GL.V4  0      0     1 0)
                --                    (GL.V4  0      0     0 0.5)
            -- uploadTexture2DToGPU'' False False cvTexture $ ImageRGBA8 $ generateImage gen 256 256
            -- updateUniforms storage $ do
            --   cvMatSlot @= return cvTexture
            GL.uniformM44F "viewProj" (GL.objectUniformSetter . cvGPU $ cvs !! 0) $
              mat4ToM44F $! toScreen .*. (fromProjective $! Data.Vect.translation $ cvpos &+ Vec3 0   0.3  0)
            GL.uniformM44F "viewProj" (GL.objectUniformSetter . cvGPU $ cvs !! 1) $
              mat4ToM44F $! toScreen .*. (fromProjective $! Data.Vect.translation $ cvpos &+ Vec3 0 (-0.3) 0)

            -- set uniforms
            timeSetter $ time / 1
            viewOrigin $ GL.V3 cx cy cz
            viewMat $ mat4ToM44F cm
            matSetter $! mat4ToM44F $! cm .*. sm .*. pm

            forM_ lcMD3Objs $ \(mat,lcmd3) -> do
              forM_ (md3instanceObject lcmd3) $ \obj -> do
                let m = mat4ToM44F $ fromProjective $ (rotationEuler (Vec3 time 0 0) .*. mat)
                    p = trim . Data.Vect._4 $ fromProjective mat
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
                  p = trim . Data.Vect._4 $ fromProjective mat
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

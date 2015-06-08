{-# LANGUAGE Arrows #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UnicodeSyntax #-}

import Prelude hiding ((.), id, null, filter)

import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Wire hiding (empty)
import Control.Wire.Session()

import Data.Maybe (fromMaybe)
import Data.Set (Set, empty, elems, insert, delete, null, filter, intersection, fromList)

import FRP.Netwire hiding (empty)

import Linear hiding (trace)
import Linear.Affine

import Text.Printf (printf)

import qualified SDL as SDL

import Debug.Trace (trace)
import System.Exit

screenWidth, screenHeight   ∷ _
(screenWidth, screenHeight) = (640, 480)

terminate ∷ Bool → String → IO a
terminate successp reason = do
  printf "%s: %s\n" (if successp then "Exiting" else "FATAL" ∷ String) reason
  SDL.quit
  exitWith (if successp then ExitSuccess else ExitFailure 1)

catchSDLFatally ∷ IO a -> IO a
catchSDLFatally = (flip catch) (\e -> terminate True $ show (e :: SDL.SDLException))

main :: IO ()
main = do
  SDL.initialize [SDL.InitEverything]
  win  ← SDL.createWindow
           "Mood"
           SDL.defaultWindow {SDL.windowSize = V2 screenWidth screenHeight }
  rend ← catchSDLFatally $
         SDL.createRenderer
            win
            (-1)
            (SDL.RendererConfig
                    { SDL.rendererAccelerated = False
                    , SDL.rendererSoftware = False
                    , SDL.rendererTargetTexture = False
                    , SDL.rendererPresentVSync = False
                    })
  SDL.showWindow win

  -- catchSDLFatally $ (simulator win rend (initialScene, Inputs empty) clockSession_ simulation)
  catchSDLFatally $ (stepper win rend ("<init>", Inputs empty) clockSession_ test)
  -- catchSDLFatally $ (stepper win rend ("", Inputs empty) (countSession_ 1) test)

  SDL.destroyRenderer rend
  SDL.destroyWindow win
  SDL.quit

elimFirst  ∷ SimWire (a, b) b
elimFirst = proc (_, x) → returnA -< x

elimSecond ∷ SimWire (a, b) a
elimSecond = proc (x, _) → returnA -< x

inhibitOnKey ∷ SDL.Scancode → SimWire Inputs Inputs
inhibitOnKey key = when (not . keyDown key)

produceOnKey ∷ SDL.Scancode → SimWire Inputs Inputs
produceOnKey key = when (keyDown key)

stepper ∷ SDL.Window → SDL.Renderer → (String, Inputs) → Session IO SimTime → SimWire (Inputs, String) (Bool, String) → IO ()
stepper win rend (preScene, preKeysDown) sesn wire = do
  keysDown <- parseEvents preKeysDown
  let wWire = (wire . (first $ inhibitOnKey SDL.ScancodeQ)
              --> pure (False, undefined))
  (nextScene, nextSesn, nextWire) ← do
    (st , sesn') ← stepSession sesn
    (ret, wire') ← stepWire wWire st $ Right (keysDown, "<start>")
    case ret of
      Left  iv              → error $ printf "stepWire: inhibited (got a %s)" iv
      Right (False, _)      → terminate True "user requested end of simulation"
      Right (True, scene')  → return (scene', sesn', wire')
  putStrLn $  nextScene
  threadDelay 100000
  stepper win rend (nextScene, keysDown) nextSesn nextWire

lol ∷ SimWire (a, Event (SimWire a a)) a
lol = rSwitch mkId

type PlugWire = SimWire (Inputs, String) (Inputs, String)

yay ∷ SDL.Scancode → SimWire (Inputs, a) ((Inputs, a), Event (SimWire (Inputs, a) (Inputs, a)))
yay key = mkId &&&
          (proc (inputs, x) → do
             ev ← now . pure mkEmpty . produceOnKey key  <|> never -< inputs
             returnA -< ev)

trigger :: SDL.Scancode → SimWire (Inputs, a) (Inputs, a)
trigger key =
    lol . yay key

mkLoop ∷ String → SimWire (Inputs, String) (Inputs, String)
mkLoop x = loo
    where loo = for 0.2  . second (pure $ "..." ++ x ++ "!") -->
                loo

-- Q: how do we turn a wire into an event?
-- A: now . pure
-- fmap (f ∷ a → b) (wire ∷ SimWire c a) ∷ SimWire c b

hmm ∷ SimWire Inputs (SimWire (Inputs, String) (Inputs, String))
hmm = arr (\(Inputs ks) →
               let kmap = [ (SDL.ScancodeY, mkLoop "Yay")
                          , (SDL.ScancodeL, mkLoop "Lol")
                          , (SDL.ScancodeW, mkLoop "Whew")]
                   ix   = intersection ks (fromList $ map fst kmap)
               in if null ix
                  then inhibit "<nothing>" -- mkId
                  else fromMaybe (mkLoop "canthappen") $
                       lookup (head $ elems ix) kmap)

addWireEvent ∷ SimWire (Inputs, String) ((Inputs, String), Event (SimWire (Inputs, String) (Inputs, String)))
addWireEvent = proc i@(inputs, _) → do
               ev ← ((now . hmm . when someKeyDown) <|> never) -< inputs
               returnA -< (i, ev)
 
-- switcheroo = rSwitch mkId .
--              (mkId &&&
--               (proc (inputs, x) → do
--                  ev ← now . when (not . someKeyDown)  <|> never -< inputs
--                  returnA -< ev))

test ∷ SimWire (Inputs, String) (Bool, String)
test = proc ins → do
         (_, x) ← loop -< ins
         returnA -< (True, x)
  where
    loop ∷ SimWire (Inputs, String) (Inputs, String)
    loop = rSwitch  (mkLoop "<rswitch0>") . addWireEvent -->
           for 0.2 . mkLoop "rSwitch inhibited " -->
           loop
    -- loop = for 2 . plug --> loop
    -- loop = for 2 .                 second "Once upon a time..." -->
    --        for 3 .                 second "... games were completely imperative..." -->
    --        trigger SDL.ScancodeF . second "... but then..." -->
    --        for 10 .                second ("Netwire 5! " <> anim) -->
    --        loop
    -- anim = proc s → do
   --          holdFor 1 . periodic 2 . "Hoo..." <|> "...ray!" -< s

integralWith' ::
    (Fractional a, HasTime t s)
    => (a -> (a, o))  -- Function for potentially limiting the integration
                      -- and producing a secondary output.
    -> a              -- Integration constant (aka start value).
    -> Wire s e m a (a, o)
integralWith' correct = loop'
  where
    loop' x' =
        mkPure $ \ds dx ->
            let dt = realToFrac (dtime ds)
                (x,b)  = correct (x' + dt*dx)
            in x' `seq` (Right (x', b), loop' x)


newtype Inputs = Inputs (Set SDL.Scancode)

parseEvents :: Inputs -> IO Inputs
parseEvents (Inputs keysDown) = do
  event ← SDL.pollEvent
  case event of
    Nothing
      → return $ Inputs keysDown
    Just e | (SDL.KeyboardEvent{..}) ← SDL.eventPayload e
      → parseEvents $ Inputs $ if keyboardEventKeyMotion == SDL.KeyDown
                               then (insert (SDL.keysymScancode keyboardEventKeysym) keysDown)
                               else (delete (SDL.keysymScancode keyboardEventKeysym) keysDown)
    _
      → parseEvents $ Inputs keysDown

someKeyDown ∷ Inputs → Bool
someKeyDown (Inputs ks) = not $ null ks

keyDown :: SDL.Scancode -> Inputs -> Bool
keyDown k (Inputs ks) = not $ null $ filter ((== k)) ks


class Renderable a where
    dim    ∷ a → V2 Double
    render ∷ SDL.Renderer → a → V2 Int → IO ()

drawBB ∷ ∀ a . Renderable a ⇒ SDL.Renderer → a → V2 Int → IO ()
drawBB rend rra pos =
  SDL.renderDrawRect rend $ SDL.Rectangle (P $ fmap fromIntegral pos) $ fmap floor $ dim rra

newtype ReBox = ReBox String
instance Renderable ReBox where
    dim    _ = V2 20.0 20.0
    render   = drawBB

data Scene where
    Arena ∷ Renderable a ⇒ Double → [a] → Scene

update_scene ∷ Scene → Double → Scene
update_scene scene x = case scene of
                         Arena _ xs → Arena x xs

initialScene ∷ Scene
initialScene = Arena 0 [ReBox "lol", ReBox "yay"]

render_scene ∷ SDL.Renderer → Scene → IO ()
render_scene rend scene = do

  SDL.setRenderDrawColor rend (V4 0 0 0 255)
  SDL.renderClear rend

  SDL.setRenderDrawColor rend (V4 255 255 255 255)
  case scene of
    Arena x rras → mapM_ (\(i, rra) → drawBB rend rra $ V2 (floor x + 50) $ 30 + i * 30) (zip [1..] rras) -- (\case x → drawBB rend x) We need the case to discharge the GADT

  SDL.renderPresent rend


type SimTime = (Timed NominalDiffTime ())
type SimWire a b = Wire SimTime String IO a b

simulator ∷ SDL.Window → SDL.Renderer → (Scene, Inputs) → Session IO SimTime → SimWire (Scene, Inputs) (Bool, Scene) → IO ()
simulator win rend (preScene, preKeysDown) sesn wire = do
  keysDown <- parseEvents preKeysDown
  (nextScene, nextSesn, nextWire) ← do
    (st , sesn') ← stepSession sesn
    (ret, wire') ← stepWire wire st $ Right (preScene, keysDown)
    case ret of
      Left  _              → error $ printf "stepWire: inhibited (got a Left)"
      Right (False, _)     → terminate True "user requested end of simulation"
      Right (True, scene') → return (scene', sesn', wire')
  render_scene rend nextScene
  simulator win rend (nextScene, keysDown) nextSesn nextWire

-- switch :: (Monad m, Monoid s) => Wire s e m a (b, Event (Wire s e m a b)) -> Wire s e m a b
simulation :: SimWire (Scene, Inputs) (Bool, Scene)
simulation = arena_sim . when (not . keyDown SDL.ScancodeQ . snd)
             --> pure (False, undefined)

-- input_events :: (MonadFix m, HasTime t s, Monoid e) => Wire s e m Inputs Double
-- input_events = (-20) . edge (keyDown SDL.ScancodeLeft) <&
--                  20  . edge (keyDown SDL.ScancodeRight)
-- spin :: (HasTime t s, Monad m) => Wire s e m a Double
-- spin = integral 0 . spinSpeed
--   where
--     spinSpeed =  5 . trigger SDL.ScancodeF -->
--                 -5 . trigger SDL.ScancodeF -->
--                 spinSpeed

arena_sim2 ∷ SimWire (Scene, Inputs) (Bool, Scene)
arena_sim2 = proc (scene, inputs) → do
               accel ← arrows_to_double -< inputs
               rec (x, coll) ← velocity_to_coords -< v
                   v ← velocity -< (accel, coll)
               returnA -< (True, update_scene scene x)

arena_sim ∷ SimWire (Scene, Inputs) (Bool, Scene)
arena_sim = proc (scene, inputs) → do
              accel ← arrows_to_double -< inputs
              rec (x, coll) ← velocity_to_coords -< v
                  v ← velocity -< (accel, coll)
              returnA -< (True, update_scene scene x)

velocity :: SimWire (Double, Bool) Double
velocity = integralWith bounce 0
    where bounce collisions v | collisions = -v
                              | otherwise  = v

velocity_to_coords :: SimWire Double (Double, Bool)
velocity_to_coords = integralWith' clamp 0
           where
             clamp p | p < 0 || p > 150 = (max 1 (min 149 p), True)
                     | otherwise        = (p, False)

arrows_to_double :: SimWire Inputs Double
arrows_to_double = (   (pure (-20)) . when (keyDown SDL.ScancodeLeft)
                   <|> (pure   20)  . when (keyDown SDL.ScancodeRight)
                   <|> (pure    0))
---
--- The following are equivalent, as per http://hub.darcs.net/ertes/netwire/browse/README.md
---
-- integral 5 . time
--
-- proc _ -> do
--   t <- time -< ()
--   integral 5 -< t

{-# LANGUAGE RecordWildCards #-}
module Camera where

import Control.Applicative
import Data.Vect
import qualified Data.Vector as V
import FRP.Elerea.Param

import GameEngine.Collision
import GameEngine.Data.BSP

userCamera :: ([Int] -> Vec3 -> Vec3) -> BSPLevel -> Vec3 -> Signal (Float, Float) -> Signal (Bool, Bool, Bool, Bool, Bool, Bool)
           -> SignalGen Float (Signal (Vec3, Vec3, Vec3, [Int]))
userCamera camTr bsp p mposs keyss = fmap (\(pos,target,up,i,_) -> (pos,target,up,i)) <$> transfer2 (p,zero,zero,[],(0,0,0)) calcCam mposs keyss
  where
    d0 = Vec4 0 (-1) 0 1
    u0 = Vec4 0 0 (-1) 1
    gravity = 1000
    jumpSpeed0 = 300
    height = 42
    calcCam dt (dmx,dmy) (left,up,down,right,turbo,jump) (p0,_,_,bIdx0,(mx,my,fallingSpeed)) =
      let nil c n = if c then n else zero
          p'  = nil left (v &* (-t)) &+ nil up (d &* t) &+ nil down (d &* (-t)) &+ nil right (v &* t) &+ p0
          k   = if turbo then 500 else 200
          t   = k * realToFrac dt
          mx' = dmx + mx
          my' = dmy + my
          rm  = fromProjective $ rotationEuler $ Vec3 (mx' / 100) (my' / 100) 0
          d   = trim $ rm *. d0 :: Vec3
          u   = trim $ rm *. u0 :: Vec3
          v   = normalize $ d &^ u
          jumpSpeed' = if jump then jumpSpeed0 else 0
          fallingVec = Vec3 0 0 (fallingSpeed * dt)
          p'2 = p' &+ fallingVec
          (p'3,bIdx'1) = case traceRay {-Sphere (height / 2.3)-} bsp p0 p'2 of
            Nothing -> (p'2,[])
            Just (hit,TraceHit{..}) -> (p0 &+ fallingVec,outputBrushIndex)
      in case traceRay bsp p'3 (p'3 &- Vec3 0 0 (height+1)) of
          Just (hit,TraceHit{..}) ->
                          let p'4 = camTr bIdx0 $ hit &+ Vec3 0 0 height
                          in (p'4,p'4 &+ d,u,outputBrushIndex ++ bIdx'1,(mx',my',jumpSpeed'))
          Nothing -> (p'3,p'3 &+ d,u,bIdx'1,(mx',my',fallingSpeed - dt*gravity + jumpSpeed'))

rotationEuler :: Vec3 -> Proj4
rotationEuler (Vec3 a b c) = orthogonal $ toOrthoUnsafe $ rotMatrixZ a .*. rotMatrixX b .*. rotMatrixY (-c)

recordSignalSamples :: Signal Bool -> Signal Bool -> Signal a -> SignalGen p (Signal [a])
recordSignalSamples = transfer3 [] record
  where
    record _ setWaypoint clearWaypoints input history
        | clearWaypoints = [] 
        | setWaypoint    = input:history 
        | otherwise      = history

playbackCamera :: Signal Bool -> Signal Bool -> Signal Float -> Signal [(Vec3, Vec3)] -> SignalGen Float (Signal (Maybe (Vec3, Vec3, Vec3)))
playbackCamera play stop speed recording = do
    let noPath = (V.empty, V.empty)
        trackPath _ play stop waypoints path 
            | stop      = noPath 
            | play      = mkPath waypoints 
            | otherwise = path
        mkPath waypoints = (camPath, targetPath)
          where
            waypoints' = reverse waypoints
            camPath = extendPath (V.fromList (map fst waypoints'))
            targetPath = extendPath (V.fromList (map snd waypoints'))
        
        stepCamera dtime (camPath, _targetPath) speed t
            | V.length camPath < 4 = 0
            | otherwise            = if t' > tmax - 0.05 then t' - tmax else t'
          where
            t' = proceedOnPath camPath 50 t (dtime * speed)
            tmax = fromIntegral (V.length camPath - 3)
    
    path <- transfer3 noPath trackPath play stop recording
    param <- transfer2 0 stepCamera path speed
    return $ do
        (camPath, targetPath) <- path
        t <- param
        return $ if V.length camPath < 4 then Nothing else Just (samplePath camPath t, samplePath targetPath t, Vec3 0 0 1)

extendPath :: V.Vector Vec3 -> V.Vector Vec3
extendPath ps = V.snoc (V.cons (2 *& ps V.! 0 &- ps V.! 1) ps) (2 *& ps V.! l &- ps V.! (l-1))
  where
    l = V.length ps - 1

proceedOnPath :: V.Vector Vec3 -> Int -> Float -> Float -> Float
proceedOnPath ps prec t d = go t (samplePath ps t) 0
  where
    tmax = fromIntegral (V.length ps - 3)
    go t p s
        | s > d     = t
        | t' > tmax = t
        | otherwise = go t' p' (s + len (p' &- p))
      where
        t' = t + d / (len grad * fromIntegral prec)
        p' = samplePath ps t'
        (i, f) = properFraction t
        grad = spline' (ps V.! i) (ps V.! (i+1)) (ps V.! (i+2)) (ps V.! (i+3)) f
{-    
    iterate step t !! prec
  where
    step t = t + d / (len s * fromIntegral prec)
      where
        (i, f) = properFraction t
        s = spline' (ps V.! i) (ps V.! (i+1)) (ps V.! (i+2)) (ps V.! (i+3)) f
-}

{-

 f(t0) = p0
 f(t0+t) = p0+d

 t = ?

 f'(t0) = p'0
 f(t0+1) ~= p0+p'0 
 f(t0+x) ~= p0+d/prec  ->  x = d/(prec*p'0)

-}

samplePath :: V.Vector Vec3 -> Float -> Vec3
samplePath ps t = spline (ps V.! i) (ps V.! (i+1)) (ps V.! (i+2)) (ps V.! (i+3)) f
  where
    (i, f) = properFraction t

spline :: Vec3 -> Vec3 -> Vec3 -> Vec3 -> Float -> Vec3
spline p0 p1 p2 p3 t = 0.5 *& (2*&p1 &+ (p2 &- p0) &* t &+ (2*&p0 &- 5*&p1 &+ 4*&p2 &- p3) &* t^2 &+ (neg p0 &+ 3*&p1 &- 3*&p2 &+ p3) &* t^3)

spline' :: Vec3 -> Vec3 -> Vec3 -> Vec3 -> Float -> Vec3
spline' p0 p1 p2 p3 t = 0.5 *& (p2 &- p0 &+ (4*&p0 &- 10*&p1 &+ 8*&p2 &- 2*&p3) &* t &+ ((-3)*&p0 &+ 9*&p1 &- 8*&p2 &+ 3*&p3) &* t^2)

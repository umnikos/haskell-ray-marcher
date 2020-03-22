module Ray
  ( Ray
  , rayMarch
  ) where

import Vector
import Color
import Scene

type Ray = (Vec3, Vec3) -- Ray origin, direction

epsilon :: Double
epsilon = 0.0001

rayMarch :: Scene -> Double -> Ray -> Maybe Color
rayMarch s end (pos,dir) -- End is the render distance (how far to march before giving up)
    | end <= 0 = Nothing
    | dist < 0.0001 = Just $ Vec3 1 0 0 -- Test Color-TODO
    | otherwise = rayMarch s (end-dist) (pos + dist `scale` dir, dir) -- Each time lowering the distance to the end with the distance we traveled.
    where   (dist, _) = s pos

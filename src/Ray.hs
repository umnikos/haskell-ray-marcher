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


getXComponents :: Int -> [Double]
getXComponents width = take width [-1, -1 + 2 / fromIntegral width .. ] -- Generates equally spaced floats from -1 to 1.

widthCoords :: [Double]
widthCoords = getXComponents 1024 -- Width of the image

getYComponents :: Int -> [Double]
getYComponents height = take height [-1, -1 + 2 / fromIntegral height .. ]

heightCoords :: [Double]
heightCoords = getYComponents 512 -- Height of the image

getRays :: Double -> [[Ray]]
getRays fov = map computeRow heightCoords
    where   z = (tan (fov / 2))
            computeRow y = [ (Vec3 0 0 0, normalize (Vec3 x (-y) z) ) | x <- widthCoords ] -- First Ray has coordinates [-1,-(-1)].

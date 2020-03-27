module Ray
  ( Ray
  , rayMarch
  , getRays
  ) where

import Vector
import Color
import Scene
import ImageSettings

type Ray = (Vec3, Vec3) -- Ray origin, direction

epsilon :: Double
epsilon = 0.0001

rayMarch :: Scene -> Double -> Ray -> Maybe Color
rayMarch s end (pos,dir) -- End is the render distance (how far to march before giving up)
    | end <= 0 = Nothing
    | dist < 0.0001 = Just $ color -- Returns just the color of the scene, no shading.
    | otherwise = rayMarch s (end-dist) (pos + dist `scale` dir, dir) -- Each time lowering the distance to the end with the distance we traveled.
    where   (dist, (color, _, _)) = s pos

getRays :: ImageSettings -> [[Ray]]
getRays setting = [[ (Vec3 0 0 0, normalize (Vec3 x (-y) z) ) -- First Ray has coordinates [-1,-(-1)].
                   | x <- widthCoords setting ]
                   | y <- heightCoords setting ]
    where z = (tan (pi - getFieldOfView setting / 2))
          widthCoords setting = spacedPoints $ getImageWidth setting
          heightCoords setting = spacedPoints $ getImageHeight setting

-- | Generates N doubles from -1 to 1, equally spaced.
spacedPoints :: Int -> [Double]
spacedPoints n = f <$> fromIntegral <$> [0..n-1]
  where f x = (-1) + x*d
        d = 2/(fromIntegral n - 1)

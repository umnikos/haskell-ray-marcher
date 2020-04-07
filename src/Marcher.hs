{-|
A ray macher implemented in haskell.
-}

{-# LANGUAGE BangPatterns #-}

module Marcher
  ( Vec3(..)
  , dot
  , scale
  , mag
  , normalize
  , Color
  , red
  , green
  , blue
  , Ray
  , rayMarch
  , getRays
  , Radius
  , Position
  , Scene
  , Material
  , sphere
  , mergeScenes
  , ImageSettings (..)
  , writePPM
  , clamp
  , scene
  ) where

import Codec.Image.PPM ( ColorArray, ppm_p6 )
import System.IO ( withBinaryFile, hPutStr, IOMode(WriteMode) )

data Vec3 = Vec3 !Double !Double !Double deriving (Show,Eq)

instance Num Vec3 where
    (Vec3 x y z) + (Vec3 x1 y1 z1) = Vec3 (x + x1) (y + y1) (z + z1)
    negate (Vec3 x y z) = Vec3 (-x) (-y) (-z)
    x - y = x + negate y
    abs (Vec3 x y z) = Vec3 (abs x) (abs y) (abs z)
    (Vec3 x y z) * (Vec3 x1 y1 z1) = Vec3 (x * x1) (y * y1) (z * z1)
    fromInteger x = let y = fromInteger x in Vec3 y y y
    signum (Vec3 x y z) = Vec3 (signum x) (signum y) (signum z)


-- | The dot product of two vectors
(Vec3 x y z) `dot` (Vec3 a s d) = x*a + y*s + z*d
-- | Scale a vector's magnitude by a number.
a `scale` (Vec3 x y z) = Vec3 (a*x) (a*y) (a*z)

-- | (See 'mag').
squared_mag :: Vec3 -> Double
squared_mag v3@(Vec3 x y z) = (x * x + y * y + z * z)

-- | Returns the magnitude (i.e. the length) of a vector.
mag :: Vec3 -> Double
mag v = sqrt (squared_mag v)

-- | Scale a vector so that it will have a magnitude of 1
normalize :: Vec3 -> Vec3
normalize (Vec3 0 0 0) = error "Cannot normalize a vector with magnitude 0"
normalize v = ( 1 / mag v) `scale` v

------------------------------------------------------------

type Color = Vec3

-- | Sets the color of an entire scene to some color.
colorize :: Color -> Scene -> Scene
colorize c s pt =
  let (d, (_,p,g)) = s pt -- Evaluating the scene
  in (d, ( c,p,g )) -- Adding the color to the scene.

red, green, blue :: Scene -> Scene
-- | Colorize red
red = colorize (Vec3 1 0 0)
-- | Colorize green
green = colorize (Vec3 0 1 0)
-- | Colorize blue
blue = colorize (Vec3 0 0 1)

------------------------------------------------------------

type Ray = (Vec3 -- Ray origin
           ,Vec3) -- Ray direction

-- | Marches a ray through a scene. Returns Nothing if it goes outside the scene.
rayMarch :: Scene -> ImageSettings -> Ray -> Maybe Color
rayMarch s sett (pos,dir)
    | end <= 0 = Nothing
    | dist < epsilon = Just $ color -- Returns just the color of the scene, no shading.
    | otherwise = rayMarch s sett{getRenderDistance=end-dist} (pos + dist `scale` dir, dir) -- Each time lowering the distance to the end with the distance we traveled.
    where   (dist, (color, _, _)) = s pos
            end = getRenderDistance sett
            epsilon = getTolerance sett

-- | Produces an array of rays to later be marched.
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

------------------------------------------------------------

type Radius = Double
type Position = Vec3 -- ^ A position in 3D space
type Scene = Position -> (Radius, Material)
type Material = (Vec3 -- Color
                ,Double -- Specular lighting
                ,Double) -- Gloss (defines how "soft"/"hard" the reflection is)

-- | Defines a sphere at a given position and with a given radius.
sphere :: Position -> Radius -> Scene
sphere pos r = \pt -> (mag (pos-pt) - r, (Vec3 1 1 1, 20, 0.5))

-- | Combines two scenes into a single scene.
mergeScenes :: Scene -> Scene -> Scene
mergeScenes scene1 scene2 pt
    | d1 < d2 = res1 -- Picks the minimum distance and the corresponding Material
    | otherwise = res2
    where   res1@(d1, _) = scene1 pt
            res2@(d2, _) = scene2 pt

------------------------------------------------------------

-- | A data type holding all of the rendering settings. This is everything needed to create a rendering, excluding the scene itself.
data ImageSettings = ImageSettings
 { getImageWidth :: Int -- ^ In pixels.
 , getImageHeight :: Int -- ^ In pixels.
 , getFieldOfView :: Double -- ^ In radians.
 , getRenderDistance :: Double -- ^ How far to march before giving up.
 , getTolerance :: Double -- ^ How close to an object to get before counting the ray as hitting that object.
 }

-- | Clamps a color and formats it for ppm outputting.
clamp :: (Integral a, Integral b, Integral c)
      => Color
      -> (a, b, c) -- ^ Returns RGB clamped triple
clamp (Vec3 r g b) = (clampFloat r, clampFloat g, clampFloat b)
    where clampFloat f = max 0 (min 255 (round ( 255 * f )))

-- | Writes a ColorArray to a file.
writePPM :: FilePath
         -> [ColorArray] -- ^ ColorArray is the Codec.Image.PPM.Color representation of a color (triple of Integers)
         -> IO ()
writePPM fileName img = do
    let imgData = ppm_p6 img -- Returns String version of the ColorArray.
    withBinaryFile fileName WriteMode (\h -> hPutStr h imgData) -- Opens a file in Binary mode and writes the imgData String in it.

------------------------------------------------------------

-- | An example scene.
scene :: Scene
scene = red $ sphere (Vec3 0 0 (-3)) 1

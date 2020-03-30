module ImageSettings
  ( ImageSettings (..)
  , writePPM
  , clamp
  ) where

import Codec.Image.PPM ( ColorArray, ppm_p6 )
import System.IO ( withBinaryFile, hPutStr, IOMode(WriteMode) )
import Vector
import Color

-- | A data type holding all of the rendering settings. This is everything needed to create a rendering, excluding the scene itself.
data ImageSettings = ImageSettings
 { getImageWidth :: Int
 , getImageHeight :: Int
 , getFieldOfView :: Double
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

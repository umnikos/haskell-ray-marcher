module ImageSettings
  ( ImageSettings (..)
  , writePPM
  , clamp
  ) where

import Codec.Image.PPM ( ColorArray, ppm_p6 )
import System.IO ( withBinaryFile, hPutStr, IOMode(WriteMode) )
import Vector
import Color

data ImageSettings = ImageSettings
 { getImageWidth :: Int
 , getImageHeight :: Int
 , getFieldOfView :: Double
 }

clamp :: (Integral a, Integral b, Integral c) => Vec3 -> (a, b, c) -- Returns RGB clamped triple
clamp (Vec3 r g b) = (clampFloat r, clampFloat g, clampFloat b)
    where clampFloat f = max 0 (min 255 (round ( 255 * f )))

writePPM :: FilePath -> [ColorArray] -> IO () -- ColorArray is the Codec.Image.PPM.Color representation of a color (triple of Integers)
writePPM fileName img = do
    let imgData = ppm_p6 img -- Returns String version of the ColorArray.
    withBinaryFile fileName WriteMode (\h -> hPutStr h imgData) -- Opens a file in Binary mode and writes the imgData String in it.

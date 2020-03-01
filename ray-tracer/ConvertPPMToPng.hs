module ConvertPPMToPng (
  convertPPMToPng
) where

import Graphics.Image

type PPMImage = (Image VS RGB Word8)

convertPPMToPng :: IO ()
convertPPMToPng = do
   img  <- readImageExact' PPM "test.ppm" :: IO(PPMImage)
   writeImageExact PNG [] "test.png" img
   return ()

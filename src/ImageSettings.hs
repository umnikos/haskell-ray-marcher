module ImageSettings
  ( ImageSettings (..)
  ) where


data ImageSettings = ImageSettings
 { getImageWidth :: Int
 , getImageHeight :: Int
 , getFieldOfView :: Double
 }

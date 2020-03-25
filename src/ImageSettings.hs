module ImageSettings
  ( ImageSettings
    , imageWidth
    , imageHeight
    , fieldOfView
  ) where


data ImageSettings = ImageSettings
 { imageWidth :: Int
 , imageHeight :: Int
 , fieldOfView :: Double
 }

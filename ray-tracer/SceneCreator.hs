{-# LANGUAGE DeriveGeneric #-}

module SceneCreator(
  BShape (..),
  encode
) where

import  Data.Binary hiding (encode)
import  GHC.Generics (Generic)
import  Vector3
import  Ray
import  Shapes
import  Color

data BShape = BSphere Point3 Radius (Color, Float, Float)
           | BPlane Normal Float (Color, Float, Float)
           deriving (Generic)

instance Binary BShape

flatred = (red, 0.0, 1.0)
shinyred = (red, 0.3, 0.9)
semishinygreen = (green, 0.5, 0.7)
shinywhite = (white, 0.3, 0.9)

shapes :: [BShape]

shapes = [ BPlane (0,(-1),0) 50 shinyred,
     BSphere (50,10,100) 40 semishinygreen,
     BSphere (350,10,100) 40 semishinygreen,
     BSphere (-80,0,80) 50 flatred]

encode :: IO ()
encode = do
  encodeFile "scene" shapes

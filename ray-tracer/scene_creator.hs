{-# LANGUAGE DeriveGeneric #-}

import           Data.Binary
import           GHC.Generics (Generic)

type Vector3 = (Float, Float, Float)
type Point3 = Vector3
type Direction3 = Vector3
type Time = Float
type Ray = (Point3, Direction3) -- base and direction
type Normal = Vector3
type Radius = Float
type Color = (Float, Float, Float)

data BShape = BSphere Point3 Radius (Color, Float, Float)
           | BPlane Normal Float (Color, Float, Float)
           deriving (Generic)

instance Binary BShape

red, green, blue, white, black, midgrey, nearlywhite :: Color
red     = (1.0, 0.0, 0.0)
green   = (0.0, 1.0, 0.0)
blue    = (0.0, 0.0, 1.0)
white   = (1.0, 1.0, 1.0)
black   = (0.0, 0.0, 0.0)
midgrey = (0.5, 0.5, 0.5)
nearlywhite = (0.8,0.8,0.8)

flatred = (red, 0.0, 1.0)
shinyred = (red, 0.3, 0.9)
semishinygreen = (green, 0.5, 0.7)
shinywhite = (white, 0.3, 0.9)

shapes = [ BPlane (0,(-1),0) 50 shinyred,
     BSphere (50,10,100) 40 semishinygreen,
     BSphere (350,10,100) 40 semishinygreen,
     BSphere (-80,0,80) 50 flatred]

main = do
  encodeFile "scene" shapes

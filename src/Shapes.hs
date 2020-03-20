module Shapes
  ( Shape (..)

) where

import Linear
import Linear.Affine

-- Point is just a newtype constructor for convenience. To differentiate between a point and a vector.

data Shape = Plane (Point V3 Double) (V3 Double)  -- Point and normal
           | Sphere (Point V3 Double) Double -- Point and radius
           | Triangle (Point V3 Double) (Point V3 Double) (Point V3 Double) (V3 Double) -- 3 points and a normal
           deriving (Show)

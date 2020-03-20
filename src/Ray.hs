module Ray
  ( Ray (..)
  , rayIntersection
) where

import Linear
import Linear.Affine
import Shapes

data Ray = Ray { rayOrigin :: Point V3 Double -- Point origin
               , rayDirection :: V3 Double -- Direction
               }
         deriving (Show)


data Intersection = Intersection { intersectionPoint :: Point V3 Double -- Point of intersection
                                 , intersectionNormal :: V3 Double -- Normal at that point
                                 , tMin :: Double -- Minimal distance from the origin to the current intersection point.
                                 }
                  deriving (Show)


rayIntersection :: Ray -> Shape -> Maybe Intersection

rayIntersection (Ray {rayOrigin = ro, rayDirection = rd}) (Plane planePoint planeNormal) =
  undefined

rayIntersection (Ray {rayOrigin = ro, rayDirection = rd}) (Sphere sphereCenter sphereRadius) =
  undefined

rayIntersection (Ray {rayOrigin = ro, rayDirection = rd}) (Triangle (P v0) (P v1) (P v2) n) =
  undefined

module Ray
  ( Ray
  , rayMarch
  ) where

import Vector
import Color
import Scene

type Ray = (Vec3, Vec3) -- Ray origin, direction

rayMarch :: Scene -> Position -> Ray -> Maybe Color
rayMarch = undefined

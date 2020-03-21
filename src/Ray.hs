module Ray(
  Ray(..)

)
where

import Vector
import Color
import Scene

type Ray = (Vec3, Vec3) -- Ray origin, direction

rayMarch :: Scene -> Double -> Ray -> Maybe Color -- Takes a scene, the position(checks for reaching out of the scene/no hit,) and the ray.
rayMarch = undefined

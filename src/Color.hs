module Color(
  Color(..)
  , colorize
)
where

import Vector
import Scene

type Color = Vec3

colorize :: Color -> Scene -> Scene
colorize = undefined

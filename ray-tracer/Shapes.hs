module Shapes(
  Shape (..),
  Normal,
  Radius
) where

import Vector3
import Ray
import Material

type Normal = Vector3
type Radius = Float

data Shape = Sphere Point3 Radius (Point3 -> Material)
           | Plane Normal Float (Point3 -> Material)

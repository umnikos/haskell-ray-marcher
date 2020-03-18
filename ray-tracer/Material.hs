module Material(
  Material,
  flatred, shinyred, semishinygreen, shinywhite,
  checked_matt_f
) where

import Ray
import Color
import Vector3

type Reflectivity = Float
type Diffuseness = Float
type Material = (Color, Reflectivity, Diffuseness)

flatred, shinyred, semishinygreen, shinywhite :: Material
flatred = (red, 0.0, 1.0)
shinyred = (red, 0.3, 0.9)
semishinygreen = (green, 0.5, 0.7)
shinywhite = (white, 0.3, 0.9)

checked_matt_f :: Point3 -> Material
checked_matt_f (x,y,z) = let xeven = even (truncate (x / 20.0))
                             yeven = even (truncate (y / 20.0))
                             zeven = even (truncate (z / 20.0))
                         in if (xeven `xor` yeven `xor` zeven) then (white, 0.0, 1.0) else (black, 0.0, 1.0)

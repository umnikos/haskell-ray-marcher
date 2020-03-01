module Material(
  Material,
  flatred, shinyred, semishinygreen, shinywhite,
  checked_matt
) where

import Ray
import Color
import Vector3

type Reflectivity = Float
type Diffuseness = Float
type Material = (Color, Reflectivity, Diffuseness)

flatred, shinyred, semishinygreen, shinywhite :: Point3 -> Material
flatred _ = (red, 0.0, 1.0)
shinyred _ = (red, 0.3, 0.9)
semishinygreen _ = (green, 0.5, 0.7)
shinywhite _ = (white, 0.3, 0.9)

checked_matt :: Point3 -> Material
checked_matt (x,y,z) = let xeven = even (truncate (x / 20.0))
                           yeven = even (truncate (y / 20.0))
                           zeven = even (truncate (z / 20.0))
                       in if (xeven `xor` yeven `xor` zeven) then (white, 0.0, 1.0) else (black, 0.0, 1.0)

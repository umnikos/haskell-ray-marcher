module Color
  ( Color
  , red
  , green
  , blue
  ) where

import Vector
import Scene

type Color = Vec3

colorize :: Color -> Scene -> Scene 
colorize c s pt =
  let (d, (_,p,g)) = s pt -- Evaluating the scene
  in (d, ( c,p,g )) -- Adding the color to the scene.

-- Example Colors
red, green, blue :: Scene -> Scene
red = colorize (Vec3 1 0 0)
green = colorize (Vec3 0 1 0)
blue = colorize (Vec3 0 0 1)

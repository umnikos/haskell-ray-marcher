module Color
  ( Color
  , red
  , green
  , blue
  ) where

import Vector
import Scene

type Color = Vec3

-- | Sets the color of an entire scene to some color.
colorize :: Color -> Scene -> Scene
colorize c s pt =
  let (d, (_,p,g)) = s pt -- Evaluating the scene
  in (d, ( c,p,g )) -- Adding the color to the scene.

red, green, blue :: Scene -> Scene
-- | Colorize red
red = colorize (Vec3 1 0 0)
-- | Colorize green
green = colorize (Vec3 0 1 0)
-- | Colorize blue
blue = colorize (Vec3 0 0 1)

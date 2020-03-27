module Color
  ( Color
  , red
  , green
  , blue
  ) where

import Vector

type Color = Vec3

colorize :: a1 -> (p -> (a2, (a3, b, c))) -> p -> (a2, (a1, b, c)) -- This polymophic monstrosity is just two Scenes, trust me.
colorize c s pt =
  let (d, (_,p,g)) = s pt -- Evaluating the scene
  in (d, ( c,p,g )) -- Adding the color to the scene.

-- Example Colors
red :: (p -> (a1, (a2, b, c))) -> p -> (a1, (Vec3, b, c))
red = colorize (Vec3 1 0 0)
green = colorize (Vec3 0 1 0)
blue = colorize (Vec3 0 0 1)

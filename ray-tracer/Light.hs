module Light(
  Light (..),
  background_color,
  ambient_light,
  lights
) where

import Vector3
import Ray
import Color

data Light = Directional Vector3 Color
           | Spotlight Point3 Color

background_color :: Color
background_color = black

ambient_light :: Color
ambient_light = (0.1,0.1,0.1)

lights :: [Light]
lights = [ Spotlight (100,-30,0) nearlywhite,
           Spotlight (-100,-100,150) nearlywhite]

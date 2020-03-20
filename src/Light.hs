module Light
  ( Light (..)

) where

import Color
import Linear
import Linear.Affine

data Light = EnvironmentLight (Color Double) -- Pure environment light
           | PointLight (Point V3 Double) (Color Double) -- Point light
           deriving (Show)

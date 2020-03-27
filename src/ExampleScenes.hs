module ExampleScenes
  ( scene
  ) where

import Scene
import Color
import Vector

scene :: Scene
scene = red $ sphere (Vec3 0 0 (-3)) 1

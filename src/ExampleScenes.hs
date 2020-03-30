module ExampleScenes
  ( scene
  ) where

import Scene
import Color
import Vector

-- | An example scene.
scene :: Scene
scene = red $ sphere (Vec3 0 0 (-3)) 1

module Camera
  ( Camera (..)

  ) where

import Linear
import Linear.Affine

data Camera = Camera (Point V3 Double) (V3 Double) (V3 Double) -- Camera origin, looking-at vector, and normal of the camera
              deriving (Show)

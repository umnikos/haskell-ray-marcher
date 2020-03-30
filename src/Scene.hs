module Scene
  ( Radius
  , Position
  , Scene
  , Material
  , sphere
  , mergeScenes
  ) where

import Vector

type Radius = Double
type Position = Vec3 -- ^ A position in 3D space
type Scene = Position -> (Radius, Material)
type Material = (Vec3 -- Color
                ,Double -- Specular lighting
                ,Double) -- Gloss (defines how "soft"/"hard" the reflection is)

-- | Defines a sphere at a given position and with a given radius.
sphere :: Position -> Radius -> Scene
sphere pos r = \pt -> (mag (pos-pt) - r, (Vec3 1 1 1, 20, 0.5))

-- | Combines two scenes into a single scene.
mergeScenes :: Scene -> Scene -> Scene
mergeScenes scene1 scene2 pt
    | d1 < d2 = res1 -- Picks the minimum distance and the corresponding Material
    | otherwise = res2
    where   res1@(d1, _) = scene1 pt
            res2@(d2, _) = scene2 pt

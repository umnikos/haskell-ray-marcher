module Scene
  ( Radius
  , Position
  , Scene
  , Material
  , sphere
  , mergeScenes
  ) where

import Vector
import Data.List ( foldl' )

type Radius = Double
type Position = Vec3
type Scene = Position -> (Radius, Material)
type Material = (Vec3, Double, Double) -- Color, Specular lighting and Gloss(defines how "soft"/"hard" the reflection is)

sphere :: Position -> Radius -> Scene
sphere pos r = \pt -> (mag (pos-pt) - r, (Vec3 1 1 1, 20, 0.5))

mergeScenes :: Scene -> Scene -> Scene
mergeScenes scene1 scene2 pt
    | d1 < d2 = res1 -- Picks the minimum distance and the corresponding Material
    | otherwise = res2
    where   res1@(d1, _) = scene1 pt
            res2@(d2, _) = scene2 pt

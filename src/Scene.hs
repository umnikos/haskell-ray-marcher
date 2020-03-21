module Scene(
  Scene(..)

)
where

import Vector

type Scene = Vec3 -> (Double, Material)
type Material = (Vec3, Double, Double) -- Color, Specular lighting and Gloss(defines how "soft"/"hard" the reflection is)
type Position = Vec3

type Radius = Double

sphere :: Position -> Radius -> (Double, Material)
sphere pt r = (mag pt - r, (Vec3 1 1 1, 20, 0.5)) -- Example sphere

mergeScenes :: Scene -> Scene -> Scene -- This will later be used with foldl probably, to generate the final scene.
mergeScenes scene1 scene2 pt
    | d1 < d2 = res1 -- Picks the minimum distance and the corresponding Material
    | otherwise = res2
    where   res1@(d1, _) = scene1 pt
            res2@(d2, _) = scene2 pt

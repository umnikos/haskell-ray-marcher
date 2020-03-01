module Rendering(
  overall_lighting,
  raytrace,
  render_to_pgm,
  reflected_ray
) where

import Vector3
import Ray
import Material
import Intersection
import Light
import Color
import Scene
import View
import Lighting

render_to_pgm :: Float -> Float -> String
render_to_pgm width height = let view = ( (0,0,-100), 100, (0,0,100), (0,-1,0))
                                 projection = perspective_projection view
                                 ray_collection = map projection (pixel_grid view width height)
                                 color_collection = map (raytrace 0) ray_collection
                             in make_pgm (round width) (round height) color_collection

reflected_ray :: Integer -> Intersection -> Color
reflected_ray depth (normal, hitpoint,(_,in_ray_dir),(color,kr,_))
   | kr == 0.0 = black
   | otherwise = let k = 2 * ((normalize normal) `dot` (normalize (neg in_ray_dir)))
                     out_ray_dir = (scalarmult (normalize normal) k) `sub` (neg in_ray_dir)
                     reflected_col = raytrace (depth + 1) (hitpoint, out_ray_dir)
                 in scalarmult reflected_col kr

overall_lighting :: Integer -> Intersection -> Color
overall_lighting depth hit = let sum_colors = foldr add_col black
                                 local_lighting = ambient_light `add_col` sum_colors (map (local_light hit) lights)
                                 global_lighting = if (depth < 2) then (reflected_ray depth hit) else black
                             in clamp (local_lighting `add_col` global_lighting)

raytrace :: Integer -> Ray -> Color
raytrace depth ray = let hits = concat (map (intersect ray) shapes)
                     in if (null hits) then background_color
                        else overall_lighting depth (closest hits)

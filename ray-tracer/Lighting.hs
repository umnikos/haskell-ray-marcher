module Lighting (
  point_is_lit,
  diffuse_coeff,
  local_light,
) where

import Vector3
import Ray
import Material
import Intersection
import Light
import Color
import Scene

point_is_lit :: Point3 -> Point3 -> Bool
point_is_lit point lightpos = let path = lightpos `sub` point
                                  time_at_light = mag path
                                  ray = (point, normalize path)
                                  hits = concat (map (intersect ray) shapes)
                                  times = fst (unzip hits)
                              in if (null times) then True else (minimum times) > time_at_light

diffuse_coeff :: Vector3 -> Vector3 -> Float
diffuse_coeff light_dir normal = max 0.0 (negate ((normalize light_dir) `dot` (normalize normal)))


local_light :: Intersection -> Light -> Color
local_light (normal,_,_,(materialcol,_,kd)) (Directional dir lightcol) =
   let mixed_color = combine_col materialcol lightcol
       diffuse = scale_col mixed_color ((diffuse_coeff dir normal) * kd)
   in diffuse

local_light (normal, hitpoint,_,(materialcol,_,kd)) (Spotlight lightpos lightcol) =
   let mixed_color = combine_col materialcol lightcol
       diffuse = scale_col mixed_color (kd * (diffuse_coeff (hitpoint `sub` lightpos) normal))
   in if (point_is_lit hitpoint lightpos) then diffuse else black

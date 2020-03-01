module Intersection(
  Intersection,
  intersect,
  closest
) where

import Vector3
import Ray
import Shapes
import Material

type Intersection = (Normal, Point3, Ray, Material)

epsilon :: Float
epsilon = 0.001

intersect :: Ray -> Shape -> [(Time, Intersection)]

intersect ray@(base, dir) (Sphere center rad materialfn) =
    let a = squared_mag dir
        b = 2 * ( dir `dot` (base `sub` center))
        c = (squared_mag (base `sub` center)) - rad^2
        times = filter (> epsilon) (roots a b c)
        normal_at_time t = normalize ((position_at_time ray t) `sub` center)
        intersection_at_time t = (normal_at_time t, position_at_time ray t, ray, materialfn (position_at_time ray t))
    in map (\t -> (t,intersection_at_time t)) times

intersect ray@(base, dir) (Plane normal d materialfn) =
    let vd = (normalize normal) `dot` dir
        v0 = negate (((normalize normal) `dot` base) + d)
    in if (vd == 0) then []
       else let t = v0 / vd
                hitpoint = position_at_time ray t
            in if t > epsilon then [ (t, (if (vd > 0) then (neg normal) else normal, hitpoint, ray, materialfn hitpoint)) ]
                              else []

closest :: [ (Time,Intersection) ] -> Intersection
closest xs = let select_nearest (t1,i1) (t2,i2) = if (t1<t2) then (t1,i1) else (t2,i2)
             in snd (foldl select_nearest (head xs) (tail xs))

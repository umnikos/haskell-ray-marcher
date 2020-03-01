module View(
  View,
  pixel_grid,
  parallel_projection,
  perspective_projection,
  make_pgm
) where

import Vector3
import Ray
import Material
import Intersection
import Light
import Color
import Scene

type View = (Point3, Float, Point3, Vector3)

pixel_grid :: View -> Float -> Float -> [ Point3 ]
pixel_grid (camerapos, viewdist, lookingat, viewup) width height =
   let grid = [ (x, y, 0) | y <- [0..width-1], x <- [0..height-1] ]
       centering_offset = (- width / 2.0, -height / 2.0 , 0)
       pixel_offsets = map (add centering_offset) grid
       viewdir = normalize (lookingat `sub` camerapos)
       screen_center = camerapos `add` (scalarmult viewdir viewdist)
       viewright = viewdir `cross` viewup
       transform (x,y,_) = screen_center `add` (scalarmult viewright x) `add` (scalarmult (neg viewup) y)
   in map transform pixel_offsets

parallel_projection :: View -> Point3 -> Ray
parallel_projection (camerapos,_,lookingat,_) point  = (point, normalize (lookingat `sub` camerapos))

perspective_projection :: View -> Point3 -> Ray
perspective_projection (camerapos,_,_,_) point = (point, normalize (point `sub` camerapos))

make_pgm :: Integer -> Integer -> [ Color ] -> String
make_pgm width height xs = "P3\n" ++ show width ++ " " ++ show height ++ "\n255\n" ++ stringify(xs)
      where stringify [] = ""
            stringify ((r,g,b):xs) = show (round (r*255)) ++ " "
             ++ show (round (g*255)) ++ " "
             ++ show (round (b*255)) ++ " "
             ++ stringify xs

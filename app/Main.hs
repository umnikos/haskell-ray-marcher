module Main where

import Marcher

myScene :: Scene
myScene = scene

setting :: ImageSettings
setting = ImageSettings 512 512 (pi/2) 100 0.0001

main :: IO ()
main = do
  let rays = getRays setting
  let colors = map (map (maybe (0,0,0) clamp . rayMarch myScene setting)) rays -- 2D list of triples(RGB Integer values).
  writePPM "rayMarch.ppm" colors

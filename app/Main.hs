module Main where

import Marcher

myScene :: Scene
myScene = defaultScene

setting :: ImageSettings
setting = defaultSettings

main :: IO ()
main = do
  let rays = getRays setting
  let colors = map (map (clamp . rayRender setting myScene)) rays -- 2D list of triples(RGB Integer values).
  writePPM "rayMarch.ppm" colors

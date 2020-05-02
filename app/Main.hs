module Main where

import Marcher
import Marcher.Scenes

myScene :: Scene
myScene = cubeDemo

setting :: ImageSettings
setting = defaultSettings

main :: IO ()
main = do
  let rays = getRays setting
  let colors = map (map (colorToRGB . rayRender setting myScene)) rays -- 2D list of triples(RGB Integer values).
  writePPM "rayMarch.ppm" colors

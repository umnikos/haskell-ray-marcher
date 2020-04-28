{-|
A module containing examples of scenes you can render with this ray marcher.
-}
module Marcher.Scenes
  ( shadowsDemo
  , infiniteSpheresDemo
  ) where

import Marcher
import Data.Fixed

-- | A scene with two spheres, one casting a shadow on the other one.
shadowsDemo :: Scene
shadowsDemo = mergeScenes
                  (colorize red $ sphere (Vec3 (0, 0, (-3))) 1)
                  (colorize blue $ sphere (Vec3 (1, 1, (-2))) 0.1)

-- | A scene with an infinite grid of spheres.
infiniteSpheresDemo :: Scene
infiniteSpheresDemo = moveScene (Vec3 (-2.5,-2.5,-3.5)) $ sphere (Vec3 (2.5, 2.5, 2.5)) 0.1 . pointModulus
  where pointModulus (Vec3 (a,b,c)) = Vec3 (a `mod'` 5, b `mod'` 5, c `mod'` 5)




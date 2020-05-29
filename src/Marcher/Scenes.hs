{-|
A module containing examples of scenes you can render with this ray marcher.
-}
module Marcher.Scenes
  ( shadowsDemo
  , infiniteSpheresDemo
  , cubeDemo
  , shapeDemo
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
infiniteSpheresDemo = moveScene (Vec3 (-2.5,0,-1))
                    $ rotateX (pi/3)
                    $ moveScene (Vec3 (0,-2.5,-2.5))
                    $ sphere (Vec3 (2.5, 2.5, 2.5)) 0.3 . pointModulus
  where pointModulus (Vec3 (a,b,c)) = Vec3 (a `mod'` 5, b `mod'` 5, c `mod'` 5)

-- | A scene with just a single cube.
cubeDemo :: Scene
cubeDemo = moveScene (Vec3 (0,0,-3))
         $ rotateX (-pi/12)
         $ rotateY (pi/6)
         $ cube (Vec3 (0,0,0)) 1

-- | A scene with a weirder shape. Currently rendered poorly due to the output being only 2 colors.
shapeDemo :: Scene
shapeDemo = moveScene (Vec3 (0,0,-3))
          $ rotateX (-pi/12)
          $ rotateY (pi/6)
          $ sceneAnd
             (cube (Vec3 (0,0,0)) 1)
             (sphere (Vec3 (0,0,0)) 1.5)


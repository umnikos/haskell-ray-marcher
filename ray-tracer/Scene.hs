module Scene (
  shapes
) where

import Data.Binary (decodeFile)
import System.IO.Unsafe (unsafePerformIO)
import SceneCreator
import Shapes

bshapeToShape (BSphere p r c) = Sphere p r (\_->c)
bshapeToShape (BPlane n f c)  = Plane n f (\_->c)

shapes :: [Shape]
shapes = unsafePerformIO $ do
  bscene <- decodeFile "scene"
  return $ map bshapeToShape bscene

module MarcherSpec.MarcherSpec where

import Test.Hspec
import Marcher

samplePoint :: Position
samplePoint = Vec3 (1,2,3)

sampleColor :: Color
sampleColor = blue

sampleScene :: Scene
sampleScene = sphere (Vec3 (0, 0, (-3))) 1

spec :: Spec
spec = do
  describe "Function colorize" $ do
    it "makes a scene have a particular color" $ do
      color `shouldBe` blue
      where (_, (color, _, _)) = colorize sampleColor sampleScene samplePoint

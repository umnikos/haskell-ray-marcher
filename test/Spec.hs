import Test.Hspec

import Marcher

samplePoint :: Position
samplePoint = Vec3 (0,0,0) -- Origin

simpleScene :: Scene
simpleScene = sphere (Vec3 (0, 0, (-3))) 1

complexScene :: Scene
complexScene = mergeScenes
                  (colorize red $ sphere (Vec3 (0, 0, (-3))) 1)
                  (colorize blue $ sphere (Vec3 (1, 1, (-2))) 0.1)

main :: IO ()
main = hspec $ do
  describe "Haskell Ray Marcher" $ do
    describe "Function colorize" $ do
      it "makes a scene blue" $ do
        let (_, Material color _ _) = colorize blue simpleScene samplePoint
        color `shouldBe` blue

      it "makes a scene red" $ do
        let (_, Material color _ _) = colorize red simpleScene samplePoint
        color `shouldBe` red

    describe "Function mixColors" $ do
      it "mixes two colors additively" $ do
        mixColors black white `shouldBe` gray -- black and white makes grey

    describe "Function colorToRGB" $ do
      it "prepares a color for output, making it in the range 0 - 255" $ do
        colorToRGB red `shouldBe` (255, 0, 0)

    describe "Function spacedPoints" $ do
      it " Generates N doubles from -1 to 1, equally spaced" $ do
        let points = [-1.0,-0.5,0.0,0.5,1.0]
        spacedPoints 5 `shouldBe` points

    describe "Scene, consisting of one sphere" $ do
      it "Returns the closest distance to a sphere" $ do
        let mySphere = sphere (Vec3 (0, 0, (-3))) 1
            myPosition = samplePoint
            (dist, _) = mySphere myPosition
        dist `shouldBe` 2 -- Z is -3, so if the radius is 1, that means the left distance to the origin is 2

    describe "Function rayRender (complete rendering of a given point)" $ do
      it "Renders a point on the surface of a red sphere" $ do
        let ray = (Vec3(0, 0, 0), Vec3(0, 0, -1)) -- Red sphere in on (Vec3 (0, 0, (-3))
        rayRender defaultSettings complexScene ray `shouldBe` red

      it "Casts a shadow on the surface of a red sphere" $ do
        let ray = (Vec3(0, 0, 0), Vec3(-0.2, -0.2, -1)) -- This is inside the shadow, given the sun position
        rayRender defaultSettings complexScene ray `shouldBe` gray * red

    describe "Function mergeScenes" $ do
      it "returns the closest scene from our view" $ do
        let scene1 = sphere (Vec3 (0, 0, (-3))) 1 -- The distance to this scene is 2
            scene2 = sphere (Vec3 (0, 0, (-5))) 1 -- The distance to this scene is 4
            resultScene = mergeScenes scene1 scene2 samplePoint
        resultScene `shouldBe` scene1 samplePoint -- Tests whether the result of evaluating the resultScene is equal to evaluating the closer scene

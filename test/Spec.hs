import Test.Hspec

import Marcher
import Control.Monad

class MyEq a where
  (~=) :: a -> a -> Expectation

epsilon :: Double
epsilon = 0.001

instance MyEq Vec3 where
    (Vec3 (x, y, z)) ~= (Vec3 (x1, y1, z1)) =
      unless (equalWithinError epsilon x x1 && equalWithinError epsilon y y1 && equalWithinError epsilon z z1 ) .
        expectationFailure $ "expected: " ++ show (Vec3 (x, y, z)) ++ "\n but got: " ++ show (Vec3 (x1, y1, z1))

instance ((Show a), (Ord a), (Num a), (Fractional a)) => MyEq [a] where
  xs ~= ys =
    unless (testForLists 0.001 xs ys) .
      expectationFailure $ "expected:" ++ show (xs) ++ "\n but got: " ++ show (ys)
    where testForLists = \eps xs ys -> foldr (&&) True $ zipWith (\x y -> equalWithinError eps x y) xs ys :: Bool

instance MyEq Double where
  x ~= y =
    unless (equalWithinError epsilon x y) .
      expectationFailure $ "expected:" ++ show (x) ++ "\n but got: " ++ show (y)


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
        color ~= blue

      it "makes a scene red" $ do
        let (_, Material color _ _) = colorize red simpleScene samplePoint
        color ~= red

    describe "Function mixColors" $ do
      it "mixes black and white" $ do
        mixColors black white ~= gray

      it "mixes red and black" $ do
        mixColors red black ~= darkRed

      it "mixes red and green" $ do
        mixColors red green ~= darkYellow

      it "mixes red and white" $ do
        mixColors red white ~= pink

    describe "Function colorToRGB" $ do
      it "prepares a color for output, making it in the range 0 - 255" $ do
        colorToRGB red `shouldBe` (255, 0, 0)

    describe "Function spacedPoints" $ do
      it " Generates N doubles from -1 to 1, equally spaced" $ do
        let points = [-1.0,-0.5,0.0,0.5,1.0]
        spacedPoints 5 ~= points

    describe "Scene, consisting of one sphere" $ do
      it "Returns the closest distance to a sphere" $ do
        let mySphere = sphere (Vec3 (0, 0, (-3))) 1
            myPosition = samplePoint
            (dist, _) = mySphere myPosition
        dist ~= 2 -- Z is -3, so if the radius is 1, that means the left distance to the origin is 2

    describe "Function mergeScenes" $ do
      it "returns the closest scene from our view" $ do
        let scene1 = sphere (Vec3 (0, 0, (-3))) 1 -- The distance to this scene is 2
            scene2 = sphere (Vec3 (0, 0, (-5))) 1 -- The distance to this scene is 4
            resultScene = mergeScenes scene1 scene2 samplePoint
        resultScene `shouldBe` scene1 samplePoint -- Tests whether the result of evaluating the resultScene is equal to evaluating the closer scene

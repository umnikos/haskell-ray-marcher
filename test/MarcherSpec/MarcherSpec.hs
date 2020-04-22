module MarcherSpec.MarcherSpec (spec) where

import Test.Hspec
import Marcher

spec :: Spec
spec = do
  describe "Some spec" $ do
    it "tests" $ do
      head [1 ..] `shouldBe` 1

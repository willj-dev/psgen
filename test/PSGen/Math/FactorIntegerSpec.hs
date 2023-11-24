module PSGen.Math.FactorIntegerSpec (main, spec) where

import Test.Hspec

import PSGen.Math.FactorInteger

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "factorPairs" $ do
    it "handles a positive prime" $ do
        factorPairs 3 `shouldBe` [(1, 3)]
    it "handles a positive composite" $ do
        factorPairs 6 `shouldBe` [(1, 6), (2, 3)]
    it "handles a positive square" $ do
        factorPairs 4 `shouldBe` [(1, 4), (2, 2)]
    it "handles a negative prime" $ do
        factorPairs (-3) `shouldBe` [(-3, 1), (-1, 3)]
    it "handles a negative composite" $ do
        factorPairs (-6) `shouldBe` [(-6, 1), (-3, 2), (-2, 3), (-1, 6)]
    it "handles a negative square" $ do
        factorPairs (-4) `shouldBe` [(-4, 1), (-2, 2), (-1, 4)]

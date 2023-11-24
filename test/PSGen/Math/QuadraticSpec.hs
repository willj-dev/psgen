module PSGen.Math.QuadraticSpec (main, spec) where

import Test.Hspec

import PSGen.Math.Quadratic

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "quadraticFormula" $ do
    it "handles a unique solution" $ do
        quadraticFormula (1, 4, 4) `shouldBe` QuadraticSolutionUnique (-2)
    it "handles two rational solutions" $ do
        quadraticFormula (1, 10, -39) `shouldBe` QuadraticSolutionPair (-5) 8 1
    it "handles two irrational solutions" $ do
        quadraticFormula (1, -4, 2) `shouldBe` QuadraticSolutionPair 2 1 2
    it "handles two complex solutions" $ do
        quadraticFormula (1, -4, 6) `shouldBe` QuadraticSolutionPair 2 1 (-2)

module PSGen.Math.RadicalSpec (main, spec) where

import Test.Hspec

import Data.Ratio ((%))
import PSGen.Math.Radical

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "reduceIntegerRadical" $ do
    it "handles a perfect square" $ do
        reduceIntegerRadical 16 `shouldBe` (4, 1)
    it "handles a positive integer" $ do
        reduceIntegerRadical 48 `shouldBe` (4, 3)
    it "handles a negative integer" $ do
        reduceIntegerRadical (-48) `shouldBe` (4, -3)
  describe "reduceRationalRadical" $ do
    it "handles a perfect square" $ do
        reduceRationalRadical 16 `shouldBe` (4, 1)
    it "handles a positive integer" $ do
        reduceRationalRadical 48 `shouldBe` (4, 3)
    it "handles a negative integer" $ do
        reduceRationalRadical (-48) `shouldBe` (4, -3)
    it "handles a ratio of perfect squares" $ do
        reduceRationalRadical (16 % 9) `shouldBe` (4 % 3, 1)
    it "handles a positive integer over a perfect square" $ do
        reduceRationalRadical (48 % 9) `shouldBe` (4 % 3, 3)
    it "handles a perfect square over a positive integer" $ do
        reduceRationalRadical (9 % 48) `shouldBe` (1 % 4, 3)
    it "handles a negative ratio" $ do
        reduceRationalRadical (-9 % 48) `shouldBe` (1 % 4, -3)

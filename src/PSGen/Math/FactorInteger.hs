module PSGen.Math.FactorInteger (factorPairs) where

import Data.Tuple (swap)

{- |
    factorPairs x factors the integer x into pairs of integer factors (a, b) where a <= b and the first element
    of each pair is increasing from left to right. If x == 0, the result is an empty list. If x < 0, it generates
    a list twice as long as that of |x| for the convenience of things like factoring quadratics where c < 0.

    factorPairs 3       = [(1, 3)]
    factorPairs (-3)    = [(-3, 1), (-1, 3)]
 -}
factorPairs :: Int -> [(Int, Int)]
factorPairs 0 = []

factorPairs x | x > 0 = foldr go [] [1 .. x `div` 2]
    where
        go f ps | x `rem` f /= 0 = ps
        go f ps | any (\a -> f == fst a || f == snd a) ps = ps
        go f ps = let f' = x `div` f in (min f f', max f f') : ps

-- negatives
factorPairs x = let
    pairsPos = factorPairs (abs x)
    negFst (a, b) = (-1 * a, b)
    -- in the left half, filter out any equal pairs (e.g. (2, 2) for 4) since they already occur in the right half
    in negFst <$> (swap <$> filter (uncurry (/=)) pairsPos) ++ reverse pairsPos

module PSGen.Math.Radical (reduceIntegerRadical, reduceRationalRadical) where

import Data.Foldable (find)
import Data.Ratio (numerator, denominator, (%))

{- |
    sqrt(n) -> a * sqrt(b) for the largest possible value of a. n is allowed to be negative, in which case b will also be negative.

    reduceRadical 0 = (0, 0)
    reduceRadical 1 = (1, 1)
    reduceRadical (-1) = (1, -1)
-}
reduceIntegerRadical :: Integer -> (Integer, Integer)
reduceIntegerRadical n | abs n < 2 = (n, signum 1)
reduceIntegerRadical n =
    case firstRoot of
        Nothing -> (1, n)
        Just r -> let
            n' = n `div` (r * r)
            (a', b') = reduceIntegerRadical n'
            in (r * a', b')
    where
        -- bunch of type hints needed because number types are a mess
        nf = fromInteger (abs n) :: Float
        maxSqrt = (floor . sqrt $ nf) :: Integer
        possibleSquareRoots = [ maxSqrt, maxSqrt - 1 .. 2 ] -- try bigger roots first to save on recursion

        firstRoot = find (\r -> n `rem` (r * r) == 0) possibleSquareRoots

-- | sqrt(n / d) -> a * sqrt(b), where a is a positive rational and b is an integer.
reduceRationalRadical :: Rational -> (Rational, Integer)
reduceRationalRadical r = ((na * a') % (da * db), b')
    where
        (na, nb) = reduceIntegerRadical (numerator r)
        (da, db) = reduceIntegerRadical (denominator r)

        (a', b') = reduceIntegerRadical (nb * db)

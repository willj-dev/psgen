{- |
    Quadratic equations of the form ax^2 + bx + c = 0, where a, b, and c are all integers.
 -}
module PSGen.Math.Quadratic where

import Data.Ratio ((%))
import PSGen.Math.Radical (reduceIntegerRadical)

-- | (a, b, c) in ax^2 + bx + c = 0
type QuadraticEquation = (Integer, Integer, Integer)

data FactoredQuadratic
    = FactoredQuadDistinct Integer Integer
    | FactoredQuadDiffSquare Integer
    | FactoredQuadSquare Integer

data QuadraticSolution
    -- | a unique rational solution
    = QuadraticSolutionUnique Rational

    -- | a pair of solutions of the form a \pm b \sqrt{c} where a, b are rational and c is an integer.
    | QuadraticSolutionPair Rational Rational Integer
    deriving (Eq, Show)


quadraticFormula :: QuadraticEquation -> QuadraticSolution
quadraticFormula (a, b, c) =
    if discriminant == 0
        then QuadraticSolutionUnique rationalPart
    else
        QuadraticSolutionPair rationalPart (rc % (2 * a)) rr
    where
        discriminant = b * b - 4 * a * c
        rationalPart = (-1 * b) % (2 * a)

        -- sqrt(det) reduced to rc * sqrt(rr)
        (rc, rr) = reduceIntegerRadical discriminant

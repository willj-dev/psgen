
module PSGen.Problems.Quadratic where

import PSGen.Problem
import PSGen.Math.Quadratic

instance Problem FactoredQuadratic where
    problemClass _ = Computation

    problemType (FactoredQuadDistinct _ _) = "quadratic.factor.distinct"
    problemType (FactoredQuadDiffSquare _) = "quadratic.factor.diff-squares"
    problemType (FactoredQuadSquare _) = "quadratic.factor.square"



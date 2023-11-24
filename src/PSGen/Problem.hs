module PSGen.Problem (ProblemClass(..), Problem(..)) where

{- |
    Distinguishes between computation problems (like "Factor x^2 + 10x - 39") or word problems.
    Point being, computation problems are easier to group and have different spacing requirements compared
    to word problems.
-}   
data ProblemClass = Computation | WordProblem deriving (Eq, Show)

class Problem a where
    -- | Is this a computation or word problem?
    problemClass :: a -> ProblemClass

    -- | A unique name for this type of problem, like "quadratic.factoring.two-distinct-integer-factors"
    problemType :: a -> String

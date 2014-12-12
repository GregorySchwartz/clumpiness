-- Types module
-- By Gregory W. Schwartz

{- | Collects all types used in the program
-}

module Math.Clumpiness.Types where

-- Algebraic
data Metric = Clumpiness
            | Diversity Double
            | MeanDiversity Double
            | Mesh deriving (Read, Show, Eq)

-- Advanced
type ClumpList a = [(a, a, Double)]

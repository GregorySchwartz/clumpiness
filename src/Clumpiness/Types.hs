-- Types module
-- By Gregory W. Schwartz

-- | Collects all types used in the program

module Clumpiness.Types where

-- Algebraic
data Metric = Clumpiness | Diversity | Mesh deriving (Read, Show, Eq)

-- Advanced
type ClumpList a = [(a, a, Double)]

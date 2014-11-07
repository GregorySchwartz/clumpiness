-- Types module
-- By Gregory W. Schwartz

-- | Collects all types used in the program

module Types where

-- Basic
type Label a     = a

-- Advanced
type LabelList a = [Label a]
type ClumpList a = [(Label a, Label a, Double)]

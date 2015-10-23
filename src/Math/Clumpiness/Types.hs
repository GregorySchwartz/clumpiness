-- Types module
-- By Gregory W. Schwartz

{- | Collects all types used in the program
-}

module Math.Clumpiness.Types where

-- Built-in
import qualified Data.Sequence as Seq

-- Algebraic
data Metric = Clumpiness
            | ClumpinessEvens
            | ClumpinessMult
            | Diversity Double
            | MeanDiversity Double
            deriving (Read, Show, Eq)

data Pinpoint a b = Pinpoint { pinpointLabel      :: a
                             , pinpointClumpiness :: Seq.Seq (b, b, Double)
                             , pinpointLeaves     :: Seq.Seq a
                             }

-- Advanced
type ClumpList a = [(a, a, Double)]

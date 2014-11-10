-- clumpiness
-- By Gregory W. Schwartz

{-# LANGUAGE BangPatterns #-}

module Clumpiness where

-- Built-in
import Data.List
import Data.Tree
import Control.Applicative
import Data.Ratio

-- Cabal
import Tree

-- Local
import Types

-- | Get the geometric average of a list
geomAvg :: [Double] -> Double
geomAvg xs = (product xs) ** (1 / (genericLength xs))

-- | Weigh the nodes by distance from the root
weigh :: Double -> Double
weigh x = 1 / (2 ** x)

-- | Only look at these labels, if they aren't both in the list (or if p1 == p2
-- and the length is 1), then ignore it
relevantList :: (Eq a) => Label a -> Label a -> [(Label a, Int)] -> [Int]
relevantList p1 p2 l
    | (p1 `elem` (map fst relevantNodes) && p2 `elem` (map fst relevantNodes))
   && (length relevantNodes > 1)               = map snd relevantNodes
    | otherwise                                = []
  where
    relevantNodes = filter (\x -> (fst x) `elem` [p1, p2]) l

-- | Get the clumpiness metric (before sample size correction)
getClumpiness :: (Eq a) => Label a -> Label a -> Tree a -> Double
getClumpiness _ _ (Node { subForest = [] }) = 0
getClumpiness p1 p2 n@(Node { subForest = xs })
    = sum $ clump : (map (getClumpiness p1 p2) xs)
  where
    clump = sum
          . map weigh
          . map fromIntegral
          . relevantList p1 p2
          . leavesHeight 0
          $ n

-- | Get the heatmap for the clumping metric, how "clumped together" the
-- properties are. Found by counting the parents whose descendent leaves are of
-- those properties. They are weighted by how far away those leaves are.
generateClumpMap :: (Eq a)
                 => [Label a]
                 -> Tree a
                 -> ClumpList a
generateClumpMap labelList tree = map correctedClump labelCompareList
  where
    labelCompareList = (\ p1 p2 -> (p1, p2)) <$> labelList <*> labelList
    correctedClump (!p1, !p2)  = ( p1
                                 , p2
                                 , geomAvg [part p1 p2 p1, part p1 p2 p2] )
    part p1 p2 p = if (numPLeaves p > 0)
                    then (clump p1 p2 * (fromRational (1 % numInner)))
                       * (fromRational (numLeaves % numPLeaves p))
                    else 0
    clump p1 p2  = getClumpiness p1 p2 tree
    numPLeaves p = genericLength . filter (== p) . leaves $ tree
    numLeaves    = genericLength . leaves $ tree
    numInner     = genericLength . innerNodes $ tree

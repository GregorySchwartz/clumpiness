-- clumpiness
-- By Gregory W. Schwartz

{-# LANGUAGE BangPatterns #-}

module Clumpiness.Algorithms where

-- Built-in
import Data.Maybe
import Data.List
import Data.Tree
import Control.Applicative
import Data.Ratio
import qualified Data.Map as M

-- Cabal
import FunTree.Tree
import FunTree.Types

-- Local
import Clumpiness.Types

-- | Get the geometric average of a list
geomAvg :: [Double] -> Double
geomAvg xs = product xs ** (1 / genericLength xs)

-- | Weigh the nodes by distance from the root
weigh :: Double -> Double
weigh x = 1 / (2 ** x)

-- | Get the property of a label
getProp :: (Ord a) => a -> PropertyMap a b -> b
getProp x = fromJust . M.lookup x

-- | Only look at these properties, if they aren't both in the list
-- (or if p1 == p2 and the length is 1), then ignore it
relevantList :: (Eq a) => a -> a -> [(a, Int)] -> [Int]
relevantList p1 p2 l
    | (p1 `elem` map fst relevantNodes && p2 `elem` map fst relevantNodes)
   && (length relevantNodes > 1)               = map snd relevantNodes
    | otherwise                                = []
  where
    relevantNodes = filter (\x -> fst x `elem` [p1, p2]) l

-- | Only look at these properties, if they aren't both in the list
-- (or if p1 == p2 and the length is 1), then ignore it, Map version
relevantMap :: (Ord a, Ord b)
            => b
            -> b
            -> PropertyMap a b
            -> M.Map a Int
            -> [Int]
relevantMap p1 p2 propertyMap lm
    | M.member p1 relevantProperties && M.member p2 relevantProperties
   && (M.size relevantNodes > 1)               = M.elems relevantNodes
    | otherwise                                = []
  where
    relevantNodes      = M.filterWithKey (\k _ -> property k `elem` [p1, p2]) lm
    relevantProperties = M.mapKeys property lm
    property x         = fromJust . M.lookup x $ propertyMap

-- | Get the clumpiness metric (before sample size correction)
getClumpiness :: (Ord a, Ord b)
              => b
              -> b
              -> PropertyMap a b
              -> Tree (SuperNode a)
              -> Double
getClumpiness _ _ _ (Node { subForest = [] }) = 0
getClumpiness p1 p2 propertyMap n@(Node { subForest = xs })
    = sum $ clump : map (getClumpiness p1 p2 propertyMap) xs
  where
    clump = sum
          . map (weigh . fromIntegral)
          . relevantMap p1 p2 propertyMap
          . M.mapKeys myRootLabel
          . leavesHeight 0
          $ n

-- | Get the heatmap for the clumping metric, how "clumped together" the
-- properties are. Found by counting the parents whose descendent leaves are of
-- those properties. They are weighted by how far away those leaves are.
generateClumpMap :: (Ord a, Ord b)
                 => PropertyMap a b
                 -> Tree (SuperNode a)
                 -> ClumpList b
generateClumpMap propertyMap tree =
    map correctedClump propertyCompareList
  where
    propertyCompareList = (\ p1 p2 -> (p1, p2))
                      <$> propertyList
                      <*> propertyList
    correctedClump (!p1, !p2)  = ( p1
                                 , p2
                                 , geomAvg [part p1 p2 p1, part p1 p2 p2] )
    part p1 p2 p = if (numPLeaves p :: Int) > 0
                    then (clump p1 p2 * fromRational (1 % numInner'))
                       * fromRational (numLeaves' % numPLeaves p)
                    else 0
    clump p1 p2  = getClumpiness p1 p2 propertyMap tree
    numPLeaves p = genericLength
                 . filter (\x -> getProp x propertyMap == p)
                 . map myRootLabel
                 . leaves
                 $ tree
    propertyList = nub . M.elems $ propertyMap
    numLeaves'   = numLeaves tree
    numInner'    = numInner tree

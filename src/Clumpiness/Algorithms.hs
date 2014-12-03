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
import Data.Function (on)

-- Cabal
import FunTree.Tree
import FunTree.Types
import Diversity.Diversity (diversity)

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
    | p1 `elem` map fst relevantNodes && p2 `elem` map fst relevantNodes
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
            -> M.Map a Int
relevantMap p1 p2 propertyMap lm
    | M.member p1 relevantProperties && M.member p2 relevantProperties
   && (M.size relevantNodes > 1) = relevantNodes
    | otherwise                  = M.empty
  where
    relevantNodes      = M.filterWithKey (\k _ -> property k `elem` [p1, p2]) lm
    relevantProperties = M.mapKeys property lm
    property x         = fromJust . M.lookup x $ propertyMap

-- | Get the clumpiness of a single node. Ignore the root node. Only count p1 ==
-- p2 case when there are at least one set of neighboring leaves in order to
-- account for the extreme cases (complete mixture, complete separation of
-- properties) which are throwing off the p1 == p2 case. So explicitly calculate
-- cases where the number of descendent leaves is 2.
getNodeClumpiness :: (Ord a, Ord b)
                  => b
                  -> b
                  -> PropertyMap a b
                  -> Tree (SuperNode a)
                  -> Double
getNodeClumpiness _ _ _ (Node {rootLabel = SuperNode {myParent = SuperRoot}})
    = 0
getNodeClumpiness p1 p2 propertyMap n
    = sum
    . map (weigh . fromIntegral)
    . M.elems
    . relevantMap p1 p2 propertyMap
    . M.map fst
--    . getSameSiblings (p1 == p2)  -- Ignore for now
    . M.mapKeys myRootLabel
    . leavesCommonHeight 0
    $ n
  where
    getSameSiblings True  = M.fromList
                          . concat
                          . filter (\x -> length x > 1)
                          . groupBy (\x y -> (snd . snd $ x) == (snd . snd $ y))
                          . sortBy (compare `on` (snd . snd))
                          . M.toAscList
    getSameSiblings False = id

-- | Get the clumpiness metric (before sample size correction)
getPropertyClumpiness :: (Ord a, Ord b)
                      => b
                      -> b
                      -> PropertyMap a b
                      -> Tree (SuperNode a)
                      -> Double
getPropertyClumpiness _ _ _ (Node { subForest = [] }) = 0
getPropertyClumpiness p1 p2 propertyMap n@(Node { subForest = xs })
    = sum $ getNodeClumpiness p1 p2 propertyMap n : rest
  where
    rest = map (getPropertyClumpiness p1 p2 propertyMap) xs

-- | Get the "Effective Mesh Size" of a node (count the number of relevant
-- leaves and square it, if the number of one property exceeds the other, get
-- the minimum and multiply by 2)
getNodeMesh :: (Ord a, Ord b)
            => b
            -> b
            -> PropertyMap a b
            -> Tree (SuperNode a)
            -> Double
getNodeMesh _ _ _ (Node {rootLabel = SuperNode {myParent = SuperRoot}})
    = 0
getNodeMesh p1 p2 propertyMap n
    = (** 2)
    . fromIntegral
    . getRelevantNum
    . relevantMap p1 p2 propertyMap
    . M.map fst
    . M.mapKeys myRootLabel
    . leavesCommonHeight 0
    $ n
  where
    getRelevantNum x = if p1 /= p2
                        then 2 * (minimum [numProp p1 x, numProp p2 x])
                        else M.size x
    numProp p = M.size . M.filterWithKey (\k _ -> property k == p)
    property x = fromJust . M.lookup x $ propertyMap

-- | Get the clumpiness metric (before sample size correction)
getPropertyMesh :: (Ord a, Ord b)
                => b
                -> b
                -> PropertyMap a b
                -> Tree (SuperNode a)
                -> Double
getPropertyMesh _ _ _ (Node { subForest = [] }) = 0
getPropertyMesh p1 p2 propertyMap n@(Node { subForest = xs })
    = sum $ getNodeMesh p1 p2 propertyMap n : rest
  where
    rest = map (getPropertyMesh p1 p2 propertyMap) xs

-- | Get the diversity of a node by its relevant leaves (if p1 == p2, then
-- set the other nodes to be Nothing to get the diversity of p1 vs the rest)
getNodeDiversity :: (Ord a, Ord b)
                 => b
                 -> b
                 -> PropertyMap a b
                 -> Tree (SuperNode a)
                 -> Double
getNodeDiversity _ _ _ (Node {rootLabel = SuperNode {myParent = SuperRoot}})
    = 0
getNodeDiversity p1 p2 propertyMap n
    = diversity 1
    . processProperties (p1 == p2)
    . map (property . myRootLabel)
    . M.keys
    . leavesCommonHeight 0
    $ n
  where
    processProperties True  = map (\x -> if x == p1 then Just p1 else Nothing)
    processProperties False = map Just . filter (`elem` [p1, p2])
    property x = fromJust . M.lookup x $ propertyMap

-- | Get the clumpiness metric (before sample size correction)
getPropertyDiversity :: (Ord a, Ord b)
                     => b
                     -> b
                     -> PropertyMap a b
                     -> Tree (SuperNode a)
                     -> Double
getPropertyDiversity _ _ _ (Node { subForest = [] }) = 0
getPropertyDiversity p1 p2 propertyMap n@(Node { subForest = xs })
    = sum $ getNodeDiversity p1 p2 propertyMap n : rest
  where
    rest = map (getPropertyDiversity p1 p2 propertyMap) xs

-- | Get the heatmap for the clumping metric, how "clumped together" the
-- properties are. Found by counting the parents whose descendent leaves are of
-- those properties. They are weighted by how far away those leaves are.
generateClumpMap :: (Ord a, Ord b)
                 => Metric
                 -> PropertyMap a b
                 -> Tree (SuperNode a)
                 -> ClumpList b
generateClumpMap metric propertyMap tree =
    map (getRelationship metric) propertyCompareList
  where
    propertyCompareList = (\ !p1 !p2 -> (p1, p2))
                      <$> propertyList
                      <*> propertyList
    getRelationship Clumpiness (!p1, !p2) =
        (p1, p2, geomAvg [part p1 p2 p1, part p1 p2 p2])
    getRelationship Mesh (!p1, !p2) =
        (p1, p2, getPropertyMesh p1 p2 propertyMap tree)
    getRelationship Diversity (!p1, !p2) =
        (p1, p2, getPropertyDiversity p1 p2 propertyMap tree)
    part p1 p2 p = if (numPLeaves p :: Int) > 0
                    then (clump p1 p2 * fromRational (1 % numInner'))
                       * fromRational (numLeaves' % numPLeaves p)
                    else 0
    clump p1 p2  = getPropertyClumpiness p1 p2 propertyMap tree
    numPLeaves p = genericLength
                 . filter (== p)
                 . M.elems
                 $ propertyMap
    propertyList = getProperties propertyMap
    numLeaves'   = numLeaves tree
    numInner'    = numInner tree

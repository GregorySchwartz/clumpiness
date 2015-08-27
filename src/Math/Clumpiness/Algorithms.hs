-- clumpiness
-- By Gregory W. Schwartz

{-# LANGUAGE BangPatterns #-}

module Math.Clumpiness.Algorithms where

-- Built-in
import Data.Maybe
import Data.List
import Data.Tree
import Control.Applicative
import Data.Ratio
import qualified Data.Map as M
import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Function (on)
import Debug.Trace

-- Cabal
import Math.FunTree.Tree
import Math.FunTree.Types
import Math.Diversity.Diversity (diversity)

-- Local
import Math.Clumpiness.Types

-- | Get the geometric average of a list
geomAvg :: [Double] -> Double
geomAvg xs = product xs ** (1 / genericLength xs)

-- | Weigh the nodes by what weight they have (based on the product of the
-- number of children their parents have, see leavesParentMult in tree-fun)
-- and invert it
weigh :: Double -> Double
weigh x = 1 / x

-- | Get the same amount of p1 and p2 nodes, getting the minimum p in the
-- list and selecting the closest p' (lower nodes have preference, so we
-- get the two different properties as close together as we can)
getEvenCount :: (Ord a, Eq b)
             => b
             -> b
             -> PropertyMap a b
             -> [(a, Double)]
             -> [(a, Double)]
getEvenCount _ _ _ []             = []
getEvenCount p1 p2 propertyMap ls = evenList
  where
    evenList = fst
             . foldl' (\ (!accList, !remainingList) !x
                      -> ( closest x remainingList : accList
                         , filter (/= closest x remainingList) remainingList ) )
               ([], pList (otherP minP))
             . pList
             $ minP
    closest (_, !h) = removeSnd
                    . head
                    . sortBy (compare `on` snd')
                    . map (\(!x, !y) -> (x, (abs (h - y), h > y), y))
                    . filter (F.elem (otherP minP) . property . fst)
    minP = if (length . pList $ p1) > (length . pList $ p2)
            then p2
            else p1
    pList p               = filter (F.elem p . property . fst) ls
    removeSnd (!x, _, !y) = (x, y)
    otherP p              = if p == p1 then p2 else p1
    snd' (_, !x, _)       = x
    -- Just get an empty sequence if it's not in the map
    property x            = fromMaybe Seq.empty $ M.lookup x propertyMap

-- | Get the same amount of p1 and p2 nodes, getting the minimum p in the
-- list and selecting the closest p' (lower nodes have preference).
-- However, here we have the case where p1 == p2, so we want nodes with any
-- other property. Here, True are cases with p1, False is any other property.
getEvenCountSame :: (Ord a, Eq b)
                 => b
                 -> PropertyMap a b
                 -> [(a, Double)]
                 -> [(a, Double)]
getEvenCountSame _ _ []            = []
getEvenCountSame p1 propertyMap ls = evenList
  where
    evenList = fst
             . foldl' (\ (!accList, !remainingList) !x
                      -> ( closest x remainingList : accList
                         , filter (/= closest x remainingList) remainingList ) )
               ([], pList (not minP))
             . pList
             $ minP
    closest (_, !h) = removeSnd
                    . head
                    . sortBy (compare `on` snd')
                    . map (\(!x, !y) -> (x, (abs (h - y), h > y), y))
                    . filter (F.elem (not minP) . property . fst)
    minP = if (length . pList $ True) > (length . pList $ False)
            then False
            else True
    pList p               = filter (F.elem p . property . fst) ls
    removeSnd (!x, _, !y) = (x, y)
    snd' (_, !x, _)       = x
    -- Just get an empty sequence if it's not in the map
    property x            = fmap (== p1)
                          . fromMaybe Seq.empty
                          $ M.lookup x propertyMap

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
-- (or if p1 == p2 and the length is 1), then ignore it, Map version.
-- Ignore nodes not in propertyMap
relevantMap :: (Ord a, Ord b)
            => b
            -> b
            -> PropertyMap a b
            -> M.Map a c
            -> M.Map a c
relevantMap p1 p2 propertyMap lm
    | Set.member p1 relevantProperties && Set.member p2 relevantProperties
   && (M.size relevantNodes > 1) = relevantNodes
    | otherwise                  = M.empty
  where
    relevantNodes = M.filterWithKey ( \k _ -> maybeToBool
                                            . fmap (F.any (`elem` [p1, p2]))
                                            $ property k ) lm
    relevantProperties   = Set.fromList
                         . F.toList
                         . F.foldl' (Seq.><) Seq.empty
                         . map fromJust
                         . filter isJust
                         . map property
                         . M.keys
                         $ lm
    maybeToBool Nothing  = False
    maybeToBool (Just x) = x
    property x           = M.lookup x propertyMap

-- | Only look at these properties, if they aren't both in the list
-- (or if p1 == p2 and the length is 1), then ignore it.
-- Ignore nodes not in propertyMap
relevantMapSame :: (Ord a, Ord b)
                => b
                -> PropertyMap a b
                -> M.Map a c
                -> M.Map a c
relevantMapSame p1 propertyMap lm
    | Set.member p1 relevantProperties
   && (not . Set.null . Set.filter (/= p1) $ relevantProperties) = lm
    | otherwise                                                  = M.empty
  where
    relevantProperties   = Set.fromList
                         . F.toList
                         . F.foldl' (Seq.><) Seq.empty
                         . map fromJust
                         . filter isJust
                         . map property
                         . M.keys
                         $ lm
    property x         = M.lookup x $ propertyMap

-- | Get the clumpiness of a single node. Ignore the root node. Only count p1 ==
-- p2 case when there are at least one set of neighboring leaves in order to
-- account for the extreme cases (complete mixture, complete separation of
-- properties) which are throwing off the p1 == p2 case. So explicitly calculate
-- cases where the number of descendent leaves is 2. Ignore nodes not in
-- propertyMap
getNodeClumpiness :: (Ord a, Ord b)
                  => Metric
                  -> b
                  -> b
                  -> PropertyMap a b
                  -> Tree (SuperNode a)
                  -> Double
getNodeClumpiness _ _ _ _ (Node {rootLabel = SuperNode {myParent = SuperRoot}})
    = 0
getNodeClumpiness metric p1 p2 propertyMap n
    = sum
    . map (weigh . snd)
    . getEvens metric (p1 == p2) -- Optional depending on metric used
    . M.toAscList
    . getRelevant (p1 == p2)
    . M.mapKeys myRootLabel
    . leavesParentMult 1
    $ n
  where
    getEvens Clumpiness True  = getEvenCountSame p1 propertyMap
    getEvens Clumpiness False = getEvenCount p1 p2 propertyMap
    getEvens _ _ = id
    getRelevant True  = relevantMapSame
                        p1
                        propertyMap
    getRelevant False = relevantMap p1 p2 propertyMap

-- | Get the clumpiness metric (before sample size correction)
getPropertyClumpiness :: (Ord a, Ord b)
                      => Metric
                      -> b
                      -> b
                      -> PropertyMap a b
                      -> Tree (SuperNode a)
                      -> Double
getPropertyClumpiness _ _ _ _ (Node { subForest = [] }) = 0
getPropertyClumpiness metric p1 p2 propertyMap n@(Node { subForest = xs })
    = sum $ getNodeClumpiness metric p1 p2 propertyMap n : rest
  where
    rest = map (getPropertyClumpiness metric p1 p2 propertyMap) xs

-- | Get the diversity of a node by its relevant leaves (if p1 == p2, then
-- set the other nodes to be Nothing to get the diversity of p1 vs the rest)
getNodeDiversity :: (Ord a, Ord b)
                 => Double
                 -> b
                 -> b
                 -> PropertyMap a b
                 -> Tree (SuperNode a)
                 -> Double
getNodeDiversity _ _ _ _ (Node {rootLabel = SuperNode {myParent = SuperRoot}})
    = 0
getNodeDiversity q p1 p2 propertyMap n
    = diversity q
    . processProperties (p1 == p2)
    . map fromJust
    . filter isJust
    . map (property . myRootLabel)
    . M.keys
    . leavesParentMult 1
    $ n
  where
    processProperties True  = (\x -> if all isNothing x then [] else x)
                            . map (\x -> if x == p1 then Just p1 else Nothing)
                            . F.toList
                            . F.foldl' (Seq.><) Seq.empty
    processProperties False = map Just
                            . F.toList
                            . F.foldl' (Seq.><) Seq.empty
                            . map (Seq.filter (`elem` [p1, p2]))
    property x = M.lookup x propertyMap

-- | Get the clumpiness metric (before sample size correction)
getPropertyDiversity :: (Ord a, Ord b)
                     => Double
                     -> b
                     -> b
                     -> PropertyMap a b
                     -> Tree (SuperNode a)
                     -> Double
getPropertyDiversity _ _ _ _ (Node { subForest = [] }) = 0
getPropertyDiversity q p1 p2 propertyMap n@(Node { subForest = xs })
    = sum $ getNodeDiversity q p1 p2 propertyMap n : rest
  where
    rest = map (getPropertyDiversity q p1 p2 propertyMap) xs

-- | Get the heatmap for the clumping metric, how "clumped together" the
-- properties are. Found by counting the parents whose descendent leaves are of
-- those properties. They are weighted by how far away those leaves are.
-- Remove any unwanted properties by having the "viable" function take in
-- a property and return if it is viable or not
generateClumpMap :: (Ord a, Ord b)
                 => Metric
                 -> (b -> Bool)
                 -> PropertyMap a b
                 -> Tree (SuperNode a)
                 -> ClumpList b
generateClumpMap metric viable propertyMap tree =
    map (getRelationship metric) propertyCompareList
  where
    propertyCompareList = (\ !p1 !p2 -> (p1, p2))
                      <$> propertyList
                      <*> propertyList
    getRelationship Clumpiness (!p1, !p2) = divResult clump p1 p2
    getRelationship ClumpinessMult (!p1, !p2) = multResult clump p1 p2
    getRelationship (Diversity x) (!p1, !p2) =
        divDiversityResult (getDiversity x) p1 p2
    getRelationship (MeanDiversity x) (!p1, !p2) =
        if p1 == p2
            then
                ( p1, p2, 1 - ( ( getPropertyDiversity x p1 p2 propertyMap tree
                              / (fromIntegral numInner' * 2) ) ) )
            else
                ( p1, p2, (getPropertyDiversity x p1 p2 propertyMap tree)
                        / (fromIntegral numInner' * 2) )
    divDiversityResult f p1 p2 =
        if p1 == p2
            then
                ( p1, p2, 1 - ( (geomAvg [divWeight False p1 p2 f p1, divWeight True p1 p2 f p2])
                              / (numProperties ^ 2) ) )
            else
                ( p1, p2, (geomAvg [divWeight False p1 p2 f p1, divWeight False p1 p2 f p2])
                          / (numProperties ^ 2) )
    divResult f p1 p2 =
        if p1 == p2
            then
                ( p1, p2, 1 - ( (geomAvg [divWeight False p1 p2 f p1, divWeight True p1 p2 f p2])
                              / numProperties ) )
            else
                ( p1, p2, (geomAvg [divWeight False p1 p2 f p1, divWeight False p1 p2 f p2])
                          / numProperties )
    multResult f p1 p2 =
        if p1 == p2
            then
                ( p1, p2, 1 - (geomAvg [multWeight False p1 p2 f p1, multWeight True p1 p2 f p2]) )
            else
                ( p1, p2, (geomAvg [multWeight False p1 p2 f p1, multWeight False p1 p2 f p2])
                          / numProperties )
    -- If we have no leaves of that property than the value is 0 (0 if it's
    -- the same as well). If all leaves are of a single property than the
    -- value is also 0.
    divWeight True p1 p2 f p = if (numPLeaves p :: Int) > 0
                               && numPLeaves p < numLeaves'
                                then (f p1 p2 * fromRational (1 % numInner'))
                                   * fromRational (numLeaves' % (numLeaves' - numPLeaves p))
                                else 0
    divWeight False p1 p2 f p = if (numPLeaves p :: Int) > 0
                                    then (f p1 p2 * fromRational (1 % numInner'))
                                       * fromRational (numLeaves' % numPLeaves p)
                                    else 0
    multWeight True p1 p2 f p = if (numPLeaves p :: Int) > 0
                                 then (f p1 p2 * fromRational (1 % numInner'))
                                    * (1 - fromRational ((numLeaves' - numPLeaves p) % numLeaves'))
                                 else 0
    multWeight False p1 p2 f p = if (numPLeaves p :: Int) > 0
                                    then (f p1 p2 * fromRational (1 % numInner'))
                                       * (1 - fromRational (numPLeaves p % numLeaves'))
                                    else 0
    clump p1 p2          = getPropertyClumpiness metric p1 p2 propertyMap tree
    getDiversity x p1 p2 = getPropertyDiversity x p1 p2 propertyMap tree
    numPLeaves p         = fromIntegral
                         . M.size
                         . M.filter (F.elem p)
                         $ propertyMap
    propertyList         = filter viable . getProperties $ propertyMap
    -- The number of properties being compared here
    numProperties        = 2 --genericLength . nub $ propertyList
    numLeaves'           = if hasRootLeaf tree
                            then numLeaves tree - 1
                            else numLeaves tree
    numInner'            = numInner tree - 1 -- We don't count the root

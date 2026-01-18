module Search where

import Data.Set (Set)
import qualified Data.Set as Set

-- graph represented as a function from nodes to neighbours
type Graph a = a -> Set a

-- breadth-first search: the set in position i consists of
-- the elements reachable from s in i steps and no fewer.
bfs :: Ord a => Graph a -> a -> [Set a]
bfs f s =
    takeWhile (not . Set.null) $
    zipWith Set.difference ss (Set.empty:ss)
  where
    ss = reachable f s

-- The set in position i consists of the elements reachable
-- from s in i steps or fewer.
reachable :: Ord a => Graph a -> a -> [Set a]
reachable f s = iterate (expand f) (Set.singleton s)

-- all the nodes reachable in one step from s, plus s
expand :: Ord a => (a -> Set a) -> Set a -> Set a
expand f s = Set.union s (unionMap f s)

-- the union of the sets obtained by applying f to each element of s
unionMap :: Ord a => (a -> Set a) -> Set a -> Set a
unionMap f s = Set.unions (map f (Set.elems s))

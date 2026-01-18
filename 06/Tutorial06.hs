module Tutorial06 where

import Geometry
import Search
import Maze
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

-- Use Set to implement a function that removes all duplicates from a list.
unique :: Ord a => [a] -> [a]
unique xs = Set.elems (Set.fromList xs)


frequencyMap :: Ord a => [a] -> Map a Int
frequencyMap xs = Map.fromListWith (+) [(x, 1) | x <- xs]

furtherest :: Maze -> Set Point
furtherest (open, start, finish) = last (bfs (moves open) start)

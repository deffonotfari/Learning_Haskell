module Geometry where

data Direction
    = North | East | South | West
    deriving Show

data Point = Coord Int Int
    deriving (Eq, Ord, Show)

turnLeft :: Direction -> Direction
turnLeft North = West
turnLeft West  = South
turnLeft South = East
turnLeft East  = North

turnRight :: Direction -> Direction
turnRight North = East
turnRight East  = South
turnRight South = West
turnRight West  = North

origin :: Point
origin = Coord 0 0

plusPoint :: Point -> Point -> Point
plusPoint (Coord x1 y1) (Coord x2 y2) = Coord (x1 + x2) (y1 + y2)

-- Function that takes a point and returns it swapped around.
swap :: Point -> Point
swap (Coord a b) = Coord b a

-- Define a function that returns a point with two copies of its argument.
dup :: Int -> Point
dup a = Coord a a

-- Subtracts two points
minusPoint :: Point -> Point -> Point
minusPoint (Coord x1 y1) (Coord x2 y2) = Coord (x1 - x2) (y1 - y2)

-- Multiplies both components by a given number
timesPoint :: Int -> Point -> Point
timesPoint n (Coord x y) = Coord (n * x) (n * y)

-- computes the sum of the absolute values of two components
normPoint :: Point -> Int
normPoint (Coord x y) = abs x + abs y

-- computes the manhattan distance between 2 points
distance :: Point -> Point -> Int
distance p1 p2 = normPoint (minusPoint p1 p2)

-- A function that maps each direction to the point one unit from the origin in that direction
oneStep :: Direction -> Point
oneStep North = Coord 0 1
oneStep East  = Coord 1 0
oneStep South = Coord 0 (-1)
oneStep West  = Coord (-1) 0

-- Add to your Geometry module a function
-- That takes a piece of text and maps it onto the two-dimensional plane, with the first 
-- character at the origin. For example, readGrid "#.#\n###\n#.#\n" should return
readGrid :: String -> [(Point, Char)]
readGrid s =
    [(Coord x y, c) |
        (y, cs) <- zip [0,-1..] (lines s),
        (x, c) <- zip [0..] cs
    ]
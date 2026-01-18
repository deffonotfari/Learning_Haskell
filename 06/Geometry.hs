module Geometry where

-- compass points
data Direction = North | South | East | West
    deriving Show

-- the direction immediately to the left
turnLeft :: Direction -> Direction
turnLeft North = West
turnLeft South = East
turnLeft East = North
turnLeft West = South

-- the direction immediately to the right
turnRight :: Direction -> Direction
turnRight North = East
turnRight South = West
turnRight East = South
turnRight West = North

-- x and y coordinates in two-dimensional space
data Point = Coord Int Int
    deriving (Eq, Ord, Show)

-- the origin of the two-dimensional space
origin :: Point
origin = Coord 0 0

-- add two points
plusPoint :: Point -> Point -> Point
plusPoint (Coord x1 y1) (Coord x2 y2) = Coord (x1+x2) (y1+y2)

-- subtract two points
minusPoint :: Point -> Point -> Point
minusPoint (Coord x1 y1) (Coord x2 y2) = Coord (x1-x2) (y1-y2)

-- multiply both components of a point by a given number
timesPoint :: Int -> Point -> Point
timesPoint n (Coord x y) = Coord (n*x) (n*y)

-- Manhattan metric of the point
normPoint :: Point -> Int
normPoint (Coord x y) = abs x + abs y

-- distance between two points using the Manhattan metric
distance :: Point -> Point -> Int
distance p1 p2 = normPoint (minusPoint p1 p2)

-- the point one unit from the origin in the given direction
oneStep :: Direction -> Point
oneStep North = Coord 0 1
oneStep South = Coord 0 (-1)
oneStep East = Coord 1 0
oneStep West = Coord (-1) 0

-- map lines of text onto two-dimensional space, starting at the origin
readGrid :: String -> [(Point, Char)]
readGrid s =
    [(Coord x y, c) |
        (y, l) <- zip [0,-1..] (lines s),
        (x, c) <- zip [0..] l]

-- Display a rectangular region, given the bottom left corner,
-- the top right corner and a function giving a character for each point.
showGrid :: Point -> Point -> (Point -> Char) -> String
showGrid (Coord minx miny) (Coord maxx maxy) cell =
    unlines [[cell (Coord x y) | x <- [minx..maxx]] |
        y <- reverse [miny..maxy]]

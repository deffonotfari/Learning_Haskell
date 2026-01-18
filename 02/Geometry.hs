module Geometry where

data Direction
    = North | East | South | West
    deriving Show

turnLeft :: Direction -> Direction
turnLeft North = East
turnLeft East = South
turnLeft South = West
turnLeft West = North

turnRight:: Direction -> Direction
turnRight North = West
turnRight East = North
turnRight South = East
turnRight West = South

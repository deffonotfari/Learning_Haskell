module Turtle where

import Geometry

-- state of a turtle
data Turtle = TurtleState Point Direction PenState
    deriving Show

-- state of a turtle's pen
data PenState = PenUp | PenDown
    deriving Show

-- start at the origin, facing North, with pen up
startTurtle :: Turtle
startTurtle = TurtleState origin North PenUp

-- location of the turtle
location :: Turtle -> Point
location (TurtleState pos dir pen) = pos

-- a command for a turtle
data Command
    = TurnLeft | TurnRight | Move Int | RaisePen | LowerPen
    deriving Show

-- action of a turtle command
action :: Turtle -> Command -> Turtle
action (TurtleState pos dir pen) TurnLeft =
    TurtleState pos (turnLeft dir) pen
action (TurtleState pos dir pen) TurnRight =
    TurtleState pos (turnRight dir) pen
action (TurtleState pos dir pen) (Move n) =
    TurtleState (plusPoint pos (timesPoint n (oneStep dir))) dir pen
action (TurtleState pos dir _) RaisePen =
    TurtleState pos dir PenUp
action (TurtleState pos dir _) LowerPen =
    TurtleState pos dir PenDown

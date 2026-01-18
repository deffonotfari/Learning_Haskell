module Turtle where

import Geometry

data Turtle
    = TurtleState Point Direction PenState
    deriving Show

data PenState
    = PenUp | PenDown
    deriving Show

data Command
    = TurnLeft | TurnRight | Move Int | RaisePen | LowerPen
    deriving Show

data Err a = OK a | Error String

startTurtle :: Turtle
startTurtle = TurtleState origin North PenUp

location :: Turtle -> Point
location (TurtleState pos dir pen) = pos

action:: Turtle -> Command -> Turtle
action (TurtleState pos dir pen) TurnLeft = TurtleState pos (turnLeft dir) pen
action (TurtleState pos dir pen) TurnRight = TurtleState pos (turnRight dir) pen
action (TurtleState pos dir pen) (Move n) = TurtleState (plusPoint pos (timesPoint n (oneStep dir))) dir pen
action (TurtleState pos dir pen) RaisePen = TurtleState pos dir PenUp
action (TurtleState pos dir pen) LowerPen = TurtleState pos dir PenDown


-- the function div gives a runtime error if its second argument is 0. Write a function
-- that reports the error case as Nothing and wraps the success case with Just.
safeDiv :: Int -> Int -> Maybe Int
safeDiv x y
    | y == 0        = Nothing
    | otherwise     = Just (div x y)

--Define a function that produces a Just result only if both arguments are Just, and a Nothing if either argument is Nothing.
pairMaybe :: Maybe a -> Maybe b -> Maybe (a,b)
pairMaybe Nothing Nothing = Nothing
pairMaybe Nothing (Just x) = Nothing
pairMaybe (Just x) Nothing = Nothing
pairMaybe (Just x) (Just y) = Just(x, y)

--Write your own implementation of the function
--which extracts the value inside a Maybe value, returning a default value (supplied as the first argument) 
--if the second argument is Nothing.
fromMaybe :: a -> Maybe a -> a
fromMaybe def Nothing = def
fromMaybe def (Just x) = x

--define a function that returns the value inside an Either value, ignoring how it is tagged.
whatever :: Either a a -> a
whatever (Left x) = x
whatever (Right y) = y

--A generalization of the Maybe type allows the failing part to include an error message:
both :: Err a -> Err b -> Err (a,b)
both (OK x) (OK y) = OK (x,y)
both (OK x) (Error msg) = Error msg
both (Error msg) (OK y) = Error msg
both (Error msg1) (Error msg2) = Error (msg1 ++ "\n" ++ msg2)
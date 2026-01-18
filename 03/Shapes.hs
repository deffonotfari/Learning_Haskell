module Shapes where

data Shape 
    = Circle Double
    | Rectangle Double Double
    deriving (Eq, Show)

--area
area :: Shape -> Double
area (Circle r) = pi * r * r
area (Rectangle w h) = w*h

--rotating a shape 90 degrees to the right
rotate :: Shape -> Shape
rotate (Circle r) = Circle r
rotate (Rectangle w h) = Rectangle w h

--- Scale a shape by a given number
scale :: Double -> Shape -> Shape
scale x (Circle r) = Circle (r*x)
scale x (Rectangle w h) = Rectangle (w*x) (h*x)
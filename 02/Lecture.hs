module Lecture where

maxThree :: Int -> Int -> Int -> Int
maxThree x y z
  | x >= y && x >= z  = x
  | y >= x && y >= z  = y
  | otherwise         = z

between :: Int -> Int -> Int -> Bool
between x y z =
    (x <= y && y <= z) || (z <= y && y <= x)

middleNumber :: Int -> Int -> Int -> Int
middleNumber x y z
  | between y x z  = x
  | between x y z  = y
  | otherwise      = z
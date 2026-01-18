module Tutorial02 where

import Data.Char

threeDifferent:: Int -> Int -> Int -> Bool
threeDifferent x y z = not (x == y || y == z || x == z)

mystery :: Int -> Int -> Int -> Bool
mystery x y z = not (x == y && y == z)

mysteryRewritten :: Int -> Int -> Int -> Bool
mysteryRewritten x y z = x /= y && y /= z

fractionalPart :: Double -> Double
fractionalPart f = f - fromIntegral (floor f)

clamp :: Double -> Double -> Double -> Double
clamp x lo hi
    | lo <= x && x <= hi    = x
    | x < lo                = lo
    | x > hi                = hi


--Write a function that converts a character that represents a digit, like '3', to the corresponding integer (3), or 0 if the character does not represent a digit. 
--You may assume that the digit characters are contiguous and in ascending order. You will need to place “import Data.Char” in your module to g
--ain access to the character functions
charToNum :: Char -> Int
charToNum x
    | isDigit x = ord x - ord '0'
    | otherwise = 0

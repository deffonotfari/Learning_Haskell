module Tutorial05 where

import Data.Char
import Text.XHtml.Strict (p)

-- Give a definition of the function that returns the input list with the lower case letters capitalized and the others unchanged.
capitalize :: String -> String
capitalize = map toUpper

-- Give a definition of the function that selects the capital letters from the input string.
capitals :: String -> String
capitals = filter isUpper

-- Write a function that does the same as capitalize, but discards non-letters.
capitalizeLettersOnly :: String -> String
capitalizeLettersOnly c = map toUpper(filter isAlpha c)

-- It returns the number of times that x occurs in the list ys
count :: Eq a => a -> [a] -> Int
count x ys = length (filter (==x)ys)

-- That is, it takes a list and selects the numbers whose squares are less than 20.
squaresLessThan20 :: [Integer] -> [Integer]
squaresLessThan20 = filter p
    where p n = n^2 < 20

-- Rewrite: f xs = map (\x -> (x+1)/2) xs
f :: [Float] -> [Float]
f = map((/2) . (+1))

foo :: [Int] -> [Int]
foo xs = zipWith (-) (tail xs) xs

-- Write a function that takes a number n if n is even, and 3n+1 if n is odd
collatz :: Int -> Int
collatz n
    | even n            = n `div` 2
    | otherwise         = 3*n + 1

-- Write a function that returns the list of numbers visited before the first 1.
collatzSeq :: Int -> [Int]
collatzSeq n = takeWhile (> 1) (iterate collatz n)

-- Write a function that takes a positive number n and returns the number of times that the 
-- function must be applied to reach 1. In the above example, collatzSteps 3 is 7.
collatzSteps :: Int -> Int
collatzSteps n = length (collatzSeq n)


-- Write a function that takes a positive number n
 --and returns the largest number encountered by repeatedly applying the function until it reaches 1. 
 --In the above example, collatzMax 3 is 16.
collatzMax :: Int -> Int
collatzMax n = foldr1 max(collatzSeq n)


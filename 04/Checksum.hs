module Checksum where

import Data.Char

-- Luhn algorithm for validating card numbers
valid :: String -> Bool
valid s = checksum s == 0

checksum :: String -> Int
checksum s = total `mod` 10
  where
    total = sum (odds rev_ns) +
            sum [substitute n | n <- evens rev_ns]
    rev_ns = reverse (digits s)

-- the digits in a string
digits :: String -> [Int]
digits s = [digitToInt c | c <- s, isDigit c]

-- elements in odd-numbered positions, counting from 1
odds :: [a] -> [a]
odds xs = [x | (i, x) <- zip [1..] xs, odd i]

-- elements in even-numbered positions, counting from 1
evens :: [a] -> [a]
evens xs = [x | (i, x) <- zip [1..] xs, even i]

-- substitute digit: same as summing digits of n*2
substitute :: Int -> Int
substitute n
  | n*2 > 9 = n*2 - 9
  | otherwise = n*2
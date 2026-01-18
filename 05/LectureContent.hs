module LectureContent where

import Data.Char

-- Repeats the function twice
twice :: (a -> a) -> a -> a
twice f x = f (f x)

--mapping
ordAll :: [Char] -> [Int]
ordAll cs = map ord cs


doubleAll :: [Int] -> [Int]
doubleAll ns = map double ns
    where double n = 2*n

--selecting elements (filtering)
pickEven :: [Int] -> [Int]
pickEven ns = filter even ns

letters :: [Char] -> [Char]
letters cs = filter isAlpha cs

pickOdd :: [Int] -> [Int]
pickOdd ns = filter odd ns


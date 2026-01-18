module LectureContent where

import Data.Char

--generalising selecting elemnts from a list
letters :: [Char] -> [Char]
letters [] = []
letters (c:cs)
  | isAlpha c = c : letters cs
  | otherwise = letters cs

--generalising getting letters from the start of a list
takeLetters :: [Char] -> [Char]
takeLetters [] = []
takeLetters (c:cs)
  | isAlpha c = c : takeLetters cs
  | otherwise = []

--The rest of the list after initial letters:
dropLetters :: [Char] -> [Char]
dropLetters [] = []
dropLetters (c:cs)
  | isAlpha c = dropLetters cs
  | otherwise = c:cs


ordAll :: [Char] -> [Int]
ordAll [] = []
ordAll (c:cs) = ord c : ordAll cs

doubleAll :: [Int] -> [Int]
doubleAll [] = []
doubleAll (n:ns) = double n : doubleAll ns
  where double x = 2*x
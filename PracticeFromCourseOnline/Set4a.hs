-- Exercise set 4a:
--
--  * using type classes
--  * working with lists
--
-- Type classes you'll need
--  * Eq
--  * Ord
--  * Num
--  * Fractional
--
-- Useful functions:
--  * maximum
--  * minimum
--  * sort

module Set4a where

import Data.List
import Data.Ord
import qualified Data.Map as Map
import Data.Array
import Control.Applicative (Alternative(some))
import Distribution.Simple.Utils (xargs)

------------------------------------------------------------------------------
-- Ex 1: implement the function allEqual which returns True if all
-- values in the list are equal.
--
-- Examples:
--   allEqual [] ==> True
--   allEqual [1,2,3] ==> False
--   allEqual [1,1,1] ==> True
--
-- PS. check out the error message you get with your implementation if
-- you remove the Eq a => constraint from the type!

allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual [_] = True
allEqual (x:y:xs)
    | x == y        = allEqual (y:xs)
    | otherwise     = False

------------------------------------------------------------------------------
-- Ex 2: implement the function distinct which returns True if all
-- values in a list are different.
--
-- Hint: a certain function from the lecture material can make this
-- really easy for you.
--
-- Examples:
--   distinct [] ==> True
--   distinct [1,1,2] ==> False
--   distinct [1,2] ==> True

distinct :: Eq a => [a] -> Bool
distinct []     = True
distinct [_]    = True
distinct (x:xs)
    | elem x xs = False
    | otherwise = distinct xs


------------------------------------------------------------------------------
-- Ex 3: implement the function middle that returns the middle value
-- (not the smallest or the largest) out of its three arguments.
--
-- The function should work on all types in the Ord class. Give it a
-- suitable type signature.
--
-- Examples:
--   middle 'b' 'a' 'c'  ==> 'b'
--   middle 1 7 3        ==> 3

middle :: Ord a => a -> a -> a -> a
middle x y z 
    | (x <= y && y <= z) || (z <= y && y <= x)  = y
    | (y <= x && x <= z) || (z <= x && x <= y)  = x
    | otherwise                                 = z

------------------------------------------------------------------------------
-- Ex 4: return the range of an input list, that is, the difference
-- between the smallest and the largest element.
--
-- Your function should work on all suitable types, like Float and
-- Int. You'll need to add _class constraints_ to the type of range.
--
-- It's fine if your function doesn't work for empty inputs.
--
-- Examples:
--   rangeOf [4,2,1,3]          ==> 3
--   rangeOf [1.5,1.0,1.1,1.2]  ==> 0.5

rangeOf :: (Ord a, Num a) => [a] -> a
rangeOf [] = 0
rangeOf [x] = x
rangeOf xs = maxVal xs - minVal xs
    where
        maxVal [x]    = x
        maxVal (x:xs)= max x (maxVal xs)
        minVal [x]    = x
        minVal (x:xs)= min x (minVal xs)


------------------------------------------------------------------------------
-- Ex 5: given a (non-empty) list of (non-empty) lists, return the longest
-- list. If there are multiple lists of the same length, return the list that
-- has the smallest _first element_.
--
-- (If multiple lists have the same length and same first element,
-- you can return any one of them.)
--
-- Give the function "longest" a suitable type.
--
-- Challenge: Can you solve this exercise without sorting the list of lists?
--
-- Examples:
--   longest [[1,2,3],[4,5],[6]] ==> [1,2,3]
--   longest ["bcd","def","ab"] ==> "bcd"

longest :: Ord a => [[a]] -> [a]
longest [x] = x
longest (x:y:xs)
    | better x y    = longest(x:xs)
    | otherwise     = longest(y:xs)
    where
        better a b
            | length a > length b   = True
            | length a < length b   = False
            | otherwise             = head a <= head b



---
--Define a function
--that returns the second largest element of a non-empty list containing at least two elements.
--
secondLargest :: Ord a => [a] -> a
secondLargest xs = maximum (filter (< maximum xs) xs)

------------------------------------------------------------------------------
-- Ex 6: Implement the function incrementKey, that takes a list of
-- (key,value) pairs, and adds 1 to all the values that have the given key.
--
-- You'll need to add _class constraints_ to the type of incrementKey
-- to make the function work!
--
-- The function needs to be generic and handle all compatible types,
-- see the examples.
--
-- Examples:
--   incrementKey True [(True,1),(False,3),(True,4)] ==> [(True,2),(False,3),(True,5)]
--   incrementKey 'a' [('a',3.4)] ==> [('a',4.4)]

incrementKey :: (Eq k, Num v) => k -> [(k,v)] -> [(k,v)]
incrementKey _ [] = []
incrementKey key ((k,v):xs)
  | key == k  = (k, v + 1) : incrementKey key xs
  | otherwise = (k, v)     : incrementKey key xs



------------------------------------------------------------------------------
-- Ex 7: compute the average of a list of values of the Fractional
-- class.
--
-- There is no need to handle the empty list case.
--
-- Hint! since Fractional is a subclass of Num, you have all
-- arithmetic operations available
--
-- Hint! you can use the function fromIntegral to convert the list
-- length to a Fractional

average :: Fractional a => [a] -> a
average xs = sum xs / fromIntegral (length xs)

------------------------------------------------------------------------------
-- Ex 8: given a map from player name to score and two players, return
-- the name of the player with more points. If the players are tied,
-- return the name of the first player (that is, the name of the
-- player who comes first in the argument list, player1).
--
-- If a player doesn't exist in the map, you can assume they have 0 points.
--
-- Hint: Map.findWithDefault can make this simpler
--
-- Examples:
--   winner (Map.fromList [("Bob",3470),("Jane",2130),("Lisa",9448)]) "Jane" "Lisa"
--     ==> "Lisa"
--   winner (Map.fromList [("Mike",13607),("Bob",5899),("Lisa",5899)]) "Lisa" "Bob"
--     ==> "Lisa"

winner :: Map.Map String Int -> String -> String -> String
winner scores player1 player2
    | score1 >= score2      = player1
    | otherwise             = player2
    where
            score1 = Map.findWithDefault 0 player1 scores
            score2 = Map.findWithDefault 0 player2 scores
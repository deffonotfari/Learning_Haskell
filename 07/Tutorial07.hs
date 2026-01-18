module Tutorial07 where

import Data.Char
import Distribution.Simple.Utils (xargs)

--The function returns the reverse of the input list. It is equivalent to the library function reverse, though the actual definition in the library is more obscure (and efficient)
reverse :: [a] -> [a]
reverse = foldl cons []
    where 
        cons xs x = x:xs

foo :: [a] -> [a]
foo [] = []
foo (x:xs) = foo xs ++ [x]


--It produces the infinite li of repeated applications of the function f to x. 
iterateProduct :: (a -> a) -> a -> [a]
iterateProduct f x = x : iterateProduct f (f x)


--Removes consecutive duplicate elements, keeping only the first element of each run.
compress :: Eq a => [a] -> [a]
compress [] = []
compress (x:xs) = x : compressConsecutive x xs

compressConsecutive :: Eq a => a -> [a] -> [a]
compressConsecutive x [] = []
compressConsecutive x (y:ys) 
  | x == y = compressConsecutive x ys
  | otherwise = y : compressConsecutive y ys


-- (using pattern matching over lists) that removes the first element of the list that is a digit, but keeps the rest. (You will need to import Data.Char.) Test your function on the strings "r2d2" and "2d2".
removeFirstDigit :: [Char] -> [Char]
removeFirstDigit [] = []
removeFirstDigit (x:xs)
  | isDigit x    = xs
  | otherwise    = x : removeFirstDigit xs

-- emoves the first element of the list that does not satisfy the property, but keeps the rest.
removeFirst :: (a -> Bool) -> [a] -> [a]
removeFirst p [] = []
removeFirst p (x:xs)
    | p x          = xs
    | otherwise    = x : removeFirst p xs

-- adds corresponding elements of two lists of the same type, with the extra elements of the longer list added at the end, e.g.
addLists :: Num a => [a] -> [a] -> [a]
addLists [] [] = []
addLists [] (y:ys) = y:addLists [] ys
addLists (x:xs) [] = x:addLists xs []
addLists (x:xs) (y:ys) = (x+y):addLists xs ys

-- Generalize the previous function to a higher-order function that takes the combining function as an argument:
-- For example, addLists should be equivalent to longZip (+).
longZip :: (a -> a -> a) -> [a] -> [a] -> [a]
longZip f [] ys = ys
longZip f xs [] = xs
longZip f (x:xs) (y:ys) = f x y: longZip f xs ys

-- Write a function that takes two lists, assumed to be ordered, and produces an ordered list obtained by merging these two lists.
merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x < y         = x:merge xs (y:ys)
    | otherwise     = y:merge (x:xs) ys


odds :: [a] -> [a]
odds [] = []
odds [x] = [x]
odds (x1:x2:xs) = x1: odds xs
 
evens :: [a] -> [a]
evens [] = []
evens [x] = []
evens (x1:x2:xs) = x2: evens xs


mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort (odds xs)) (mergeSort (evens xs))
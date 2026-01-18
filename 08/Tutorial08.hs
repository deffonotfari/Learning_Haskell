module Tutorial08 where

import Data.List

-- Consider the type of “leaf trees”:
data LTree a = Leaf a | Branch (LTree a) (LTree a)
    deriving (Show)

tree1 = Branch (Leaf 2) (Branch (Leaf 3) (Leaf 7))
tree2 = Branch (Leaf 4) (Branch (Leaf 12) (Leaf 34))


sumLTree :: Num a => LTree a -> a
sumLTree (Leaf x) = x
sumLTree (Branch l r) = sumLTree l + sumLTree r

-- return the size (number of leaves) of a leaf tree.
sizeOfLTree :: LTree a -> Int
sizeOfLTree (Leaf x) = 1
sizeOfLTree (Branch l r) = sizeOfLTree (l) + sizeOfLTree (r)


-- return the depth of a tree leaf
depth :: LTree a -> Int
depth (Leaf x) = 0
depth (Branch l r) = (depth l `max` depth r) + 1

-- return the list of values of a tree
leaves :: LTree a -> [a]
leaves (Leaf x) = [x]
leaves (Branch l r) = leaves (l) ++ leaves (r)

-- return the mirror image of an input leaf tree.
revLTree :: LTree a -> LTree a
revLTree (Leaf x) = Leaf x
revLTree (Branch l r) = Branch (revLTree r) (revLTree l)

-- Generalize the sumLTree function from the lecture to obtain a higher-order function
--  that reduces a tree to a summary value by combining the values 
-- of branches using the supplied function.
foldLTree :: (a -> a -> a) -> LTree a -> a
foldLTree f (Leaf x) = x
foldLTree f (Branch l r) = f (foldLTree f l) (foldLTree f r)

mapLTree :: (a -> b) -> LTree a -> LTree b
mapLTree f (Leaf x) = Leaf (f x)
mapLTree f (Branch l r) = Branch (mapLTree f l) (mapLTree f r)

-- Define the function returning the size of a leaf tree as a composition of the form sumLTree . mapLTree g for some suitable function  g. (You might also find the standard function const useful here.)
size2 :: LTree a -> Integer
size2 = sumLTree . mapLTree (const 1)

-- Redefining all of the previous functions using this notation
size3 :: LTree a -> Integer
size3 = foldLTree (+) . mapLTree (const 1)

depth2 :: LTree a -> Integer
depth2 = foldLTree (\n m -> max n m + 1) . mapLTree (const 0)

revLTree3 :: LTree a -> LTree a
revLTree3 = foldLTree (flip Branch) . mapLTree Leaf
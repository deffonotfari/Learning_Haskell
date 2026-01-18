module Lecture08 where

--trees with anything in their leaves
data LTree a = Leaf a | Branch (LTree a) (LTree a)
    deriving Show

--defining trees with a number type
tree1 :: (Num a) => LTree a
tree1 = Branch (Leaf 2) (Branch (Leaf 3) (Leaf 7))

-- summing tree leaves with a number type
sumLTree :: Num a => LTree a -> a
sumLTree (Leaf x) = x
sumLTree (Branch l r) = sumLTree l + sumLTree r


-- Doubling each element in a tree of numbers:
doubleTree :: LTree Int -> LTree Int
doubleTree (Leaf x) = Leaf (2*x)
doubleTree (Branch l r) = Branch (doubleTree l) (doubleTree r)

-- Applying an arbitrary function to each value in a tree:
mapLTree :: (a -> b) -> LTree a -> LTree b
mapLTree f (Leaf x) = Leaf (f x)
mapLTree f (Branch l r) = Branch (mapLTree f l) (mapLTree f r)


--Different Types of Trees
data NTree a = Empty | Node a (NTree a) (NTree a)
    deriving Show

--example of a NTREE
nTree1 :: (Num a) => NTree a
nTree1 = Node 17 (Node 14 Empty Empty) (Node 20 Empty Empty)

-- If we keep these trees ordered, we can use them as search trees:
member :: Ord a => a -> NTree a -> Bool
member x Empty = False
member x (Node k l r)
  | x < k = member x l
  | x > k = member x r
  | otherwise = True

module Set3c where
import Distribution.Simple.Utils (xargs)

-- ex 1: mplement minBy that takes a measuring function and two values and 
-- returns the one with the smaller measure.
minBy :: (a -> Int) -> a -> a -> a
minBy measure x y
    | measure x <= measure y        = x
    | otherwise                     = y


-- Return the value inside Just, but if it is Nothing, return the default.
maybeDefault :: a -> Maybe a -> a
maybeDefault _ (Just a) = a
maybeDefault def Nothing = def

-- Return the value inside Right, but if it is Left, return the default.
eitherDefault :: b -> Either a b -> b
eitherDefault def (Left x) = def
eitherDefault def (Right x) = x

--Count how many Right values appear in a list.
countRights :: [Either a b] -> Int
countRights [] = 0
countRights (x:xs) =
    case x of
        Right _ -> 1 + countRights xs
        Left _ -> countRights xs

-- Split a list into two halves.
--If the list has odd length, the middle element goes to the first half
splitHalves :: [a] -> ([a], [a])
splitHalves xs = go xs xs
  where
    go (x:xs) (_:_:ys) =
      let (a, b) = go xs ys
      in (x : a, b)
    go xs _ = ([], xs)

splitOdd :: [a] -> ([a], [a])
splitOdd xs = splitAt ((length xs + 1) `div` 2) xs

splitEven :: [a] -> ([a], [a])
splitEven xs
  | even (length xs) = splitAt (length xs `div` 2) xs
  | otherwise       = error "List length must be even"


-- Turn a list into adjacent pairs.
adjacentPairs :: [a] -> [(a,a)]
adjacentPairs [] = []
adjacentPairs [_] = []
adjacentPairs (x:y:xs) = (x,y):adjacentPairs(y:xs)

-- Generalize sorted using a comparison function.
sortedBy :: (a -> a -> Bool) -> [a] -> Bool
sortedBy _ [] = True
sortedBy _ [_] = True
sortedBy func (x:y:xs) 
    | func x y      = sortedBy func (y:xs)
    | otherwise     = False

--Generalize merge using a “comes first” function.
mergeBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
mergeBy _ xs [] = xs
mergeBy _ [] ys = ys
mergeBy func (x:xs) (y:ys)
    | func x y      = x : mergeBy func xs (y:ys)
    | otherwise     = y: mergeBy func (x:xs) ys

-- Like map2, but over 3 lists at the same time.
-- Stop when any list ends.
map3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
map3 f [] _ _ = []
map3 f _ [] _ = []
map3 f _ _ []= []
map3 f (x:xs) (y:ys) (z:zs) = f x y z : map3 f xs ys zs 

-- This is like your maybeMap, but it works on two lists.
-- If either element produces Nothing, skip it.
maybeMap2 :: (a -> b -> Maybe c) -> [a] -> [b] -> [c]
maybeMap2 f xs [] = []
maybeMap2 f [] ys = []
maybeMap2 f (x:xs) (y:ys) =
    case f x y of
        Just z -> z : maybeMap2 f xs ys
        Nothing -> maybeMap2 f xs ys

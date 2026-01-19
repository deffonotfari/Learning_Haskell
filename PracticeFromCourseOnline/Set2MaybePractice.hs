{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Set2MaybePractice where

import Data.Char
import Text.Read
import Data.List 

-- Return the first element of a list safely.
maybeHead :: [a] -> Maybe a
maybeHead xs
    | null xs              = Nothing
    | otherwise            = Just (head xs)

-- Return the tail of a list safely.
maybeTail :: [a] -> Maybe [a]
maybeTail xs
    | null xs              = Nothing
    | otherwise            = Just (tail xs)

-- Add two Maybe Int values:
-- If both are Just, return their sum
-- Otherwise Nothing

addMaybes :: Maybe Int -> Maybe Int -> Maybe Int
addMaybes (Just x) (Just y) = Just (x + y)
addMaybes _ _ = Nothing

-- Convert a single character digit into an Int.
parseDigit :: Char -> Maybe Int
parseDigit c
    | isDigit c      = Just (digitToInt c)
    | otherwise      = Nothing

--Safe last element of a list.
safeLast :: [a] -> Maybe a
safeLast xs
    | null xs       = Nothing
    | otherwise     = Just(last xs)


-- Given a list of keys and an association list, return all values.
-- If any key is missing, return Nothing.
lookupAll :: Eq k => [k] -> [(k,v)] -> Maybe [v]
lookupAll [] _ = Just []
lookupAll (k:ks) table = do
  v  <- lookup k table
  vs <- lookupAll ks table
  return (v:vs)


-- Like your substring, but return Nothing if indexes are invalid:
-- i < 0, j < 0
-- i > j
-- j > length s

safeSubstring :: Int -> Int -> String -> Maybe String
safeSubstring i j s
    | i < 0 || j < 0        = Nothing
    | i > j                 = Nothing
    | j > length s          = Nothing
    | otherwise             = Just (take (j - i) (drop i s))
    
--Apply a Maybe (a -> b) to a Maybe a.
-- Only succeeds if both are Just.
applyMaybe :: Maybe (a -> b) -> Maybe a -> Maybe b
applyMaybe (Just f) (Just x) = Just (f x)
applyMaybe _ _ = Nothing

-- Parse a whole string as an Int. Return Nothing 
-- if it’s not a valid integer (allow optional leading -).
safeReadInt :: String -> Maybe Int
safeReadInt = readMaybe

maybeMaximumBy :: (a -> a -> Ordering) -> [a] -> Maybe a
maybeMaximumBy _ [] = Nothing
maybeMaximumBy cmp (x: xs) = Just (foldl' pick x xs)
    where
        pick best a =
            case cmp best a of
                LT -> a
                _ -> best

sequenceMaybe :: [Maybe a] -> Maybe [a]
sequenceMaybe [] = Just []
sequenceMaybe (m:ms) = do
  x  <- m
  xs <- sequenceMaybe ms
  return (x:xs)

safeIndex :: [a] -> Int -> Maybe a
safeIndex [] _ = Nothing
safeIndex _ i | i < 0 = Nothing
safeIndex (x:_) 0 = Just x
safeIndex (_:xs) i = safeIndex xs (i - 1)

index2D :: [[a]] -> Int -> Int -> Maybe a
index2D xss r c = do
  row <- safeIndex xss r
  safeIndex row c

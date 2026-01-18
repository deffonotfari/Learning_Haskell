module Generator where

--A simple scheme for generating pseudo-random numbers from a seed value:
generate :: Int -> [Int]
generate seed = iterate (\ n -> 224149*n + 1) seed

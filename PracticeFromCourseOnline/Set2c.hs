------------------------------------------------------------------------------
-- Ex 9: implement factorial using recursion.
-- Examples:
--   factorial 5 ==> 120
--   factorial 0 ==> 1

factorial :: Integer -> Integer
factorial n
    | n == 0        = 1
    | otherwise     = n*factorial(n-1)

------------------------------------------------------------------------------
-- Ex 10: implement power (exponentiation) using recursion.
-- Examples:
--   power 2 5 ==> 32
--   power 7 0 ==> 1

power :: Integer -> Integer -> Integer
power n k
    | k == 0        = 1
    | otherwise     = n * power n (k-1)

------------------------------------------------------------------------------
-- Ex 11: implement sumTo that sums numbers from 1..n recursively.
-- Examples:
--   sumTo 5 ==> 15
--   sumTo 1 ==> 1

sumTo :: Integer -> Integer
sumTo n
    | n == 1        = 1
    | otherwise     = n + sumTo(n-1)

------------------------------------------------------------------------------
-- Ex 12: implement repeatString that repeats a string n times.
-- Examples:
--   repeatString "ha" 3 ==> "hahaha"
--   repeatString "x" 0 ==> ""

repeatString :: String -> Int -> String
repeatString str n
    | n == 0    = ""
    | otherwise = str ++ repeatString str (n-1)

------------------------------------------------------------------------------
-- Ex 13: implement rightpad which pads spaces on the RIGHT until length n.
-- Examples:
--   rightpad "foo" 5 ==> "foo  "
--   rightpad "xxxxx" 3 ==> "xxxxx"

rightpad :: String -> Int -> String
rightpad s n
  | length s >= n = s
  | otherwise     = rightpad (s ++ " ") n

------------------------------------------------------------------------------
-- Ex 14: implement countDigits that counts digits in a non-negative integer.
-- Examples:
--   countDigits 0 ==> 1
--   countDigits 7 ==> 1
--   countDigits 12345 ==> 5

countDigits :: Integer -> Integer
countDigits n
  | n < 10    = 1
  | otherwise = 1 + countDigits (n `div` 10)

------------------------------------------------------------------------------
-- Ex 15: implement sumDigits that sums digits in a non-negative integer.
-- Examples:
--   sumDigits 0 ==> 0
--   sumDigits 7 ==> 7
--   sumDigits 123 ==> 6

sumDigits :: Integer -> Integer
sumDigits n
    | n < 10 = n
    | otherwise = (n `mod` 10) + sumDigits(n `div` 10)

------------------------------------------------------------------------------
-- Ex 16: implement nextPrime that returns the smallest prime >= n.
-- Use isPrime.
-- Examples:
--   nextPrime 10 ==> 11
--   nextPrime 11 ==> 11
isPrime :: Integer -> Bool
isPrime n
  | n <= 1    = False
  | otherwise = check 2
  where
    check k
      | k == n        = True
      | n `mod` k == 0 = False
      | otherwise     = check (k + 1)

nextPrime :: Integer -> Integer
nextPrime n
    | n <= 2        = 2
    | isPrime n     = n
    | otherwise     = nextPrime(n +1)

prevPrime :: Integer -> Integer
prevPrime n
    | n <= 2        = 0
    | isPrime n     = n
    | otherwise     = prevPrime(n - 1)

---Ex 17: implement fibonacci (recursive).
fibonacci :: Integer -> Integer
fibonacci n
    | n <= 1        = n
    | otherwise     = fibonacci (n - 1) + fibonacci (n - 2)

-- Ex 18: implement triangularCheck that checks if n is a triangular number.
-- A triangular number is 1+2+...+k for some k.
-- Examples:
--   triangularCheck 6 ==> True   (1+2+3)
--   triangularCheck 10 ==> True  (1+2+3+4)
--   triangularCheck 8 ==> False

triangularCheck :: Integer -> Bool
triangularCheck n = go 1 0
  where
    go k acc
      | acc == n  = True
      | acc > n   = False
      | otherwise = go (k + 1) (acc + k)


--- implement binaryString that converts a non-negative number to binary.
binaryString :: Integer -> String
binaryString n
  | n < 0     = error "binaryString: negative input"
  | n == 0    = "0"
  | otherwise = go n
  where
    go 0 = ""
    go k = go (k `div` 2) ++ show (k `mod` 2)

--implement isPerfect that checks if n is a perfect number.
--A perfect number equals the sum of its proper divisors (excluding itself).
isPerfect :: Integer -> Bool
isPerfect n = sumProperDivs 1 == n
  where
    sumProperDivs k
      | k >= n         = 0
      | n `mod` k == 0 = k + sumProperDivs (k + 1)
      | otherwise      = sumProperDivs (k + 1)




--Implement reverseString using recursion (no built-in reverse).	
reverseString :: String -> String
reverseString []     = []
reverseString (c:cs) = reverseString cs ++ [c]

------------------------------------------------------------------------------
-- Ex 17: implement largestDivisor that returns the largest proper
-- divisor of a number (smaller than n).
--
-- Examples:
--   largestDivisor 10 ==> 5
--   largestDivisor 21 ==> 7
--   largestDivisor 13 ==> 1
largestDivisor :: Integer -> Integer
largestDivisor n = go (n - 1)
  where
    go k
      | k <= 1        = 1
      | n `mod` k == 0 = k
      | otherwise     = go (k - 1)

------------------------------------------------------------------------------
-- Ex 18: implement primeFactors that returns the prime factorization
-- of a number as a list, in ascending order.
--
-- Examples:
--   primeFactors 60 ==> [2,2,3,5]
--   primeFactors 13 ==> [13]

primeFactors :: Integer -> [Integer]
primeFactors n = go n 2
  where
    go m k
      | m <= 1        = []
      | m `mod` k == 0 = k : go (m `div` k) k
      | otherwise     = go m (k + 1)

------------------------------------------------------------------------------
-- Ex 19: implement coprime that checks if two numbers are coprime
-- (their gcd is 1).
--
-- Examples:
--   coprime 9 28 ==> True
--   coprime 12 18 ==> False

coprime :: Integer -> Integer -> Bool
coprime = undefined

------------------------------------------------------------------------------
-- Ex 20: implement totient using Euler’s totient function.
-- It returns how many numbers from 1..n are coprime with n.
--
-- Examples:
--   totient 9 ==> 6
--   totient 10 ==> 4

totient :: Integer -> Integer
totient = undefined

------------------------------------------------------------------------------
-- Ex 22: implement removeChar that removes all occurrences of a
-- character from a string.
--
-- Examples:
--   removeChar 'a' "banana" ==> "bnn"
--   removeChar 'x' "hello" ==> "hello"

removeChar :: Char -> String -> String
removeChar = undefined

------------------------------------------------------------------------------
-- Ex 23: implement isPalindrome that checks whether a string reads
-- the same forwards and backwards.
--
-- Examples:
--   isPalindrome "racecar" ==> True
--   isPalindrome "hello" ==> False

isPalindrome :: String -> Bool
isPalindrome = undefined

------------------------------------------------------------------------------
-- Ex 24: implement longestPrefixOfSpaces that counts how many spaces
-- appear at the start of a string.
--
-- Examples:
--   longestPrefixOfSpaces "   hello" ==> 3
--   longestPrefixOfSpaces "hi" ==> 0

longestPrefixOfSpaces :: String -> Int
longestPrefixOfSpaces = undefined

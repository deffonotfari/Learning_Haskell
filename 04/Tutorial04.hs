module Tutorial04 where
import Data.Char
import Data.List

-- This function squares each integer in a list:
squareAll :: [Int] -> [Int]
squareAll ns = [n^2 | n <- ns]

-- gives a definition of the function that returns
-- the input list with the lower case letters capitalised
capitalize :: String -> String
capitalize cs = [toUpper c | c <- cs]

-- Generalizing the previous part, write a function
-- that does the same, but discards non-letters. You may wish to use the following function from the Data.Char module:
capitalizeLetters :: String -> String
capitalizeLetters cs = [toUpper c | c <- cs, isAlpha c]

-- Write a function hat takes a string consisting of words, and returns a string of the same words in the reverse order
-- For example, backwards "This is a string" should return "string a is This".

backwards :: String -> String
backwards bk = unwords(reverse(words bk))

-- Write a function of the same type that takes a string consisting of words, and returns a string of the same words in the same order, but with each word reversed. 
-- For example, reverseEach "This is a string" should return "sihT si a gnirts".
revwords :: String -> String
revwords s = unwords [reverse w | w <- words s]

--Write a function returning the list of numbers that evenly divide the given number, e.g. divisors 12 should be [1,2,3,4,6,12].
divisors :: Int -> [Int]
divisors n = [s | s <- [1..n], n `mod` s == 0]

-- Write a function that computes the average of a list of numbers.
average :: [Double] -> Double
average l = sum l / fromIntegral(length l)

-- Write a function to test whether a word is a palindrome. (There’s no need for a list comprehension here.)
palindrome :: String -> Bool
palindrome w 
    | reverse w == w    = True
    | otherwise         = False

-- Some famous palindromes rely on ignoring non-letters and the distinction between upper and lower case (e.g. “Madam, I’m Adam”). Write another function to test for this.
palindrome2 :: String -> Bool
palindrome2 w
    | palindrome (capitalizeLetters w)      = True
    | otherwise                             = False

-- Write a function that tests whether one word is a rearrangement of the other.
anagram :: [Char] -> [Char] -> Bool
anagram a b 
    | sort a == sort b      = True
    | otherwise             = False

frequency :: [Char] -> [(Char, Int)]
frequency ws = [(head g, length g) |g <- group(sort ws)]

-- Consider how you would write a function
-- that returns True if some re-arrangement of the input string (assumed to be all lowercase letters) is a palindrome. 
-- We could do this by generating all the re-arrangements of the input string, but there will be a lot of them, and there is another approach. For example,
-- The function should return True for "", "a", "aa", "aaa", "aabba" and "ababc".
-- It should return False for "abc" and "aaabbc".
-- That is because while "ababc" can be re-arranged as the palindromes "abcba" or "bacab", that is not possible for "abc" or "aaabbc".
palindromic :: [Char] -> Bool
palindromic xs = length [x | (x, n) <- frequency xs, odd n] <= 1


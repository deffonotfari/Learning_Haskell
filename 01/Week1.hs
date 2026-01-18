module Week1 where

size :: Integer
size = 12+13

-- The square of an integer.
square :: Integer -> Integer
square n = n*n

-- Triple an integer.
triple :: Integer -> Integer
triple n = 3*n


squareOfTriple :: Integer -> Integer
squareOfTriple n = square (triple n)

secondsInAWeek:: Integer -> Integer
secondsInAWeek n = n*24*60*60

phi :: Double
phi = (1 + sqrt(5))/2

milesToKm :: Double -> Double
milesToKm n = n * 1.609344

kmToMiles :: Double -> Double
kmToMiles n = n / 1.609344

tripleOfSquare:: Integer -> Integer
tripleOfSquare n = triple (square n)

fourthPower:: Integer -> Integer
fourthPower n = square (square n)

factorial :: Integer -> Integer
factorial n = product[1..n]

norm :: Double -> Double -> Double
norm x y = sqrt((x*x)+ (y*y))


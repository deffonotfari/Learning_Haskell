-- Exercise set 5a
--
-- * defining algebraic datatypes
-- * recursive datatypes

module Set5a where
import GHC.IO.Encoding.Failure (CodingFailureMode(TransliterateCodingFailure))

------------------------------------------------------------------------------
-- Ex 1: Define the type Vehicle that has four constructors: Bike,
-- Bus, Tram and Train.
--
-- The constructors don't need any fields.
data Vehicle = Bike | Bus | Tram | Train
  deriving (Show)

------------------------------------------------------------------------------
-- Ex 2: Define the type BusTicket that can represent values like these:
--  - SingleTicket
--  - MonthlyTicket "January"
--  - MonthlyTicket "December"
data BusTicket = SingleTicket | MonthlyTicket String
  deriving (Show)

------------------------------------------------------------------------------
-- Ex 3: Here's the definition for a datatype ShoppingEntry that
-- represents an entry in a shopping basket. It has an item name (a
-- String), an item price (a Double) and a count (an Int). You'll also
-- find two examples of ShoppingEntry values.
--
-- Implement the functions totalPrice and buyOneMore below.

data ShoppingEntry = MkShoppingEntry String Double Int
  deriving Show

threeApples :: ShoppingEntry
threeApples = MkShoppingEntry "Apple" 0.5 3

twoBananas :: ShoppingEntry
twoBananas = MkShoppingEntry "Banana" 1.1 2

-- totalPrice should return the total price for an entry
--
-- Hint: you'll probably need fromIntegral to convert the Int into a
-- Double
--
-- Examples:
--   totalPrice threeApples  ==> 1.5
--   totalPrice twoBananas   ==> 2.2

totalPrice :: ShoppingEntry -> Double
totalPrice (MkShoppingEntry _ price count) =
  price * fromIntegral count

-- buyOneMore should increment the count in an entry by one
--
-- Example:
--   buyOneMore twoBananas    ==> MkShoppingEntry "Banana" 1.1 3

buyOneMore :: ShoppingEntry -> ShoppingEntry
buyOneMore (MkShoppingEntry item price count) = MkShoppingEntry item price (count + 1)

------------------------------------------------------------------------------
-- Ex 4: define a datatype Person, which should contain the age (an
-- Int) and the name (a String) of a person.
--
-- Also define a Person value fred, and the functions getAge, getName,
-- setAge and setName (see below).

data Person = Person String Int
  deriving Show

-- fred is a person whose name is Fred and age is 90
fred :: Person
fred = Person "Fred" 90

-- getName returns the name of the person
getName :: Person -> String
getName (Person name _) = name  

-- getAge returns the age of the person
getAge :: Person -> Int
getAge (Person _ age) = age

-- setName takes a person and returns a new person with the name changed
setName :: String -> Person -> Person
setName name (Person _ age) = Person name age

-- setAge does likewise for age
setAge :: Int -> Person -> Person
setAge age (Person name _) = Person name age

------------------------------------------------------------------------------
-- Ex 5: define a datatype Position which contains two Int values, x
-- and y. Also define the functions below for operating on a Position.
--
-- Examples:
--   getY (up (up origin))    ==> 2
--   getX (up (right origin)) ==> 1

data Position = Position Int Int 

-- origin is a Position value with x and y set to 0
origin :: Position
origin = Position 0 0

-- getX returns the x of a Position
getX :: Position -> Int
getX (Position x _) = x

-- getY returns the y of a position
getY :: Position -> Int
getY (Position _ y) = y

-- up increases the y value of a position by one
up :: Position -> Position
up (Position x y) = Position x (y+1)

-- right increases the x value of a position by one
right :: Position -> Position
right (Position x y) = Position (x+1) y

------------------------------------------------------------------------------
-- Ex 6: Here's a datatype that represents a student. A student can
-- either be a freshman, a nth year student, or graduated.
-- Implement the function study, which changes a Freshman into a 1st
-- year student, a 1st year student into a 2nd year student, and so
-- on. A 7th year student gets changed to a graduated student. A
-- graduated student stays graduated even if he studies.

data Student = Freshman | NthYear Int | Graduated
  deriving (Show,Eq)

study :: Student -> Student
study Freshman      = NthYear 1
study (NthYear n)
  | n < 7     = NthYear (n + 1)
  | otherwise = Graduated
study Graduated     = Graduated


------------------------------------------------------------------------------
-- Ex 7: define a datatype UpDown that represents a counter that can
-- either be in increasing or decreasing mode. Also implement the
-- functions zero, toggle, tick and get below.
--
-- NB! Define _two_ constructors for your datatype (feel free to name the
-- constructors however you want)
--
-- Examples:
--
-- get (tick zero)
--   ==> 1
-- get (tick (tick zero))
--   ==> 2
-- get (tick (tick (toggle (tick zero))))
--   ==> -1

data UpDown = Up Int | Down Int
  deriving Show

-- zero is an increasing counter with value 0
zero :: UpDown
zero = Up 0

-- get returns the counter value
get :: UpDown -> Int
get (Up n)  = n
get (Down n) = n

-- tick increases an increasing counter by one or decreases a
-- decreasing counter by one
tick :: UpDown -> UpDown
tick (Up n) = Up (n+1)
tick (Down n) = Down (n-1)

-- toggle changes an increasing counter into a decreasing counter and
-- vice versa
toggle :: UpDown -> UpDown
toggle (Up n) = Down n
toggle (Down n) = Up n
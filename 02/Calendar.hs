module Calendar where

--Define an enumerated type Month for representing the months of the year.
data Month
    = Jan | Feb | Mar | Apr | May | Jun
    | Jul | Aug | Sep | Oct | Nov | Dec
    deriving Show


isLeapYear :: Int -> Bool
isLeapYear y
  | y `mod` 400 == 0 = True
  | y `mod` 100 == 0 = False
  | y `mod` 4   == 0 = True
  | otherwise        = False


daysInYear :: Int -> Int
daysInYear year
    | isLeapYear year   = 366
    | otherwise         = 365

daysInMonth :: Month -> Int -> Int
daysInMonth Jan y = 31
daysInMonth Feb y
  | isLeapYear y  = 29
  | otherwise     = 28
daysInMonth Mar y = 31
daysInMonth Apr y = 30
daysInMonth May y = 31
daysInMonth Jun y = 30
daysInMonth Jul y = 31
daysInMonth Aug y = 31
daysInMonth Sep y = 30
daysInMonth Oct y = 31
daysInMonth Nov y = 30
daysInMonth Dec y = 31
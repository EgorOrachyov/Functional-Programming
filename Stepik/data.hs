import Data.Char
import Data.List

data Color = Red | Green | Blue

instance Show Color where
    show Red = "Red"
    show Green = "Green"
    show Blue = "Blue"


stringToColor :: String -> Color
stringToColor "Red" = Red
stringToColor "Green" = Green
stringToColor "Blue" = Blue


charToInt :: Char -> Int
charToInt c 
    | c >= '0' && c <= '9' = digitToInt c


emptyOrSingleton :: Bool -> a -> [a]
emptyOrSingleton False _ = []
emptyOrSingleton True x = [x]

isEqual :: (Eq a, Eq b) => (a, b) -> (a, b) -> Bool
isEqual (a, b) (a', b') = a == a' && b == b'

{-

data LogLevel = Error | Warning | Info

cmp :: LogLevel -> LogLevel -> Ordering
cmp Error Error = EQ
cmp Error Warning = GT
cmp Error Info = GT
cmp Warning Warning = EQ
cmp Warning Info = GT
cmp Warning Error = LT
cmp Info Info = EQ
cmp Info Error = LT
cmp Info Warning = LT

-}

{-

processData :: SomeData -> String
processData sd = case doSomeWork sd of
    (x,0) -> "Success"
    (_,n) -> "Fail: " ++ (show n)  

-}

-----------------------------------------------

{-

data Result' = Done Result | Failure Int

instance Show Result' where
    show (Done r)    = "Success"
    show (Failure n) = "Fail: " ++ (show n)

doSomeWork' :: SomeData -> Result'
doSomeWork' ds = case doSomeWork ds of
    (r,0) -> Done r
    (_,n) -> Failure n

-}

{-

data Point = Point Double Double

origin :: Point
origin = Point 0.0 0.0

distanceToOrigin :: Point -> Double
distanceToOrigin (Point x y) = sqrt (x ^ 2 + y ^ 2)

distance :: Point -> Point -> Double
distance (Point x0 y0) (Point x1 y1) = distanceToOrigin (Point (x0 - x1) (y0 - y1)) 

-}

data Shape = Circle Double | Rectangle Double Double

area :: Shape -> Double
area (Circle r) = pi * r * r
area (Rectangle a b) = a * b 


square :: Double -> Shape
square a = Rectangle a a

isSquare :: Shape -> Bool
isSquare (Rectangle a b) = (a == b)
isSquare _ = False 


data Bit = Zero | One deriving (Show)
data Sign = Minus | Plus
data Z = Z Sign [Bit]

fromBits :: [Bit] -> Integer
fromBits x = helper x 1 where
    helper :: [Bit] -> Integer -> Integer
    helper (Zero:xs) acc = helper xs (acc * 2)
    helper (One:xs) acc = acc + (helper xs (acc * 2))
    helper [] _ = 0

toBits :: Integer -> [Bit]
toBits = unfoldr 
    (\x -> if (x <= 0) 
                then Nothing 
                else Just (if (x `mod` 2 == 0) then Zero else One, x `div` 2))

toIntegerFromZ :: Z -> Integer
toIntegerFromZ (Z Minus x) = (-1) * (fromBits x)
toIntegerFromZ (Z Plus x) = fromBits x

fromIntegerToZ :: Integer -> Z
fromIntegerToZ i 
    | i > 0 = Z Plus (toBits i)
    | i < 0 = Z Minus (toBits (-i))
    | otherwise = Z Plus []

add :: Z -> Z -> Z
add a b = fromIntegerToZ $ (toIntegerFromZ a) + (toIntegerFromZ b) 

mul :: Z -> Z -> Z
mul a b = fromIntegerToZ $ (toIntegerFromZ a) * (toIntegerFromZ b)  

-----------------------------------------------

{-

import Data.Time.Clock
import Data.Time.Format
import System.Locale

timeToString :: UTCTime -> String
timeToString = formatTime defaultTimeLocale "%a %d %T"

data LogLevel = Error | Warning | Info deriving (Show)

data LogEntry = LogEntry { timestamp :: UTCTime, logLevel :: LogLevel, message :: String }

logLevelToString :: LogLevel -> String
logLevelToString = show

logEntryToString :: LogEntry -> String
logEntryToString entry = 
        (timeToString (timestamp entry)) ++ ": " ++
        (logLevelToString (logLevel entry)) ++ ": " ++
        (message entry)   

-}

data Person = Person { firstName :: String, lastName :: String, age :: Int }

updateLastName :: Person -> Person -> Person
updateLastName p1 p2 = Person (firstName p2) (lastName p1) (age p2)

abbrFirstName :: Person -> Person
abbrFirstName p@(Person fn ln ag) 
    | length fn > 2 = Person ([(head fn)] ++ ".") ln ag
    | otherwise = p


isRectangle :: Shape -> Bool
isRectangle Rectangle{} = True
isRectangle _ = False

-----------------------------------------------

data Coord a = Coord a a deriving (Show)

distance :: Coord Double -> Coord Double -> Double
distance (Coord x y) (Coord z w) = sqrt ((x - z) ^ 2 + (y - w) ^ 2)  

manhDistance :: Coord Int -> Coord Int -> Int
manhDistance (Coord x y) (Coord z w) = abs (x - z) + abs (y - w) 


getCenter :: Double -> Coord Int -> Coord Double
getCenter l (Coord x y) = Coord (l * ((toEnum x) + 0.5)) (l * ((toEnum y) + 0.5))

getCell :: Double -> Coord Double -> Coord Int
getCell l (Coord x y) = Coord (ceiling (x / l) - 1) (ceiling (y / l) - 1)


findDigit :: [Char] -> Maybe Char
findDigit [] = Nothing
findDigit (x:xs)
    | isDigit x = Just x
    | otherwise = findDigit xs


findDigitOrX :: [Char] -> Char
findDigitOrX s = case findDigit s of 
    Just c -> c
    Nothing -> 'X'


maybeToList :: Maybe a -> [a]
maybeToList (Just a) = [a]
maybeToList (Nothing) = []

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x 

-----------------------------------------------

main :: IO()
main = print ()





{-# LANGUAGE ScopedTypeVariables #-}
import Control.Monad.State
import Data.Char

main :: IO ()
main = do
    --count <- getLine >>= \x -> return (read x :: Int)
    --result <- execStateT (process count) (("Nobody", -1), ("Nobody", 2000000))
    print $ parse $ reverse "234/34/54" 
    --print result
    print ""

type Person = (String, Int)
type RecordState = (Person, Person)


process ::  Int -> StateT RecordState IO ()
process 0     = do
    return ()
process count = do
    line <- liftIO $ getLine
    let name : date : _ = words line
        age             = read date :: Int
    get >>= \person -> put $ record person (name, age)
    process (count - 1)

parse :: String -> Int
parse []     = 0
parse (x:xs)
    | isDigit x = digitToInt x + 10 * parse xs
    | otherwise = parse xs
    
record :: RecordState -> Person -> RecordState 
record ((max_guy, max), (min_guy, min)) (name, age)
    | age > max = ((name, age), (min_guy, min))
    | age < min = ((max_guy, max), (name, age))
         
            
{-    line <- getLine
    let y : m : d : _ = map read $ words line :: [Int]
    print $ dayFromAd y m d
    -}
    
dayFromAd :: Int -> Int -> Int -> Int
dayFromAd y m d = countDayOfYear y + thDayOfThisYear y m d

countDayOfYear :: Int -> Int
countDayOfYear 0 = 0
countDayOfYear y = dayOfYear (y - 1) + countDayOfYear (y - 1)

thDayOfThisYear :: Int -> Int -> Int -> Int
thDayOfThisYear _ 1 d = d
thDayOfThisYear y m d = dayOfMonth y (m - 1) + thDayOfThisYear y (m - 1) d

dayOfYear :: Int -> Int
dayOfYear y
    | isLeapYear y = 366
    | otherwise    = 365

  
isLeapYear y = if ((y `mod` 4 == 0 && y `mod` 100 /= 0) || y `mod` 400 == 0)
                   then True
                   else False


dayOfMonth :: Int -> Int -> Int
dayOfMonth y m
    | m == 1 = 31
    | m == 2 = if isLeapYear y then 29 else 28
    | m == 3 = 31
    | m == 4 = 30
    | m == 5 = 31
    | m == 6 = 30
    | m == 7 = 31
    | m == 8 = 31
    | m == 9 = 30
    | m == 10 = 31
    | m == 11 = 30
    | m == 12 = 31
              
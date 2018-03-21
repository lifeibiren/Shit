{-# LANGUAGE ScopedTypeVariables #-}
import Data.Char
import Text.Printf

type Person = (String, Int)
type RecordState = (Person, Person, Int)

main :: IO ()
main = do
    count <- getLine >>= \x -> return (read x :: Int)
    ((youngest, _), (oldest, _), num) <- process count (("Nobody", -1), ("Nobody", 20170906), 0)
    printf "%d %s %s" num oldest youngest

process ::  Int -> RecordState -> IO RecordState
process 0     stats = return stats
process count stats = do
    line <- getLine
    let [name, date] = words line
        age          = parse . reverse $ date
    if valid 20140906 18140906 age
       then process (count - 1) $ record stats (name, age)
       else process (count - 1) stats

parse :: String -> Int
parse []        = 0
parse (x:xs)
    | isDigit x = digitToInt x + 10 * parse xs
    | otherwise = parse xs
    
record :: RecordState -> Person -> RecordState 
record ((max_guy, max), (min_guy, min), count) (name, age)
    | age > max = ((name, age), (min_guy, min), count + 1)
    | age < min = ((max_guy, max), (name, age), count + 1)
    | otherwise = ((max_guy, max), (min_guy, min), count + 1)

valid :: Int -> Int -> Int -> Bool
valid max min x
    | x <= max && x >= min = True
    | otherwise            = False
    

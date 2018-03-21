{-# LANGUAGE ScopedTypeVariables #-}
import Control.Monad.State
import Data.Char
import Text.Printf

type Person = (String, Int)
type RecordState = (Person, Person, Int)

main :: IO ()
main = do
    count <- getLine >>= \x -> return (read x :: Int)
    result <- execStateT (process count) (("Nobody", -1), ("Nobody", 20000001), 0)
    let ((youngest, _), (oldest, _), num) = result
    printf "%d %s %s" num oldest youngest

process ::  Int -> StateT RecordState IO ()
process 0     = do
    return ()
process count = do
    line <- liftIO $ getLine
    let [name, date] = words line
        age          = parse . reverse $ date
    if valid 20140906 18140906 age
       then do stats <- get
               put $ record stats (name, age)
               process (count - 1)
       else process (count - 1)

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
    

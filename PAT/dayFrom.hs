{-# LANGUAGE ScopedTypeVariables #-}
main :: IO ()
main = do
    line <- getLine
    let y : m : d : _ = map read $ words line :: [Int]
    print $ dayFrom 2000 1 1 y m d
    
dayFrom :: Int -> Int -> Int -> Int -> Int -> Int -> Int
dayFrom y0 m0 d0 y1 m1 d1 = countDayOfYear y0 y1 + thDayOfThisYear y1 m1 d1 - thDayOfThisYear y0 m0 d0

countDayOfYear :: Int -> Int -> Int
countDayOfYear y0 y1
    | y0 == y1  = 0
    | otherwise = dayOfYear (y1 - 1) + countDayOfYear y0 (y1 - 1)

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
              

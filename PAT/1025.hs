import Text.Printf
import qualified Data.IntMap.Strict as IntMap
main = do
    (start, num, reverse, list) <- get_input
    print(list)
    print(after start 1 list)
   -- print(reverse_list start reverse list)
    
get_val (val, _) = val
get_next (_, next) = next

get_input :: IO (Int, Int, Int, IntMap.IntMap (Int, Int))
get_input = do 
    contents <- readFile "input.txt"
    let input = map read $ words contents :: [Int]
    let start : val : reverse : raw_list = input
    printf "start=%d val=%d reverse=%d\n" start val reverse
    print_list raw_list
    return (start, val, reverse, make_map raw_list)
        where make_map (addr:val:next:list) = IntMap.insert addr (val, next) $ make_map list
              make_map []                   = IntMap.empty
    
    --print $ head int_list
    --mapM print list

print_list :: [Int] -> IO ()
print_list (addr:val:next:xs) = do
    if next /= -1
        then printf "addr=%05d data=%d next=%05d\n" addr val next
        else printf "addr=%05d data=%d next=%d\n" addr val next
    print_list xs
print_list [] = return ()


reverse_list :: Int -> Int -> IntMap.IntMap (Int, Int) -> IntMap.IntMap (Int, Int)
reverse_list start reverse list = IntMap.union first_segment following_segment
    where (first_segment, next) = reverse_segment start reverse list
          following_segment = reverse_list next reverse list

reverse_segment :: Int -> Int -> IntMap.IntMap (Int, Int) -> (IntMap.IntMap (Int, Int), Int)
reverse_segment start reverse list = (last result, next)
    where (result, next) = do_reverse start reverse list 0
          last = IntMap.insert start (get_val $ list IntMap.! start, after next (reverse - 1) list)

do_reverse :: Int -> Int -> IntMap.IntMap (Int, Int) -> Int -> (IntMap.IntMap (Int, Int), Int)
do_reverse start reverse list left = (IntMap.empty, left)

after :: Int -> Int -> IntMap.IntMap (Int, Int) -> Int
after it time list
    | time == 0 = it
    | otherwise = after (get_next (list IntMap.! it)) (time - 1) list

--do printf "%05d %d %d" start, get_val elem, get_next elem)
 --                                            where elem = list ! start
                                       
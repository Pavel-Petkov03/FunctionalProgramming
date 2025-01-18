collectUntilgreater :: Int -> [Int] -> [Int]-> [[Int]]
collectUntilgreater x (y:xs) mem =
    if y >= x then mem : collectUntilgreater y xs [y] else
        collectUntilgreater y xs (mem ++ [y])
    

segments :: [Int] -> [[Int]]
segments (x:xs) = collectUntilgreater x xs [x]

fillSegments :: [Int] -> [Int]
fillSegments xs = concat [[head fs, head fs - 1 .. 0]|  fs <- segments xs]


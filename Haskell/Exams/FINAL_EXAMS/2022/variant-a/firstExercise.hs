-- няма да решавам на този вариант 2,3,4 задача , защото ги реших в б вариант и разликата 
-- е точно една дума или никаква 



calculateSum :: [Int] -> Int
calculateSum [] = 0
calculateSum [n] = n * n
calculateSum (x:nums) =
     x * last nums + calculateSum (init nums)


somosRecurse :: [Int] -> Int -> [Int]
somosRecurse [] _ = []
somosRecurse (x:xs) k = currentSum : somosRecurse (xs ++ [currentSum]) k
    where
        wantedNumbers = [xs !! index | index <- [0..k-2]]
        currentSum = calculateSum wantedNumbers `div` x

somos :: Int -> [Int]
somos k = generatedOnes ++ somosRecurse generatedOnes k
    where
        generatedOnes = [1 | x <- [1..k]]



findN :: Int -> Int
findN k = findNRecurse generatedOnes k 0
    where
        generatedOnes = [1 | x <- [1..k]]

findNRecurse :: [Int] -> Int -> Int -> Int
findNRecurse (x:xs) k n
    | currentSum `rem` x == 0 = findNRecurse (xs ++ [toAppend]) k (n + 1)
    | otherwise = n
    where
        wantedNumbers = [xs !! index | index <- [0..k-2]]
        currentSum = calculateSum wantedNumbers
        toAppend = currentSum `div` x

-- идеята на бонуса е че хващаме не цяло число само когато делим
-- тоест когато rem е /= 0
-- сиг може да се напише с по - малко copy paste ама го оставям така
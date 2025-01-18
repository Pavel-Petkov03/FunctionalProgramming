
isPrimeRecurse :: Int -> Int -> Bool
isPrimeRecurse cur n
    | cur == n = True
    | n `mod` cur == 0 = False
    | otherwise = isPrimeRecurse (cur + 1) n



isPrime :: Int -> Bool
isPrime = isPrimeRecurse 2 
-- todo решето на ератостен

merge :: [Int] -> [Int] -> [Int]
merge (x:xs) (y:ys)
    | x < y = x : merge xs (y : ys)
    | x > y = y : merge (x : xs) ys
    | otherwise =  x : merge xs ys

mergeAll :: [[Int]] -> [Int]
mergeAll (xs:ys:restxx) = 
    merge xs ys ++ mergeAll restxx

primitive :: Int -> [Int]
primitive 1 = [ x |  x <- [2..] , isPrime x]
primitive n = mergeAll [map (*prime) (primitive (n -1)) | prime <- primitive 1]

-- това решение е с бонуса
-- ако някой чете това :) .Ако има такава задача и пише сортирайте 
--и махнете дубликатите на безкраен поток винаги се прави merge, в случая трябва и
-- mergeAll защото merge се прави безкраен брой пъти (реално не се защото викаме take като тестваме)


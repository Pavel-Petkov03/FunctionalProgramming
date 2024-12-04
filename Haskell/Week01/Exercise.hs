myAbs :: Int -> Int
myAbs x = if x > 0 then x else (-x)

-- >>> myAbs (-5)
-- 5


isTriangle :: (Ord a, Num a) => a -> a -> a -> Bool
isTriangle a b c = (a + b > c) && (a + c > b) && (c + b > a)
--- (Ord a , Num a) => се нарича type class constraint и казва че 
--- вариатичния клас а трябва да поддържа събиране и сравнение


-- >>> isTriangle 3 4 5
-- True

isPrime :: Int -> Bool
isPrime n
    | n <= 1    = False
    | otherwise = not (any (\d -> n `mod` d == 0) [2,5..limit])
    where
        limit = floor (sqrt (fromIntegral n))

-- >>> isPrime 31
-- True

sumDivisors :: Int -> Int
sumDivisors x = foldr (\current acc -> if x `mod` current == 0 then acc + current else acc) 0 [1..x]

sumDivisors2 :: Int -> Int
sumDivisors2 n = sumDivisors2Helper n 1 0
    where 
        sumDivisors2Helper :: Int -> Int -> Int -> Int
        sumDivisors2Helper n current acc
            | current > n = acc
            | n `mod` current == 0 = sumDivisors2Helper n (current + 1) (acc + current)
            | otherwise = sumDivisors2Helper n (current + 1) acc

--- >>> sumDivisors2 3
-- 4

isPerfect :: Int -> Bool
isPerfect x = sumDivisors x - x == x

--- >>> isPerfect 27
-- False

countBinaryDigits :: Int -> Int
countBinaryDigits 0 = 1
countBinaryDigits n = 1 + countBinaryDigitsRecurse (n `div` 2)
    where 
        countBinaryDigitsRecurse :: Int -> Int
        countBinaryDigitsRecurse 0 = 0
        countBinaryDigitsRecurse k = 1 + countBinaryDigitsRecurse (k `div` 2)

--- >>> countBinaryDigits 4
-- 3

isEvil :: Int -> Bool
isEvil x = even (isEvilRecurse x)
    where 
        isEvilRecurse :: Int -> Int
        isEvilRecurse 0 = 0
        isEvilRecurse x =  (if even x then 0 else 1) + isEvilRecurse (div x 2)

--- >>> isEvil 5
-- True

sumEvil :: Int -> Int -> Int
sumEvil a b = foldr (\x acc -> if isEvil x then acc + 1 else acc) 0 [a..b]


sumEvil2 :: Int -> Int -> Int
sumEvil2 a b = sumEvil2Recurse a b 0
    where
        sumEvil2Recurse :: Int -> Int -> Int -> Int
        sumEvil2Recurse a b acc = if a > b then acc 
        else sumEvil2Recurse (a + 1) b (if isEvil a then acc + 1 else acc)

--- >>> sumEvil2 3 5
-- 2

compose :: (t1 -> t2) -> (t3 -> t1) -> t3 -> t2
compose f g x = f (g x)


--- >>> compose not not True
-- True




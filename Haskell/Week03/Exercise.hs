import Data.Char
whisper :: String -> String
whisper = map toUpper
-- >>> whisper "banana"
-- "BANANA"


removeSpaces  :: String -> String
removeSpaces = filter (not . isSpace)
-- >>> removeSpaces "The Sound And The Fury"
-- "TheSoundAndTheFury"

-- тази техника да не подавам str а haskell имплицитно да си го подава се нарича point-free style

switchCaps :: String -> String
switchCaps = map (\x -> if isUpper x then toLower x else toUpper x)


-- >>> switchCaps "baNaNA"
-- "BAnAna"

encypt :: Int -> String -> String
encypt n = map (\x -> chr (ord x + n))

-- >>> encypt 1 "ABC"
-- "BCD"


decrypt :: Int -> String -> String
decrypt n = map (\x -> chr (ord x - n))

-- >>> decrypt 1 "BCD"
-- "ABC"

joinWords :: Char -> [String] -> String
joinWords _ [] = ""
joinWords ch (x:xs) = foldr (\y acc -> y ++ [ch] ++ acc) x xs


-- >>> joinWords ' ' ["The", "Sound", "of", "Silence"]
-- "Sound of Silence The"




indices :: (Eq a) => a -> [a] -> [Int]
indices elem xs = extractResult elem xs 0
    where
        extractResult :: (Eq a) => a -> [a] -> Int -> [Int]
        extractResult _ [] _ = []
        extractResult elem (x:xs) index =
            if elem /= x then extractResult elem xs (index + 1)
            else index : extractResult elem xs (index + 1)

-- >>> indices 1 [1, 2, 3, 1, 4]
-- >>> indices 1 []    
-- [0,3]
-- []

lastIndex :: (Eq a) => a -> [a] -> Int
lastIndex elem xs =
    let indexArray = indices elem xs
    in findLast indexArray
    where
        findLast :: [Int] -> Int
        findLast [x] = x
        findLast [] = error "No index found"
        findLast (x:xs) = findLast xs

-- >>> lastIndex 1 [1, 2, 3, 1, 4]
-- >>> lastIndex 1 []    
-- 3
-- No index found

countMin :: [Int] -> Int
countMin xs =
    let min = minimum xs
    in length (filter (== min) xs)

-- >>> countMin [1, 2, 3, 1, 4]
-- >>> countMin []    
-- 2
-- 0


dedup :: (Eq a) =>  [a] -> [a]
dedup xs =
    dedupRecurse xs []
    where
        dedupRecurse :: (Eq a) =>  [a] -> [a] -> [a]
        dedupRecurse [] ys = ys
        dedupRecurse (x:xs) ys =
            if x `elem` ys then dedupRecurse xs ys
            else dedupRecurse xs (ys ++ [x])


-- >>> dedup [1, 2, 1]
-- >>> dedup [1, 3, 7, 3, 5, 1]
-- [1,2]
-- [1,3,7,5]
-- това е доста бавно

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) = if x < y then x : merge xs (y:ys) else y : merge (x:xs) ys

-- >>> merge [1, 3, 7] [2, 4, 6]
-- [1,2,3,4,6,7]

mergeSort :: (Ord a) => [a] -> [a]
mergeSort [x] = [x]
mergeSort xs =
    merge leftPart rightPart
    where
        middle = length xs `div` 2
        leftPart =  mergeSort (take middle xs)
        rightPart =  mergeSort (drop middle xs)


-- >>> mergeSort [2, 1, 3, 7, -16, 5, -33]
-- [-33,-16,1,2,3,5,7]


subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x:xs) = subsets xs ++ map (x:) (subsets xs)


-- >>> subsets [1,2,3]
-- [[],[3],[2],[2,3],[1],[1,3],[1,2],[1,2,3]]


pick :: Int -> [a] -> [[a]]
pick _ [] = []
pick 0 _ = [[]]
pick 1 [x] = [[x]]
pick n (x:xs) = pick n xs ++ map (x:) (pick (n -1) xs)

-- >>> pick 2 [1,2,3]
-- [[2,3],[1,3],[1,2]]



maximize :: (Ord a) => [a -> a] -> a -> a
maximize (f:fs) x = foldr (\g acc -> if g x >= acc x then g else acc) f fs x

-- >>> maximize [(\x -> x ** 3), (\x -> x + 1)] 0.5
-- 1.5
-- >>> maximize [(\x -> x ** 3), (\x -> x + 1), (\x -> x ** 4)] (-2)
-- 16.0



compose :: [a -> a] -> a -> a
compose fs x = foldr (\f x -> f x) x fs

-- >>> compose [(+1), (2*)] 7
-- 15

facts :: [Int]
facts = generateFacts 1 1
    where
        generateFacts :: Int -> Int -> [Int]
        generateFacts prev n =
            (prev * n) : generateFacts  (prev * n) (n + 1)


-- >>> take 10 facts
-- [1,2,6,24,120,720,5040,40320,362880,3628800]


points :: [(Int , Int)]
points = [ (x, z - x) | z <- [0..], x <- [0..z] ]


-- >>> take 10 points
-- [(0,0),(0,1),(1,0),(0,2),(1,1),(2,0),(0,3),(1,2),(2,1),(3,0)]


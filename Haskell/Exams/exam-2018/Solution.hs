customNub :: Eq a => [a] -> [a]
customNub = nubHelper []
  where
    nubHelper _ [] = []
    nubHelper seen (x:xs)
      | x `elem` seen = nubHelper seen xs
      | otherwise     = x : nubHelper (x:seen) xs

generatePowers :: Integer -> Integer -> [Integer]
merge :: [Integer] -> [Integer] -> [Integer]
merge (l:ls) (r:rs) =
    if l < r then l : merge ls (r:rs)
    else r : merge (l:ls) rs

generatePowers k t = customNub $  1 : generatePowersRecurse [k^x | x <- [1..]] [t^x | x <- [1..]]
    where
        generatePowersRecurse (l:lstream) (r:rstream) =
            if l < r then
                l : generatePowersRecurse lstream (merge (map (*l) (r:rstream)) (r:rstream))
            else r : generatePowersRecurse (merge (map (*r) (l:lstream)) (l:lstream)) rstream



-- >>> take 10 (generatePowers 2 3)
-- [1,2,3,4,6,8,9,12,16,18]

-- когато сортираме безкрайни потоци merge е доста адекватен вариант ngl


data Tree a =
    Node a (Tree a) (Tree a) |
    EmptyTree

countCodes :: Tree Int -> Int
countCodes tree = countCodesRecurse tree 1
    where
    countCodesRecurse (Node value left right) res =
        (if res == value then 1 else 0)
        + countCodesRecurse left (2 * res)
        + countCodesRecurse right (2 * res + 1)
    countCodesRecurse EmptyTree _  = 0


tree :: Tree Int
tree = Node 1
        (Node 2
            EmptyTree
            (Node 3
                EmptyTree
                EmptyTree))
        (Node 4
            (Node 6
                EmptyTree
                EmptyTree)
            EmptyTree)

-- >>> countCodes tree
-- 3

-- тъй като не гоня грам performace
-- мисля просто да генерирам всички възможни масиви
-- като наредени двойки отговор и потенциална прогресия
-- и ако потенциалната прогресия е прогресия
-- я връща
-- sounds stupid tbh


generateTuples :: [[Int]] -> [Int -> Int] -> [[(Int, Int)]]
generateTuples = zipWith (\ ls f -> map (\ el -> (el, f el)) ls)


myConcat :: [[a]] -> [a]
myConcat xs = foldr (++) [] xs

generateResults :: [[(a1, a2)]] -> [([a1], [a2])]
generateResults [] = [([], [])]  -- Base case: a single tuple of empty lists
generateResults (l:ls) =
    myConcat [map (\(ls1, ls2) -> (a : ls1, b : ls2)) (generateResults ls) | (a, b) <- l]

-- >>>  generateResults $ generateTuples [[1,2], [3,4], [5,7]] [(+3), id, (7-)]
-- [([1,3,5],[4,3,2]),([1,3,7],[4,3,0]),([1,4,5],[4,4,2]),([1,4,7],[4,4,0]),([2,3,5],[5,3,2]),([2,3,7],[5,3,0]),([2,4,5],[5,4,2]),([2,4,7],[5,4,0])]

isProgression :: [Int] -> Bool
isProgression (x:y:rest) =
    isProgressionRecurse (y:rest) (y-x)
    where
        isProgressionRecurse (x:y:r) diff =
            (y - x) == diff && isProgressionRecurse (y:r) diff
        isProgressionRecurse [] _ = True
        isProgressionRecurse [x] _ = True


aProg :: [[Int]] -> [Int -> Int] -> [[Int]]
aProg matrix fs = map fst $ filter (\(a,b) -> isProgression b) $  generateResults $ generateTuples matrix fs

-- >>> aProg [[1,2], [3,4], [5,7]] [(+3), id, (7-)]
-- [[1,3,5]]

-- сложил съм да е матрица защото може да са повече



isStronger :: (String, [(String, Integer)]) -> (String, [(String, Integer)]) -> Bool

isStronger (_, ls1) (_ , ls2)= map fst ls2 `contains` map fst ls1 &&
    any (\(name1, val1) -> val1 < snd (head (filter (\(name2, _) -> name1 == name2) ls1))
    ) ls2


contains :: (Eq a) =>[a] -> [a] -> Bool
contains ls1 ls2 = all (`elem` ls2) ls1


l :: [(String, [(String, Integer)])]
l = [("A",[("p",5),("q",3)]),("B",[("p",4),("q",3)]),("C",[("p",3)])]
-- >>>  isStronger (l!!1) (l!!2)
-- >>> 
-- True

-- >>> isStronger (l!!2) (l!!1)
-- False
strongRelation :: [(String, [(String, Integer)])] -> [((String, [(String, Integer)]), [String])]
strongRelation ls = map (\t -> (t, map fst (filter (`isStronger` t) ls))) ls

--- >>> strongRelation l
-- [(("A",[("p",5),("q",3)]),[]),(("B",[("p",4),("q",3)]),["A"]),(("C",[("p",3)]),["A","B"])]






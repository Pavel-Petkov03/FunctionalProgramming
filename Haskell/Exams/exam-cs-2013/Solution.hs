getDistance :: (Double, Double) -> (Double, Double) ->  Double

getDistance (x1, y1) (x2, y2) = sqrt ( (x2 - x1) ^ 2 +  (y2 - y1) ^ 2)

-- >>> getDistance (1,2) (2,2)
-- 1.0

maxDistance :: [(Double, Double)] -> Double
maxDistance ls = maximum [getDistance x1 x2 | x1 <- ls, x2 <- ls]


-- >>> maxDistance [(-1.1, 1), (1.8, 2), (3, 1), (-1, -2)]
-- 5.0


selectionSort :: (Ord a) => [a] -> [a]
selectionSort [] = []
selectionSort ls =
    let current = minimum ls in
        current : selectionSort (filter (/= current) ls)

-- >>> selectionSort [5, 4.4, 3, 6, 1]
-- [1.0,3.0,4.4,5.0,6.0]



extractNeighboors :: [[Int]] -> Int -> [Int]
extractNeighboors g u = tail $ head $ filter (\x -> head x == u) g

removeNeighbors :: Int -> [[Int]] -> [[Int]]
removeNeighbors u = filter (\x -> head x /= u)


isReachable :: [[Int]] -> Int -> Int -> Bool
isReachable graph v1 v2 = isReachableRecurse graph v1 0
    where
        isReachableRecurse :: [[Int]] -> Int -> Int -> Bool
        isReachableRecurse [] _ result = False
        isReachableRecurse graph currentNode res
            | res + currentNode < 0 = False
            | currentNode == v2 = True
            | otherwise = any (\n -> isReachableRecurse (removeNeighbors n graph) n (res + n)) (extractNeighboors graph currentNode)

--представянето ми е матрица, в която първият елемент е ноуда а другите са съседите



matchedBy :: String -> [[String]]
matchedBy str = generate left "" right (last left)
    where
        left = takeWhile (/= '*') str
        right = drop 1 (dropWhile (/= '*') str)
        generate :: String -> String -> String -> Char -> [[String]]
        generate l mid r symbol = 
            [l ++ mid ++ r] : generate l (symbol:mid) r symbol


-- >>> take 10 (matchedBy "Tro0*l")
-- [["Tro0l"],["Tro00l"],["Tro000l"],["Tro0000l"],["Tro00000l"],["Tro000000l"],["Tro0000000l"],["Tro00000000l"],["Tro000000000l"],["Tro0000000000l"]]
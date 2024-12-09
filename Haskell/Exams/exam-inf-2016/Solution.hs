import Data.List (transpose, maximumBy)
import Data.Ord (comparing)

findColumns :: [[Int]] -> Int
findColumns matrix = length $ filter validColumn (transpose matrix)
    where
        validColumn column = any (\row -> all (`elem` row) column) matrix

-- >>> findColumns [[1,4,3],[4,5,6],[7,4,9]]
-- 1

combine :: (a-> b -> c) -> (d -> a) -> (d -> b) -> d -> c
combine h f g x = h  (f x) (g x)


check :: Int -> Int -> [Int -> Int] -> [Int -> Int -> Int] -> Bool
check a b uns bins =
    any match [combine h f g | f <- uns, g <- uns, h <- bins ]
    where
        match func = any (\seekF -> all (\x -> func x == seekF x) [a..b]) uns



-- >>> check 1 9 [(+1),(subtract 1),(subtract 1).(^2)] [(*)]
-- True


garden :: [(String, Int, Int)] -> ((Int, Int), [String])

garden plants =
    ((minTime, maxTime), map (\(a,b,c) -> a) (filter (timeFilter minTime maxTime) plants))
    where
        (minTime, maxTime, _) = extractTimes plants
        extractTimes plants = maximumBy (comparing (\(a,b,c) -> c)) [(start, end, getCount start end) | start <- left, end <- right, start <= end]
        left = map (\(a,b,c) -> b) plants
        right = map (\(a,b,c) -> c) plants
        getCount start end = length (filter (timeFilter start end) plants)
        timeFilter start end (a,b,c) = b <= start && c >= end


-- >>> garden[("peas", 5, 25), ("beans", 3, 15), ("cocoa", 20, 30)]
-- ((20,25),["peas","cocoa"])

-- сто процента има по оптимално решение но не ми се мислеше


maxPath :: [[Int]] -> Int -> [Int]
maxPath graph u = reverse $ maxPathRecurse graph u []
  where
    maxPathRecurse :: [[Int]] -> Int -> [Int] -> [Int]
    maxPathRecurse graph u result
      | u `elem` result = result
      | null neighboors = result
      | otherwise = maximumBy (comparing length) paths
      where
        neighboors = extractNeighboors graph u
        paths = [maxPathRecurse (removeNeighbors u graph) n (u : result) | n <- neighboors]

    extractNeighboors :: [[Int]] -> Int -> [Int]
    extractNeighboors g u = tail $ head $ filter (\x -> head x == u) g

    removeNeighbors :: Int -> [[Int]] -> [[Int]]
    removeNeighbors u = filter (\x -> head x /= u)


-- >>> maxPath [[1,2,4],[2,3],[3,2],[4]] 1
-- [1,2,3]




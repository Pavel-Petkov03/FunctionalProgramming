import Data.List (group, sort, maximumBy, sortBy)
import Data.Ord (comparing)
countingArr :: [Int] -> [(Int, Int)]
countingArr l = [(head a, length a) | a <- group (sort l)]


mFrequent :: [(Int, Int)] -> [Int]
mFrequent ls = map fst (filter (\t -> snd t == maxMimum) ls)
    where
        maxMimum = snd (maximumBy (comparing snd) ls)


getKeyFrequenceList :: [Int] -> [Int]
getKeyFrequenceList  = mFrequent . countingArr

intersect :: Eq a => [a] -> [a] -> [a]
intersect xs ys = [x | x <- xs, x `elem` ys]

mostFrequent :: [[Int]] -> Int
mostFrequent (l:ls) = head $ foldl (\acc cur -> if null (acc `intersect` getKeyFrequenceList cur) then [0] else acc) (getKeyFrequenceList l) ls
-- >>>mostFrequent [[1,1,3,2],[1,5,5],[1,5],[1,1,1,3]] 
-- 0


data Tree a = 
    Node a (Tree a) (Tree a)
    | EmptyTree

grow :: Tree a -> a -> Tree a
grow EmptyTree _ = EmptyTree
grow (Node x EmptyTree EmptyTree) y = Node x (Node y EmptyTree EmptyTree) (Node y EmptyTree EmptyTree) 
grow (Node x left right) y = Node x (grow left y) (grow right y)


growingTrees :: Tree Integer
growingTrees = generateGrowingTrees 0
    where
        generateGrowingTrees :: Integer -> Tree Integer
        generateGrowingTrees n = Node n (generateGrowingTrees $ n + 1) (generateGrowingTrees $ n + 1)



endTime :: (Int, Int) -> Int -> Int
endTime (h, m) duration = (h * 60 + m) + duration

lastShow :: [(String, (Int, Int), Int)] -> String
lastShow shows = let (result, _, _) = maximumBy (comparing (\(_, startTime, duration) -> endTime startTime duration)) shows in result


showss :: [(String, (Int, Int), Int)]
showss = [("A", (11, 0), 120), ("B", (12, 0), 15), ("C", (10, 30), 90)]
-- >>> lastShow showss
-- "A"
-- нз защо е Б в отговорите според мен е А

-- другата подточка ще я мисля друг път


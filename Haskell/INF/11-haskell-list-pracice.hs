
-- другите задачи вече съм ги правил

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort left ++ [pivot] ++ quickSort right
        where 
            pivot = x
            left = filter (< pivot) xs
            right= filter (>= pivot) xs

-- >>> quickSort [0,344124,333,-5,2,0,7,-5555]
-- [-5555,-5,0,0,2,7,333,344124]


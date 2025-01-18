import Control.Arrow (ArrowChoice(left))

data Tree a = Empty | Node a (Tree a) (Tree a)
    deriving (Eq, Show, Read)

makeLeaf :: a -> Tree a
makeLeaf n = Node n Empty Empty

mapTree :: (a -> b) -> Tree a -> Tree b

mapTree f (Node x l r)= Node (f x) (mapTree f l) (mapTree f r)

depth:: Tree a -> Integer

depth (Node x Empty Empty) = 1
depth Empty = 0
depth (Node x l r) = 1 + max (depth l) (depth r)


countLeaves :: Tree a -> Integer
countLeaves Empty = 0
countLeaves (Node x Empty Empty) = 1
countLeaves (Node x l r) = countLeaves l + countLeaves r

collectPreOrder :: Tree a -> [a]
collectPreOrder (Node x l r) = [x] ++ collectPreOrder l ++ collectPreOrder r
collectPreOrder Empty = []


collectInOrder  :: Tree a -> [a]
collectInOrder  (Node x l r) = collectInOrder l ++ [x] ++ collectInOrder  r
collectInOrder Empty = []

level :: Tree a -> Integer -> [a]

level (Node x l r) 0 = [x]
level Empty n = []
level (Node x l r) n = level l (n - 1) ++ level r (n - 1)

exampleTree :: Tree Int
exampleTree = Node 1
               (Node 2 Empty Empty)
               (Node 3 (Node 4 Empty Empty) Empty)

prune :: Tree a -> Tree a
prune (Node x Empty Empty) = Empty
prune Empty = Empty
prune (Node x l r) = Node x (prune l) (prune r)

invert :: Tree a -> Tree a
invert (Node x l r) = Node x (invert r) (invert l)

path :: Eq a => Tree a -> a -> [a]
path Empty _ = []
path (Node x l r) k
    | x == k    = [x]
    | not (null leftPath)  = x : leftPath
    | not (null rightPath) = x : rightPath
    | otherwise = []
    where
        leftPath = path l k
        rightPath = path r k

contains :: Eq a => Tree a -> [a] -> Bool

contains _ [] = True
contains Empty _ = False
contains (Node x l r) (p:rest)
    | x == p = contains l rest || contains r rest
    | otherwise = contains l (p:rest) || contains r (p:rest)


isBst :: Ord a => Tree a -> Bool
isBst (Node x l r) = isBstHelper l Nothing (Just x) && isBstHelper r (Just x) Nothing

isBstHelper :: Ord a => Tree a -> Maybe a -> Maybe a -> Bool
isBstHelper (Node x l r) minValue maxValue =
    (case minValue of
        Just m -> x > m
        Nothing -> True) &&
    (case maxValue of
        Just m -> x < m
        Nothing -> False) &&
        isBstHelper l minValue (Just x) &&
        isBstHelper r (Just x) maxValue


bstInsert :: Ord a => Tree a -> a -> Tree a
bstInsert Empty el = Node el Empty Empty
bstInsert (Node x l r) el
    | el < x = Node x (bstInsert l el) r
    | el > x = Node x l (bstInsert r el)
    | otherwise = Node x l r


treeSort :: Ord a => [a] -> Tree a
treeSort xs = treeSortRecurese Empty xs
    where
        treeSortRecurese :: Ord a => Tree a -> [a] -> Tree a
        treeSortRecurese tree (x:xs) = treeSortRecurese (bstInsert tree x) xs
        treeSortRecurese tree [] = tree

getLeaves :: Tree a -> [a]
getLeaves Empty = []
getLeaves (Node x Empty Empty) = [x]
getLeaves (Node x l r) = getLeaves l ++ getLeaves r

treePaths :: Eq a => Tree a -> [[a]]
treePaths tree = map (path tree)  (getLeaves tree)



getChildren :: Tree a -> Tree a ->  [a]
getChildren Empty Empty = []
getChildren Empty (Node x l r) = [x]
getChildren (Node x l r) Empty = [x]
getChildren (Node x l r) (Node y ll rr) = [x, y]

findMeanNodesRecurse :: (Fractional a, Eq a) => Maybe a -> Tree a -> [a]
findMeanNodesRecurse _ Empty = []
findMeanNodesRecurse root (Node x l r) = 
    ([x | toAppend]) ++ findMeanNodesRecurse (Just x) l ++ findMeanNodesRecurse (Just x) r
    where
        toAppend :: Bool
        toAppend = case calculateMean root (getChildren l r) of
            Just t -> t == x
            Nothing -> False
        
        
        calculateMean :: (Fractional a, Eq a) => Maybe a -> [a] -> Maybe a
        calculateMean Nothing [] = Nothing
        calculateMean Nothing xs = Just (sum xs / fromIntegral (length xs))
        calculateMean (Just parent) xs = Just ((sum xs + parent) / fromIntegral (length xs + 1))



findMeanNodes :: (Fractional a, Eq a) => Tree a -> [a]
findMeanNodes = findMeanNodesRecurse Nothing

findGrandpas :: Tree Int -> [Int]
findGrandpas (Node x (Node y ll lr) (Node z rl rr)) = [x | toAdd] ++ findGrandpas (Node y ll lr) ++ findGrandpas (Node z rl rr)
    where
        toAdd :: Bool
        leftChildren = getChildren ll lr
        rightChildren = getChildren rl rr
        allChildren = rightChildren ++ leftChildren
        toAdd = sum allChildren == x


heavyNodesRecurse :: (Ord a, Num a ) => Tree a -> a -> [a]
heavyNodesRecurse Empty _ = []
heavyNodesRecurse (Node x l r) current = 
    [x | x > current] ++ heavyNodesRecurse l (current + x) ++ heavyNodesRecurse r (current + x)

heavyNodes :: (Ord a, Num a) => Tree a -> [a]
heavyNodes Empty = []
heavyNodes (Node x l r) = [x] ++ heavyNodesRecurse l x  ++ heavyNodesRecurse r x


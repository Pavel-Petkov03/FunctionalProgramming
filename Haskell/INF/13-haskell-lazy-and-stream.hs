nats :: [Int]
nats = natsRecurse 1
    where
        natsRecurse :: Int -> [Int]
        natsRecurse n = n : natsRecurse (n + 1)

-- >>> take 10 nats
-- [1,2,3,4,5,6,7,8,9,10]

isPrime :: Int -> Bool
isPrime n
  | n <= 1    = False
  | otherwise = isPrimeRecurse 2
  where
    isPrimeRecurse :: Int -> Bool
    isPrimeRecurse k
      | k * k > n     = True
      | n `mod` k == 0 = False
      | otherwise      = isPrimeRecurse (k + 1)


primes :: [Int]
primes = filter isPrime nats
-- >>> take 10 primes
-- [2,3,5,7,11,13,17,19,23,29]


primes2 :: [Int]
primes2 = sieve [2..]
    where
        sieve :: [Int] -> [Int]
        sieve (x:xs) = x : sieve (filter (\y -> y `mod` x /= 0) xs)

-- >>> take 10 primes2
-- [2,3,5,7,11,13,17,19,23,29]

myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : iterateRecurse (f x)
  where
    iterateRecurse currentX = currentX : iterateRecurse (f currentX)

-- >>> take 10 (myIterate (*2) 1)
-- [1,2,4,8,16,32,64,128,256,512]



rats :: [(Integer, Integer)]
rats = [(x, y) | x <- [1..] ,y <- [1..x]]

-- >>> take 10 rats
-- [(1,1),(2,1),(2,2),(3,1),(3,2),(3,3),(4,1),(4,2),(4,3),(4,4)]

fibs :: [Integer]
fibs = fibsIter 0 1
    where
        fibsIter :: Integer -> Integer -> [Integer]
        fibsIter a b = a : fibsIter b (a + b)
-- >>> take 10 fibs
-- [0,1,1,2,3,5,8,13,21,34]



data BinaryTree a
    = EmptyTree
    | Node a (BinaryTree a) (BinaryTree a)  


babaTree :: BinaryTree String 
babaTree = generateBabaTree ""
    where 
        generateBabaTree  :: String -> BinaryTree String
        generateBabaTree str = Node str (generateBabaTree (str ++ "a")) (generateBabaTree (str ++ "b"))

trimBinTree :: Integer -> BinaryTree a -> BinaryTree a
trimBinTree 0 _ = EmptyTree
trimBinTree n EmptyTree = EmptyTree
trimBinTree n (Node val left right) = Node val (trimBinTree (n-1) left) (trimBinTree (n-1) right)


-- >>> trimBinTree 6 babaTree
-- ""
--     "a"
--         "aa"
--             "aaa"
--                 "aaaa"
--                     "aaaaa"
--                     "aaaab"
--                 "aaab"
--                     "aaaba"
--                     "aaabb"
--             "aab"
--                 "aaba"
--                     "aabaa"
--                     "aabab"
--                 "aabb"
--                     "aabba"
--                     "aabbb"
--         "ab"
--             "aba"
--                 "abaa"
--                     "abaaa"
--                     "abaab"
--                 "abab"
--                     "ababa"
--                     "ababb"
--             "abb"
--                 "abba"
--                     "abbaa"
--                     "abbab"
--                 "abbb"
--                     "abbba"
--                     "abbbb"
--     "b"
--         "ba"
--             "baa"
--                 "baaa"
--                     "baaaa"
--                     "baaab"
--                 "baab"
--                     "baaba"
--                     "baabb"
--             "bab"
--                 "baba"
--                     "babaa"
--                     "babab"
--                 "babb"
--                     "babba"
--                     "babbb"
--         "bb"
--             "bba"
--                 "bbaa"
--                     "bbaaa"
--                     "bbaab"
--                 "bbab"
--                     "bbaba"
--                     "bbabb"
--             "bbb"
--                 "bbba"
--                     "bbbaa"
--                     "bbbab"
--                 "bbbb"
--                     "bbbba"
--                     "bbbbb"

instance Show a => Show (BinaryTree a) where
    show tree = showTree tree 0
      where
        showTree :: Show a => BinaryTree a -> Int -> String
        showTree EmptyTree _ = ""
        showTree (Node val left right) level =
            replicate (level * indent) ' ' ++ show val ++ "\n" ++
            showTree left (level + 1) ++
            showTree right (level + 1)
            where
                indent = 4

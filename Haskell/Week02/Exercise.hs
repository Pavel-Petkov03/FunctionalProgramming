import Prelude hiding (length, foldr, foldl, reverse, init, product, zip, zipWith, any, all, push)



length :: [a] -> Int
length (x:xs) = 1 + length xs
length [] = 0

--- >>> length [1,2,3,4]
-- 4


any :: (a -> Bool) -> [a] -> Bool
any  _ [] = False
any pred (x:xs) = if pred x then True else any pred xs

--- >>> any even [1,3,5]
-- False

all :: (a -> Bool) -> [a] -> Bool
all  _ [] = True
all pred (x:xs) = if pred x then any pred xs else False

--- >>> all odd [1,3,5]
-- True

member :: Eq a => a -> [a] -> Bool
member _ [] = False
member x (y:ys) = (x == y) || member x ys
-- Това работи за всякакви типове стига да са сравними (overload за оператор =)

push :: a -> [a] -> [a]
push x [] = [x]
push x (y:ys) = y : push x ys

--- >>> push 5 [1,3,5]
-- [1,3,5,5]

reverse :: [a] -> [a]
reverse (x:xs) = reverse xs ++ [x]
reverse [] = []

-- това работи но е 0(n^2)

--- >>> reverse [1, 2 , 3]
-- [3,2,1]

umenReverse :: [a] -> [a]
umenReverse xs = reverseRecurse xs []
    where
        reverseRecurse :: [a] -> [a] -> [a]
        reverseRecurse [] acc = acc
        reverseRecurse (x:xs) acc = reverseRecurse xs (x:acc)

-- това вече е 0(n)
--- >>> umenReverse [1, 2 , 3, 4]
-- [4,3,2,1]


init :: [a] -> [a]
init [] = []
init [x] = []
init (x:xs) = x: init xs

--- >>> init [1, 2, 3, 4]
-- [1,2,3]

insert :: [a] -> Int -> a -> [a]
insert xs 0 elem = elem : xs
insert [] _ elem = [elem]
insert (x : xs) n elem = x : insert xs (n -1) elem

--- >>> insert [1,2,3] 2 5
-- [1,2,5,3]

foldl :: (a -> b -> b) -> [a] -> b -> b
foldl _ [] acc = acc
foldl op (x:xs) acc = foldl op xs (op x acc)


--- >>> foldl (+) [1,2,3,4]  0
-- 10


foldr :: (a -> b -> b) -> [a] -> b -> b
foldr _ []  acc = acc
foldr op (x:xs)acc = op x (foldr op xs acc)

--- >>> foldr (+)  [1,2,3,4, 33]  0
-- 43


product :: [Int] -> Int
product xs = foldr (*) xs 1

--- >>> product [1,2,3]
-- 6

zip :: [a] -> [b] -> [(a, b)]
zip [] _ = []
zip _ [] = []
zip (x:xs) (y:ys) = (x, y) : zip xs ys


--- >>> zip [1,2,3] [4,5,6]
-- [(1,4),(2,5),(3,6)]

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ [] _ = []
zipWith _ _ [] = []
zipWith f (x : xs) (y : ys) = f x y : zipWith f xs ys 

-- >>> zipWith (+) [1, 2, 3] [2, 4, 6]
-- [3,6,9]


interleave:: [a] -> [a] -> [a]
interleave [] ys = ys
interleave xs [] = xs
interleave (x:xs) (y:ys) = x : y : interleave xs ys

-- >>> interleave [1,2,1] [4,5,6,3,4]
-- [1,4,2,5,1,6,3,4]

nats :: [Int]
nats = generate 1
  where
    generate n = n : generate (n + 1)

nats2 :: [Int]
nats2 =  [1..]
-- >>> take 15 nats2
-- [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]

pythagoreanTriples :: Int -> Int ->[(Int,Int,Int)]
pythagoreanTriples a b = [ (x, y, z) | x <- [a..b], y <- [x..b], z <- [y..b],
                            x^2 + y^2 == z^2]

-- >>> (pythagoreanTriples 1 100)
-- [(3,4,5),(5,12,13),(6,8,10),(7,24,25),(8,15,17),(9,12,15),(9,40,41),(10,24,26),(11,60,61),(12,16,20),(12,35,37),(13,84,85),(14,48,50),(15,20,25),(15,36,39),(16,30,34),(16,63,65),(18,24,30),(18,80,82),(20,21,29),(20,48,52),(21,28,35),(21,72,75),(24,32,40),(24,45,51),(24,70,74),(25,60,65),(27,36,45),(28,45,53),(28,96,100),(30,40,50),(30,72,78),(32,60,68),(33,44,55),(33,56,65),(35,84,91),(36,48,60),(36,77,85),(39,52,65),(39,80,89),(40,42,58),(40,75,85),(42,56,70),(45,60,75),(48,55,73),(48,64,80),(51,68,85),(54,72,90),(57,76,95),(60,63,87),(60,80,100),(65,72,97)]
--Това е краен поток


pythagoreanTriplesInf :: [(Int,Int,Int)]
pythagoreanTriplesInf =
    [ (x, y, z)
    | z <- [1..]
    , x <- [1..z]
    , y <- [x..z]
    , x^2 + y^2 == z^2
    ]

-- >>> take 30 pythagoreanTriplesInf
-- [(3,4,5),(6,8,10),(5,12,13),(9,12,15),(8,15,17),(12,16,20),(7,24,25),(15,20,25),(10,24,26),(20,21,29)]
-- при безкрайно потоци обръщаме seeker-а за да не изпаднем в безкраен цикъл

-- и за двете задачи не правя gcd защото хващам и непримитивните питагорови тройки


fibs :: [Int]
fibs = generateFibs 0 1
    where 
        generateFibs :: Int -> Int -> [Int]
        generateFibs a b = 
            a : generateFibs b (a + b)

-- >>> take 10 fibs
-- [0,1,1,2,3,5,8,13,21,34]



primes :: [Int]
primes = sieve [2..]
    where
        sieve :: [Int] -> [Int]
        sieve (x:xs) = x : sieve (filter (\y -> y `mod` x /= 0) xs)

-- >>> take 10 primes
-- [2,3,5,7,11,13,17,19,23,29]
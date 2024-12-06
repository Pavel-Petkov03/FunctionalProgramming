

fact :: Int -> Int
fact 0 = 1 
fact n = n * fact n - 1


-- >>> fact 5


fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fibIter 1 1 n
    where
        fibIter :: Int -> Int -> Int -> Int
        fibIter a b 0 = b
        fibIter a b n = fibIter b (a + b) (n - 1)

-- >>> fib 5
-- 13

myAbs :: Int -> Int
myAbs x = if x >= 0 then x else (-x)

-- >>> myAbs (-5)
-- 5

composeInt :: (Int -> Int) -> (Int -> Int) -> (Int-> Int)
composeInt f g = \x -> f (g x)




addOne :: Int -> Int
addOne x = x + 1

multiplyByTwo :: Int -> Int
multiplyByTwo x = x * 2

-- >>> composeInt addOne multiplyByTwo 2
-- 5

compose :: (a -> b) -> (b -> c) -> (a -> c)
compose f g = \x -> f (g x)

myConcant :: [a] -> [a] -> [a]
myConcant [] ys = ys
myConcant (x:xs) ys = x : myConcant xs ys


-- >>> myConcant [1,2,3] [4,5,6]
-- [1,2,3,4,5,6]

isIntPrefix :: [Int] -> [Int] -> Bool
isIntPrefix [] ys = True
isIntPrefix (x:xs) (y:ys) = x == y && isIntPrefix xs ys

-- >>> isIntPrefix [1,3,3] [1,3,3,5,5,5,5]
-- True

isPrefix :: (Eq a) => [a] -> [a] -> Bool
isPrefix [] ys = True
isPrefix (x:xs) (y:ys) = x == y && isPrefix xs ys

frepeat :: Int -> (a -> a) -> a -> a
frepeat 1 f x = f x
frepeat n f x = f (frepeat (n-1) f x)

-- >>> frepeat 2 (**2) 2
-- 16.0


frepeated :: Int -> (a -> a) -> (a -> a)
frepeated n f = \x -> frepeat n f x


-- >>> frepeated 2 (**2) 3
-- 81.0

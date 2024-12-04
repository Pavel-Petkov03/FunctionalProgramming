hypothenuse :: Double -> Double -> Double
hypothenuse a b = sqrt (a**2 + b**2)
square :: Int -> Int
square x = x * x

-- >>> hypothenuse 4 4
-- 5.656854249492381

twice :: (t -> t) -> t -> t
twice f x = f (f x)
diag :: (t -> t -> t) -> t -> t
diag f x = f x x
-- >>> twice square 5
-- 625

-- mod 13 5
-- >>> 13 `mod` 5
-- 3

fact :: Int -> Int
fact x
    | x >0 = x * fact (x - 1)
    | x == 0 = 1
    | otherwise = error "Error is generated"

-- >>>fact (5)
-- 120


(!=) :: Eq a => a -> a -> Bool
(!=) x y = x /= y

-- EQ e type constaint  for a to have == and /= 

-- >>> 6 != 6
-- False


pow2 :: Int -> Int
pow2 0 = 1
pow2 n  = 2 * pow2 (n - 1)

-- >>> pow2 7
-- 128

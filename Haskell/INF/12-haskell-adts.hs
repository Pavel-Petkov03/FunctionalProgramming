data MyBool 
    = MyTrue 
    | MyFalse
    deriving (Show)

data Colour 
    = Red 
    | Green 
    | Cyan 
    | Magenta 
    | Yellow 
    | Blue 
    deriving (Show)


data Shape
    = Circle Float
    | Rectangle Float Float
    deriving (Show)

-- >>> Circle 32
-- Circle 32.0
-- >>> Rectangle 43 22
-- Rectangle 43.0 22.0

area :: Shape -> Float
area (Rectangle a b) = a * b
area (Circle r) = r * r * pi


rect :: Shape
rect = Rectangle 3 4

circle:: Shape
circle = Circle 5
-- >>> area rect
-- 12.0
-- >>> area circle
-- 78.53982
perimeter :: Shape -> Float
perimeter (Rectangle a b) = 2 * a + 2 * b
perimeter (Circle r) = 2 * pi * r


-- >>> perimeter rect
-- 14.0
-- >>> perimeter circle
-- 31.415928


data RPS
    = Rock
    | Paper
    | Scissors
    deriving (Show)
beats :: RPS -> RPS -> Bool
beats Rock Paper = False
beats Rock Scissors = True
beats Scissors Paper = True
beats Scissors Rock = False
beats Paper Rock = True
beats Paper Scissors = False
beats Rock Rock = False
beats Paper Paper = False
beats Scissors Scissors = False

-- >>> beats Rock Paper
-- False


data List a
    = Cons a (List a)
    | Empty

lmap :: (a -> b) -> List a -> List b
lmap f Empty = Empty
lmap f (Cons a ls) = Cons (f a) (lmap f ls)

lfilter :: (a -> Bool) -> List a -> List a 
lfilter _ Empty = Empty
lfilter pred (Cons a ls) = 
    if pred a 
        then Cons a (lfilter pred ls) 
        else lfilter pred ls

ls :: List Int
ls = Cons 1 ( Cons 2 Empty)
-- >>> lmap (+1) ls
-- Cons 2 (Cons 3 Empty)
-- >>> lfilter (>1) ls
-- Cons 2 Empty


lfoldr :: (b -> a -> b) -> List a -> b -> b
lfoldr f (Cons x Empty) init = f init x
lfoldr f (Cons x ls) init = lfoldr f ls (f init x)

-- >>> lfoldr (\acc cur -> acc + cur) ls 0
-- 3

instance Show a => Show (List a) where
    show lst = "List" ++ showElements lst
      where
        showElements Empty = "()"
        showElements (Cons x xs) = "(" ++ showElements' (Cons x xs) ++ ")"
        
        showElements' (Cons x Empty) = show x
        showElements' (Cons x xs) = show x ++ "," ++ showElements' xs

-- >>> ls
-- List(1,2)


data BinaryTree a
    = EmptyTree
    | Node a (BinaryTree a) (BinaryTree a)  
    deriving (Show)

height :: BinaryTree a -> Int
height EmptyTree = 0
height (Node _ left right) = 1 + max (height left) (height right)

isBalanced :: BinaryTree a -> Bool
isBalanced EmptyTree = True
isBalanced (Node _ left right) =
    abs (height left - height right) <= 1 && isBalanced left && isBalanced right

exampleTree :: BinaryTree Int
exampleTree = Node 10
                (Node 5
                    (Node 3 EmptyTree EmptyTree)
                    (Node 7 EmptyTree EmptyTree))
                (Node 15
                    EmptyTree
                    (Node 18 EmptyTree EmptyTree))

exampleUnbalancedTree :: BinaryTree Int
exampleUnbalancedTree = Node 10
                            (Node 5
                                (Node 3 EmptyTree EmptyTree)
                                EmptyTree)
                            EmptyTree

-- >>> isBalanced exampleTree
-- True
-- >>> isBalanced exampleUnbalancedTree
-- False

import Data.List (nub, minimumBy, maximumBy)
import Data.Ord  (comparing)
find :: (Eq a) => [(a, b)] -> a -> b
find xs key = let filtered = filter (\(first, second) -> first == key) xs in
        if null filtered then error "Error: Nqma brat sori" else snd $ head filtered

ls :: [(Int, Int)]
ls = [(1,2), (3,4)]

-- >>> find ls 5
-- Error: Nqma brat sori

groupBy :: (Eq b) => (a -> b) -> [a] -> [(b, [a])]
groupBy f xs = map createGroupFunc uniqueKeys
  where
    uniqueKeys = nub (map f xs)
    createGroupFunc key = (key, filter (\t -> f t == key) xs)

-- >>> groupBy (`mod` 3) [1, 2, 3, 4, 5, 6]
-- [(1,[1,4]),(2,[2,5]),(0,[3,6])]
type Subject = String
type Student = String
type Exam = Int
type Note = Double 

type Record = (Subject, Student, Exam, Note)

uniqueStudents :: [Record] -> [Student]
uniqueStudents records = nub (map (\(_, student, _, _) -> student) records)

averageNote :: [Record] -> Student -> Double
averageNote records student = 
    let studentRecords = filter (\(_, s, _, _) -> s == student) records
        notes = map (\(_, _, _, note) -> note) studentRecords
    in sum notes / fromIntegral (length notes)

topOfClass :: [Record] -> Student
topOfClass records = 
    let students = uniqueStudents records
        averages = map (\student -> (student, averageNote records student)) students
    in fst $ maximumBy (comparing snd) averages


-- ще го довърша това и почвам scheme малко

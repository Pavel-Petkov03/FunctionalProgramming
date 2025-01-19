type Name = String
type Prediction = Int
type ActualResult = Int
type Record = (String, Prediction, ActualResult)
type ErrorRecord = (Name, Int, ActualResult)

getErrorRecord :: [Record] -> [ErrorRecord]
getErrorRecord = map (\(name, pred, actual) -> (name, abs (pred - actual), actual))

takeLeastRecord :: [ErrorRecord] -> ErrorRecord
takeLeastRecord = foldr1 (
    \(firstName, firstError, firstActual) (secondName, secondError, secondActual)
    -> if firstError < secondError then (firstName, firstError, firstActual) else (secondName, secondError, secondActual))


removeRecord :: [ErrorRecord] -> ErrorRecord -> [ErrorRecord]
removeRecord records removeRecord = filter (/= removeRecord) records

selectionSort :: [ErrorRecord] -> [ErrorRecord]
selectionSort [] = []
selectionSort xs = currentRecord : selectionSort (removeRecord xs currentRecord)
    where
        currentRecord = takeLeastRecord xs

records :: [ErrorRecord]
records = [("Deqn",10,14),
            ("Eslin",5,8),
            ("Mariq",11,10),
            ("Martin",1,6)
    ]
addBonuses :: [ErrorRecord] -> [(Name, Int)]
addBonuses xs = [addBonusesExtract xs index (n - 1- index) | index <- [0..n - 1]]
    where
        n = length xs

addBonusesExtract:: [ErrorRecord] -> Int -> Int -> (Name, Int)
addBonusesExtract xs currentIndex otherIndex = (name, actual + addError)
    where
        (name, error, actual) = xs !! currentIndex
        (_, addError, _) = xs !! otherIndex


finalScores :: [Record] -> [(Name, Int)]
finalScores records = addBonuses $ selectionSort $  getErrorRecord records


import Data.List (sortBy)
racaman :: [Int]

racaman = 0 : racamanRecurse 0 1 []
    where
        racamanRecurse :: Int -> Int -> [Int] -> [Int]
        racamanRecurse prev index cache=
            if prev > index && (prev - index) `notElem` cache
                then (prev - index) : racamanRecurse (prev - index) (index + 1) ((prev - index) : cache)
                else prev + index : racamanRecurse (prev + index) (index + 1) ((prev + index) : cache)


-- >>> take 10 racaman
-- [0,1,3,6,2,7,13,20,12,21]


checkId :: [Float -> Float] -> [Float] -> Bool
checkId fs array = any (\h -> all (\value -> value == h value) array) [f . g | f <- fs, g <- fs]


-- >>> checkId [(^2), sin, (+2), sqrt] [1..5]
-- True
-- >>> checkId [abs, max 2, min 4] [-1..5]
-- False


allTeams :: [(String, String, Int, Int)] -> [String]
allTeams (l:ls) =
    let (first, second, _ , _) = l in
    reverse $ foldl (\acc (a,b,_,_) -> addStr acc a b) [second, first] ls
    where
        addStr ls a b
            | a `notElem` ls && b `notElem` ls = a : b : ls
            | a `notElem` ls = a : ls
            | b `notElem` ls = b : ls
            | otherwise = ls


-- >>> allTeams [("a", "b", 1, 2), ("a", "c", 1, 2), ("a", "a" , 1 , 2), ("b", "d", 1 , 2)]
-- ["a","b","c","d"]


teamScore :: [(String, String, Int, Int)] -> String -> (Int, Int, Int)
teamScore games name =
    (getPoints games name, getMadeGoals games name, getTakenGoals games name)
    where
        getPoints games name =
            foldl (\acc currentGame->
                case currentGame of
                    (firstName, secondName, firstPoints, secondPoints)
                        | firstName == name && firstPoints > secondPoints -> acc + 3
                        | firstName == name && firstPoints == secondPoints -> acc + 1
                        | firstName == name && firstPoints < secondPoints ->  acc
                        | secondName == name && secondPoints > firstPoints ->  acc + 3
                        | secondName == name && secondPoints == firstPoints ->  acc + 1
                        | secondName == name && secondPoints < firstPoints ->  acc
                        | otherwise ->  acc
                ) 0 games

        getMadeGoals games name = foldl (\acc game ->
            case game of
                (firstName, secondName, firstPoints, secondPoints)
                    | firstName == name -> acc + firstPoints
                    | secondName == name -> acc + secondPoints
                    | otherwise  -> acc
            ) 0 games
        
        getTakenGoals games name = foldl (\acc game ->
            case game of
                (firstName, secondName, firstPoints, secondPoints)
                    | firstName == name -> acc + secondPoints
                    | secondName == name -> acc + firstPoints
                    | otherwise  -> acc
            ) 0 games
        

scoreBoard :: [(String, String, Int, Int)] -> [(String, Int, Int, Int)]
scoreBoard games = sortBy sortF $ map (\name -> 
    let (points, goalsMade, goalsTaken) = teamScore games name
        in (name, points, goalsMade, goalsTaken)
    ) (allTeams games)
    where
        sortF (name1, points1, goals1 ,taken1)  (name2, points2, goals2 ,taken2) 
            | points1 /= points2 = compare points2 points1
            | (goals1 - taken1) /= (goals2 - taken2) = compare (goals1 - taken1) (goals2 - taken2)
            | goals1 /= goals2 = compare goals1 goals2
            | otherwise = EQ

-- strah me e da go testvam(priemame che e vqrno)


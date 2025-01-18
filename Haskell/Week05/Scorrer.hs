type Student = String
type Subject = String
type Note = Double

data Record = Record {student :: Student, subject :: Subject, note :: Note}
  deriving (Read, Show)

customNub :: (Eq a) => [a] -> [a]
customNub ls = customNubRecurse ls []
    where
        customNubRecurse :: (Eq a) => [a] -> [a] -> [a]
        customNubRecurse [] memory = memory
        customNubRecurse (x:xs) memory
            | x `elem` memory = customNubRecurse xs memory
            | otherwise = customNubRecurse xs (x:memory)

getStudents :: [Record] -> [Student]
getStudents records = customNub (map student records)

getStudentsWithAtLeastOneExcellentScore :: [Record] -> [Student]
getStudentsWithAtLeastOneExcellentScore records =
    filter (\s -> any (\record -> s == student record && note record == 6)  records) (getStudents records)

goodStudentsAverage :: [Record] -> Note
goodStudentsAverage records = sum allAverage / fromIntegral (length allAverage)
    where
        averageForStudent s =
            let studentGrades = [note record | record <- records, student record == s]
            in if null studentGrades then 0 else sum studentGrades / fromIntegral (length studentGrades)
        allAverage =  map averageForStudent (getStudentsWithAtLeastOneExcellentScore records)




records :: [Record]
records = [
    Record "Alice" "Math" 6,
    Record "Alice" "Science" 4,
    Record "Bob" "Math" 5,
    Record "Bob" "Science" 6,
    Record "Charlie" "Math" 6,
    Record "Charlie" "History" 5,
    Record "David" "Math" 3,
    Record "David" "Science" 2
    ]


type Name = String
type Goals = Int
type Assists = Int
type Hometown = Name


-- приемаме , че няма играч, който да играе за два отбора (звучи ми нелогично)
data Player = Player {playerName :: Name, goals :: Goals, assists :: Assists}
  deriving (Read, Show)


data Team = Team {teamName :: Name, hometown :: Hometown, players :: [Player]}
  deriving (Read, Show)

getAllPlayers :: [Team] -> [Player]
getAllPlayers = concatMap players

topScorrer :: [Team] -> Name
topScorrer teams = playerName $ foldr1 (\player currentMaxPlayer -> if goals player > goals currentMaxPlayer then player else currentMaxPlayer) (getAllPlayers teams)


topAssists :: [Team] -> Name
topAssists teams = playerName $ foldr1 (\player currentMaxPlayer -> if assists player > assists currentMaxPlayer then player else currentMaxPlayer) (getAllPlayers teams)


goalsTeamPair :: Team -> (Goals, Team)
goalsTeamPair team = (sum (map goals (players team))  , team)

topTeam :: [Team] -> Name
topTeam teams =  teamName $ snd $ foldr1(\(goals1, team1) (goals2, team2) -> if goals1 > goals2 then (goals1, team1) else (goals2, team2))  (map goalsTeamPair teams)


cityTeamsCountPair :: Hometown -> [Team] -> (Hometown, Int)
cityTeamsCountPair cityName teams = (cityName, foldr (\cur acc -> if teamName cur == cityName then acc + 1 else acc) 0 teams)
topCity :: [Team] -> Hometown
topCity teams = fst $ foldr1 (\(team1, times1) (team2, times2) -> if times1 > times2 then (team1, times1) else (team2, times2)) homeTownTeamCountPairs
    where
        homeTownTeamCountPairs = map (\team -> cityTeamsCountPair (hometown team) teams) teams


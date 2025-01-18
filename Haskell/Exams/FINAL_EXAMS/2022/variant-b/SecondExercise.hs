
type State = Int
type Transition = (State, Char, State)

data Automat = Automat {states :: [State], startState :: State, transitions :: [Transition], finalStates :: [State]}
    deriving (Show)

findAllTransitions :: State -> Automat -> [Transition]
findAllTransitions currentState automat = filter (\(s,_,_) -> s == currentState) (transitions automat)

detectedWordsRecurse :: State -> Automat -> String ->  [State ]-> [String]
detectedWordsRecurse currentState automat generatedString passedStates
    | currentState `elem` finalStates automat = generatedString : concatMap (\(s,c,f) -> detectedWordsRecurse f automat (c : generatedString) [s]) (findAllTransitions currentState automat)
    | all (\(s,_,_) -> s `elem` passedStates)  possibleTransitions  = []
    | otherwise = concatMap (\(s,c,f) -> detectedWordsRecurse f automat (c : generatedString) (currentState : passedStates)) (findAllTransitions currentState automat)
    where
        possibleTransitions = findAllTransitions currentState automat


detectedWords :: Automat -> [String]
detectedWords automat = detectedWordsRecurse (startState automat) automat "" []


avtomat :: Automat
avtomat = Automat [0,1,2,3] 0 [(0,'a', 1), (1,'b' , 1), (0,'a', 2), (0,'b', 2), (2,'a',3), (1, 'a', 2)] [0,2,3]

-- РЕШЕНИЕТО НЕ Е НАПЪЛНО КОРЕКТНО ЗАЩОТО НЕ ИЗКАРВА ВСИЧКИ ДУМИ ПРИ БЕЗКРАЕН ЦИКЪЛ А САМО ТЕЗИ ПРИ ПЪРВИЯТ СРЕЩНАТ БРЕЗКРАН ЦИКЪЛ
-- ще го дооправя после

newtype Task = Task String
    deriving (Show, Eq)

newtype TaskGraph = TaskGraph [(Task, [Task])]
    deriving (Show, Eq)


extractReady :: [(Task, [Task])] -> Maybe Task
extractReady = foldr (\(task, deps) acc -> if null deps then Just task else acc) Nothing


removeReadyTasks :: [(Task, [Task])] -> Task -> [(Task, [Task])]
removeReadyTasks g toRemove = foldl removeFunc [] g
  where
    removeFunc :: [(Task, [Task])] -> (Task, [Task]) -> [(Task, [Task])]
    removeFunc acc (task, deps) =
        if task == toRemove then acc else (task, filter (/= toRemove) deps) : acc


taskOrder :: TaskGraph -> Maybe [Task]
taskOrder (TaskGraph graph) = taskOrderRecurse graph []
    where
        taskOrderRecurse :: [(Task, [Task])]  -> [Task] -> Maybe [Task]
        taskOrderRecurse graph result =
            let ready = extractReady graph
            in case ready of
                Nothing -> if null graph
                        then Just (reverse result)
                        else Nothing
                Just task -> taskOrderRecurse (removeReadyTasks graph task) (task : result)


graph :: TaskGraph
task1 :: Task
task1 = Task "Task1"
task2 :: Task
task2 = Task "Task2"
task3 :: Task
task3 = Task "Task3"
task4 :: Task
task4 = Task "Task4"
graph = TaskGraph [(task1, [task2]), (task2, [task3]), (task3, [task4]), (task4, [])]
-- >>> taskOrder graph
-- Just [Task "Task4",Task "Task3",Task "Task2",Task "Task1"]
graph2 :: TaskGraph
graph2 = TaskGraph [(task1, [task2]), (task2, [task1])]


-- >>> taskOrder graph2
-- Nothing

-- tva mi razkaza igrata
-- чак сега разбрах защо in case e необходимо в моменти, ако искам да правя проверки по тип




data ArithExpr
    = Const Float
    | Var String    
    | Add ArithExpr ArithExpr
    | Sub ArithExpr ArithExpr 
    | Mul ArithExpr ArithExpr 
    | Div ArithExpr ArithExpr 
    | Pow ArithExpr ArithExpr
    deriving (Show)

calculate :: [(String, Float)] -> ArithExpr -> Maybe Float
calculate _ (Const x) = Just x
calculate env (Var x) = lookup x env 


calculate env (Add e1 e2) = do
    v1 <- calculate env e1
    v2 <- calculate env e2
    return (v1 + v2)
calculate env (Sub e1 e2) = do
    v1 <- calculate env e1
    v2 <- calculate env e2
    return (v1 - v2)
calculate env (Mul e1 e2) = do
    v1 <- calculate env e1
    v2 <- calculate env e2
    return (v1 * v2)
calculate env (Div e1 e2) = do
    v1 <- calculate env e1
    v2 <- calculate env e2
    if v2 == 0
        then Nothing
        else return (v1 / v2)
calculate env (Pow e1 e2) = do
    v1 <- calculate env e1
    v2 <- calculate env e2
    return (v1 ** v2)


env :: [(String, Float)]
env = [("x", 5), ("y", 2), ("z", 3)]

example1 :: ArithExpr
example1 = Add (Const 3) (Var "x")

example2 :: ArithExpr
example2 = Mul (Var "y") (Sub (Var "x") (Const 4))

example3 :: ArithExpr
example3 = Pow (Var "x") (Const 2)

-- >>> calculate env example1
-- Just 8.0
-- >>> calculate env example2
-- >>> calculate env example3
-- Just 2.0
-- Just 25.0


-- причината да позвам do, e че ползвам монадата Maybe и тя има вграден fail и ако имам нестване на операции няма нужда
-- да се притеснявам ако някоя вътрешна операция върне Nothing


-- calculate env (Add e1 e2) = 
--     calculate env e1 >>= \v1 ->  -- Извличаме стойността на първия израз
--     calculate env e2 >>= \v2 ->  -- Извличаме стойността на втория израз
--     return (v1 + v2)    -- това е без do

differentiate :: String -> ArithExpr -> ArithExpr
differentiate var (Const _) = Const 0 
differentiate var (Var v) 
    | var == v  = Const 1           
    | otherwise = Const 0            
differentiate var (Add e1 e2) = 
    Add (differentiate var e1) (differentiate var e2)
differentiate var (Sub e1 e2) = 
    Sub (differentiate var e1) (differentiate var e2) 
differentiate var (Mul e1 e2) = 
    Add (Mul (differentiate var e1) e2) (Mul e1 (differentiate var e2))
differentiate var (Div e1 e2) = 
    Div (Sub (Mul (differentiate var e1) e2) (Mul e1 (differentiate var e2))) (Mul e2 e2) 

-- >>> differentiate "x" example1
-- Add (Const 0.0) (Const 1.0)
-- не ми се пише pow, защото трябва да пиша log (d/dx a^y = a^y * lna *d/dx y)
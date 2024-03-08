module TopDownEval where
import PrologParser
import PatternMatch

-------------------- topDownEvalaute --------------------
topDownEvaluate :: ASTNode -> [ASTNode] -> [(ASTNode, ASTNode)]
topDownEvaluate statement1 parsedFile
    | member (PredVariable "FALSE", PredVariable "FALSE") result = [(PredVariable "FALSE", PredVariable "FALSE")]
    | otherwise = result
    where result = topDownEvaluate2 statement1 parsedFile parsedFile

-- Evaluate a query using top down approach
topDownEvaluate2 :: ASTNode -> [ASTNode] -> [ASTNode] -> [(ASTNode, ASTNode)]
-- If no matching rule/fact was found, query is insatisfiable
topDownEvaluate2 _ [] _ = [(PredVariable "FALSE", PredVariable "FALSE")]

-- If a fact was found, try to match it
topDownEvaluate2 (Fact (Predicate x xs)) (Fact (Predicate y ys) : rest) parsedFile
    -- If the matching was successful, return the MGU
    | x == y && result /= [(PredVariable "FALSE", PredVariable "FALSE")] = result
    -- Else if the matching failed, continue searching
    | otherwise = topDownEvaluate2 (Fact (Predicate x xs)) rest parsedFile
    where result = unify (Predicate x xs) (Predicate y ys)

-- If a rule was found, try to match it
topDownEvaluate2 (Fact (Predicate x xs)) ((Rule head body) : rest) parsedFile
    -- If the matching failed, continue searching
    | mgu == [(PredVariable "FALSE", PredVariable "FALSE")] = topDownEvaluate2 (Fact (Predicate x xs)) rest parsedFile
    -- Else if the matching was successful, apply MGU to the body and evaluate the body
    | otherwise = mgu ++ evaluateBody (applyMgu mgu (renameBody body (1+maxQuotes xs 0))) parsedFile
    where mgu = unify (Predicate x xs) (renameVars head (1+maxQuotes xs 0))
----------------------------------------

-------------------- Quote calculation --------------------
-- Calculate the max quotes of a body
maxQuotes :: [ASTNode] -> Int -> Int
maxQuotes [] n = n
maxQuotes [PredVariable x] n
    | currentQuotes > n = currentQuotes
    | otherwise = n
    where currentQuotes = countQuotes x
maxQuotes [Predicate x args] n
    | currentQuotes > n = currentQuotes
    | otherwise = n
    where currentQuotes = maxQuotes args n
maxQuotes (x:xs) n
    | currentQuotes > n = maxQuotes xs currentQuotes
    | otherwise = maxQuotes xs currentQuotes
    where currentQuotes = maxQuotes [x] n

-- Count the quotes found in a string
countQuotes :: String -> Int
countQuotes "" = 0
countQuotes (x:xs)
    | x == '\'' = 1 + countQuotes xs
    | otherwise = countQuotes xs

-- Print n quotes
printQuotes :: Int -> String
printQuotes 1 = "\'"
printQuotes n = "\'" ++ printQuotes (n-1)
----------------------------------------

-------------------- Body management --------------------
-- Evaluate the body of a rule
evaluateBody :: [ASTNode] -> [ASTNode] -> [(ASTNode, ASTNode)]
evaluateBody [] _ = []
evaluateBody (body : bodyRest) parsedFile
    -- If the top down evaluation for the first statement of the body failed, then evaluation fails
    | mgu == [(PredVariable "FALSE", PredVariable "FALSE")] = [(PredVariable "FALSE", PredVariable "FALSE")]
    -- Else, save the MGU that occurs and move onto the next statement of the body
    | otherwise = mgu ++ evaluateBody (applyMgu mgu bodyRest) parsedFile
    where mgu = topDownEvaluate (Fact body) parsedFile

-- Rename the body of a rule by applying quotes to each variable
renameBody :: [ASTNode] -> Int -> [ASTNode]
renameBody [] _ = []
renameBody (body:bodyRest) n = renameVars body n : renameBody bodyRest n

-- Rename a predicate applying quotes to each variable
renameVars :: ASTNode -> Int -> ASTNode
renameVars (Predicate x []) _ = Predicate x []
renameVars (Predicate x (y:ys)) n = Predicate x (renameVars2 (y:ys) n)

-- Rename a list of predicates applying quotes to each variable
renameVars2 :: [ASTNode] -> Int -> [ASTNode]
renameVars2 [] _ = []
renameVars2 ((PredVariable x):xs) n = PredVariable (x ++ printQuotes n) : renameVars2 xs n
renameVars2 ((Predicate x y):xs) n = Predicate x (renameVars2 y n) : renameVars2 xs n
----------------------------------------

-------------------- formatAnswer --------------------
-- Takes the MGU and the list of the query's variables
-- Outputs a formatted answer which can easily be read by the user
formatAnswer :: [(ASTNode, ASTNode)] -> [ASTNode] -> String
formatAnswer answer vars
    | answer == [(PredVariable "FALSE", PredVariable "FALSE")] = "false."
    | null answer = "true."
    | otherwise = formatAnswer2 trimmed
    where trimmed = trimAnswer (composeMgu answer) vars

-- Format "(Variable X, someAssignment)" to "X = someAssignment"
formatAnswer2 :: [(ASTNode, ASTNode)] -> String
formatAnswer2 [] = "true."
formatAnswer2 [(PredVariable x, PredVariable y)] = x ++ " = " ++ y
formatAnswer2 [(PredVariable x, Predicate y [])] = x ++ " = " ++ y
formatAnswer2 [(PredVariable x, Predicate y args)] = x ++ " = " ++ formatAnswer3 [Predicate y args]
formatAnswer2 (x:xs) = formatAnswer2 [x] ++ ", " ++ formatAnswer2 xs

-- Format "Predicate s [arg1, arg2, arg3]" to s(arg1,arg2,arg3,)
-- NOTE: I have no idea how to get rid of the final comma :( 
formatAnswer3 :: [ASTNode] -> String
formatAnswer3 [PredVariable x] = x ++ ","
formatAnswer3 [Predicate x []] = x ++ ","
formatAnswer3 [Predicate x args] = x ++ "(" ++ formatAnswer3 args ++ ")"
formatAnswer3 (x:xs) = formatAnswer3 [x] ++ " " ++ formatAnswer3 xs

-- Get all the variables of a given query
getVariables :: [ASTNode] -> [ASTNode]
getVariables [] = []
getVariables [PredVariable x] = [PredVariable x]
getVariables [Fact x] = getVariables [x]
getVariables [Predicate _ args] = getVariables args
getVariables (x:xs) = getVariables [x] ++ getVariables xs

-- Some of the replacements may not be needed in the final answer
-- Only keep important replacements by trimming replacements that aren't related to the query's variables
trimAnswer :: [(ASTNode, ASTNode)] -> [ASTNode] -> [(ASTNode, ASTNode)]
trimAnswer [] _ = []
trimAnswer ((PredVariable x, PredVariable y):xs) vars = trimAnswer xs vars
trimAnswer ((x, y):xs) vars
    | member x vars = (x,y) : trimAnswer xs vars
    | otherwise = trimAnswer xs vars
----------------------------------------
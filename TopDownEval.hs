module TopDownEval where
import PrologParser
import PatternMatch
import Debug.Trace

-- Some of the replacements may not be needed in the final answer
-- Only keep important replacements by trimming replacements that aren't related to the query's variables
trimAnswer :: [(ASTNode, ASTNode)] -> [ASTNode] -> [(ASTNode, ASTNode)]
trimAnswer [] _ = []
trimAnswer ((x, y):xs) vars
    | member x vars = (x,y) : trimAnswer xs vars
    | otherwise = trimAnswer xs vars

-- Takes the MGU and the list of the query's variables
-- Outputs a formatted answer which can easily be read by the user
formatAnswer :: [(ASTNode, ASTNode)] -> [ASTNode] -> String
formatAnswer answer vars
    | answer == [(PredVariable "FALSE", PredVariable "FALSE")] = "false."
    | null answer = "true."
    | otherwise = formatAnswer2 trimmed
    where trimmed = trimAnswer answer vars

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

-- -- getAnswer :: [ASTNode] -> [(ASTNode, ASTNode)] -> [(ASTNode, ASTNode)]
-- getAnswer [] _ = []

-- simplifyMgu :: [(ASTNode, ASTNode)] -> [(ASTNode, ASTNode)] -> [(ASTNode, ASTNode)]
-- simplifyMgu [] mgu = mgu
-- simplifyMgu (x:xs) mgu
--     | simplified == (x:xs) = simplified
--     | otherwise = simplifyMgu simplified simplified
--     where simplified = simplify x mgu

-- simplify :: (ASTNode, ASTNode) -> [(ASTNode, ASTNode)] -> [(ASTNode, ASTNode)]
-- simplify (x,y) [] = [(x,y)]
-- simplify (PredVariable x1, PredVariable y1) ((PredVariable x2, PredVariable y2) : xs)
--     | x1 == x2 && y1 == y2 = simplify (PredVariable x1, PredVariable y1) xs
--     | otherwise = (PredVariable x2, PredVariable y2) : simplify (PredVariable x1, PredVariable y1) xs
-- simplify (PredVariable x1, y1) ((x2, y2) : xs)
--     | y1 == x2 = simplify (PredVariable x1, y2) xs
--     | otherwise = (x2, y2) : simplify (PredVariable x1, y1) xs
-- simplify x (y:ys) = simplify x ys
-- simplify (x,y) ((z,w) : xs) = (z,w) : simplify (x,y) xs

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

-- APO EDW KAI KATW KSEKINANE TA MAGIKA ME TA RULES. --

-- FOR NOW IGNORE RULES
topDownEvaluate2 (Fact (Predicate x xs)) ((Rule head body) : rest) parsedFile = topDownEvaluate2 (Fact (Predicate x xs)) rest parsedFile

-- In order to evaluate a rule, first we need to match the query with a head
-- topDownEvaluate2 (Fact (Predicate x xs)) ((Rule head body) : rest) parsedFile
--     -- If a head was matched, evaluate query using body
--     | mgu /= [(PredVariable "FALSE", PredVariable "FALSE")] = evaluateBody (applyMgu mgu (renameBody body)) parsedFile
--     -- Else if the matching failed, continue searching
--     | otherwise = topDownEvaluate2 (Fact (Predicate x xs)) rest parsedFile
--     where mgu = unify (Predicate x xs) (renameVars head)

-- APO EDW KAI KATW KSEKINANE TA MAGIKA ME TO EVALUATION TOU BODY ENOS RULE --

-- Evaluate the body of a rule
evaluateBody :: [ASTNode] -> [ASTNode] -> [(ASTNode, ASTNode)]
evaluateBody [] _ = []
evaluateBody (body : bodyRest) parsedFile
    -- If the top down evaluation for the first statement of the body failed, then evaluation fails
    | mgu == [(PredVariable "FALSE", PredVariable "FALSE")] = [(PredVariable "FALSE", PredVariable "FALSE")]
    -- Else, save the MGU that occurs and move onto the next statement of the body
    | otherwise = mgu ++ evaluateBody (applyMgu mgu bodyRest) parsedFile
    where mgu = topDownEvaluate (Fact body) parsedFile

-- BOH8HTIKES SYNARTHSEIS APPLYMGU KAI APPLYREPLACEMENT--
-- H APPLY MGU PAIRNEI 1O ORISMA ENA MGU, 2O ORISMA ENA BODY KAI EFARMOZEI OLES TIS ANTIKATASTASEIS TOU MGU STO BODY ENOS RULE --
-- H APPLYREPLACEMENT PAIRNEI 1O ORISMA 1 REPLACEMENT, 2O ORISMA ENA BODY KAI EFARMOZEI OLES TIS ANTIKATASTASEIS TOU MGU STO BODY ENOS RULE --
-- H APPLYMGU KALEI THN APPLYREPLACEMENT GIA KA8E REPLACEMENT POU UPARXEI STO MGU --
-- NOMIZW OTI AYTES DOULEUOUN TELEIA, DEN XREIAZETAI NA TIS PEIRAKSOUME AKOMA KAI AN ALLAKSOUME TON TROPO SKEPSHS THS EVALUATEBODY--

renameBody [] = []
renameBody (body:bodyRest) = renameVars body : renameBody bodyRest

renameVars :: ASTNode -> ASTNode
renameVars (Predicate x []) = Predicate x []
renameVars (Predicate x (y:ys)) = Predicate x (renameVars2 (y:ys))

renameVars2 [] = []
renameVars2 ((PredVariable x):xs) = PredVariable (x++"\'") : renameVars2 xs
renameVars2 ((Predicate x y):xs) = Predicate x (renameVars2 y) : renameVars2 xs

-- Get all the variables of a given query
getVariables :: [ASTNode] -> [ASTNode]
getVariables [] = []
getVariables [PredVariable x] = [PredVariable x]
getVariables [Fact x] = getVariables [x]
getVariables [Predicate _ args] = getVariables args
getVariables (x:xs) = getVariables [x] ++ getVariables xs

-- Check if a fact only contains discrete variables as arguements
-- onlyVars :: [ASTNode] -> [String] -> Bool
-- onlyVars [] _ = False
-- onlyVars [PredVariable x] found = not (member x found)
-- onlyVars ((PredVariable x):xs) found
--     | member x found = False
--     | otherwise = onlyVars xs (found ++ [x])
-- onlyVars ((Predicate _ _):_) _ = False
module TopDownEval where
import PrologParser
import PatternMatch

-- G: Trexon stoxos pros ikanopoihsh
-- Stoiba GS: Upoloipoi stoixoi
-- VL: Mexri stigmhs ana8eseis timwn pou exoun gine se metablhtes
-- SS: Stoiba gia na uparxei h dunatothta opis8odromhshs se prohgoumenes katastaseis
-- Ta stoixeia ths SS einai 4-tuples: (G, C, GS, VL)
-- C: Mia protash

-- Evaluate a query using top down approach
topDownEvaluate :: ASTNode -> [ASTNode] -> [(ASTNode, ASTNode)]
-- If no matching rule/fact was found, query is insatisfiable
topDownEvaluate _ [] = [(PredVariable "evalfail", PredVariable "evalfail")]

-- If a fact was found, try to match it
topDownEvaluate (Fact (Predicate x xs)) (Fact (Predicate y ys) : rest)
    -- If the matching was successful, return the MGU
    | x == y && result /= [(PredVariable "FALSE", PredVariable "FALSE")] = result
    -- Else if the matching failed, continue searching
    | otherwise = topDownEvaluate (Fact (Predicate x xs)) rest
    where result = unify (Predicate x xs) (Predicate y ys)
    
-- FOR NOW IGNORE RULES
topDownEvaluate x ((Rule y ys) : rest) = topDownEvaluate x rest

-- --for reference, diavase vivlio takh selida 38
-- findMatches query [] = []
-- findMatches query (pred:predicates) = initializeEval query pred

-- --step 1,2,3
-- initializeEval query (Rule x (r:reqs)) =
--     --step 5,6 check for unification
--                 --  G
--     if (unify query r) != [(PredVariable "FAIL", PredVariable "FAIL")] 
--                     -- C     GS   VL SS
--         then evaluate query [reqs,[],[]] --will need to try and unify with all
--                                              --rule requirements
--         else False

-- --step 4
-- -- if we talking bout a fact the GS list is empty therefore fullfilled query
-- intializeEval query (Fact x) = unify query x
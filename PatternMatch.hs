module PatternMatch where
import PrologParser
import PrologLexer

-------------------- Unify --------------------
-- Unification algorithm reference: https://www.javatpoint.com/ai-unification-in-first-order-logic
-- Find the MGU using unify algorithm. Unify returns a list of 2-tuples: (ASTNode1, ASTNode2)
-- This means that we assign ASTNode2 to ASTNode1. For example, if MGU = [(X, s(15)), (Y,X)], we assign
-- the predicate s(15) to the variable X and assign variable X to variable Y
-- Empty MGUs are represented by the empty list: []
-- When an instance of unify2 fails, unify returns [(PredVariable "FALSE", PredVariable "FALSE")]
unify :: ASTNode -> ASTNode -> [(ASTNode, ASTNode)]
unify statement1 statement2
    | member (PredVariable "FALSE", PredVariable "FALSE") result = [(PredVariable "FALSE", PredVariable "FALSE")]
    | otherwise = result
    where result = unify2 statement1 statement2

unify2 :: ASTNode -> ASTNode -> [(ASTNode, ASTNode)]
-- When trying to unify Facts, ignore the 'Fact' prefix
unify2 (Fact x) (Fact y) = unify2 x y
-- Case: Unify 2 variables
-- If they are the same variable, MGU is empty. Else, assign y to x 
unify2 (PredVariable x) (PredVariable y)
    | x == y = []
    | otherwise = [(PredVariable x, PredVariable y)]

-- Case: Unify a variable with a predicate
-- If x occurs in y, unifying is impossible. Else, assign the predicate to x
unify2 (PredVariable x) (Predicate y yargs)
    | foundInArgs x yargs = [(PredVariable "FALSE", PredVariable "FALSE")]
    | otherwise = [(PredVariable x, Predicate y yargs)]
-- If the variable is on the right side, symmetrically call the function
unify2 (Predicate y yargs) (PredVariable x) = unify2 (PredVariable x) (Predicate y yargs)

-- SWTHRHS LISTES--
unify2 (Predicate "[]" x) (Predicate "[]" [PredVariable "|",PredVariable y])
    = [(PredVariable y,Predicate "[]" x)] 

unify2 (Predicate "[]" [(PredVariable "|"),PredVariable y]) (Predicate "[]" x)
    = [(PredVariable y,Predicate "[]" x)] 

unify2 (Predicate "[]" (x:xargs)) (Predicate "[]" (y:yargs))
   | (unify2 x y /= [(PredVariable "FALSE", PredVariable "FALSE")]) = (unify2 x y) ++ unify2 (Predicate "[]" xargs) (Predicate "[]" yargs)
   | otherwise = unify2 (Predicate "[]" xargs) (Predicate "[]" yargs)
-----

-- Case: Unify 2 predicates
-- If they have different names or different number of arguements, return FALSE
-- [A,B,C|D] is possible which means [1,2,3,4,5,6,7] -> A=1 B=2 C=3 D=[4,5,6,7]
-- unify (Predicate "[]" xargs) (Predicate y yargs)
-- Otherwise, find substitution set and return it
unify2 (Predicate x []) (Predicate y [])
    | x == y = []
    | otherwise = [(PredVariable "FALSE", PredVariable "FALSE")]
unify2 (Predicate x xargs) (Predicate y yargs)
    | x /= y || length xargs /= length yargs = [(PredVariable "FALSE", PredVariable "FALSE")]
    | otherwise = fillSubstitutionSet xargs yargs
 
-- 2 stoixeia -> enopoiisi
-- stoixeio TailOp -> enopoiisi stoixeiou me oti menei
-- 2 stoixeia (stoixeio me keno ) -> FAIL

-- Returns true when a is a member of the given list. False otherwise
member :: (Eq a) => a -> [a] -> Bool
member x [] = False
member x (y:ys) | x==y = True
                | otherwise = member x ys

-- if substitutions are more than 1 we add them here
fillSubstitutionSet :: [ASTNode] -> [ASTNode] -> [(ASTNode, ASTNode)]
-- Given list of arguements has ended. Stop filling substitution set
fillSubstitutionSet [] [] = []
fillSubstitutionSet (x:xs) (y:ys)
    -- If mgu is empty, just move to the next arguements
    | null mgu = fillSubstitutionSet xs ys
    -- If unification failed, then unification is not possible
    | mgu == [(PredVariable "FALSE", PredVariable "FALSE")] = [(PredVariable "FALSE", PredVariable "FALSE")]
    -- Else, save the unification, apply mgu to the rest of the arguements and continue
    | otherwise = mgu ++ fillSubstitutionSet (applyMgu mgu xs) (applyMgu mgu ys)
    where mgu  = unify x y

-- Returns true if variable X occurs in an arguement. False otherwise.
foundInArgs :: String -> [ASTNode] -> Bool
-- Case: Only arguement is a predicate. Check if x exists in its arguements
foundInArgs x [Predicate y yargs] = foundInArgs x yargs
-- Case: Only arguement is a variable. Check if they are the same variable
foundInArgs x [PredVariable y]
    | x == y = True
    | otherwise = False
-- Case: Multiple arguements. Check if x is in any one of them
foundInArgs x [] = False
foundInArgs x (y:ys)
    | not (foundInArgs x [y]) = foundInArgs x ys
    | otherwise = True
----------------------------------------

-------------------- Apply MGU --------------------
-- Apply the MGU to a whole body
applyMgu :: [(ASTNode, ASTNode)] ->[ASTNode] -> [ASTNode]
-- If the MGU is empty, then no replacements exist. Return the body with no changes
applyMgu [] body = body
-- If there is only 1 replacement, apply it to the body
applyMgu [replacement] body = applyReplacement replacement body
-- Apply current replacement to the body. Apply other replacements to the new body
applyMgu (replacement : rest) body = applyMgu rest (applyReplacement replacement body)

-- Apply 1 replacement to a whole body
applyReplacement :: (ASTNode, ASTNode) -> [ASTNode] -> [ASTNode]
applyReplacement _ [] = []
-- If a predicate occurs, apply the replacement on its arguements and move to next statement
applyReplacement replacement ((Predicate pred args) : bodyRest) = Predicate pred newArgs : applyReplacement replacement bodyRest
    where newArgs = applyReplacement replacement args
-- If an arguement occurs, attempt to replace it and move to the next statement
applyReplacement (PredVariable x, replacement) ((PredVariable y) : bodyRest)
    | x == y = replacement : applyReplacement (PredVariable x, replacement) bodyRest
    | otherwise = PredVariable y : applyReplacement (PredVariable x, replacement) bodyRest
----------------------------------------

-------------------- Compose MGUs --------------------
-- Compose a list of MGUs and produce a more concise MGU
composeMgu :: [(ASTNode, ASTNode)] -> [(ASTNode, ASTNode)]
composeMgu mgu = composeMgu2 mgu mgu

composeMgu2 :: [(ASTNode, ASTNode)] -> [(ASTNode, ASTNode)] -> [(ASTNode, ASTNode)]
composeMgu2 [] mgu = mgu
composeMgu2 (x:xs) mgu
    | equal composed mgu = composeMgu2 xs mgu
    | otherwise = composeMgu2 composed composed
    where composed = composeList x mgu

-- Compose a substitution with a list
composeList :: (ASTNode, ASTNode) -> [(ASTNode, ASTNode)] -> [(ASTNode, ASTNode)]
composeList x [] = [x]
composeList x (y:ys)
    | x == y = composeList x ys
    | null composed = y : composeList x ys
    | otherwise = composed ++ ys
    where composed = compose x y

-- Compose 2 substitutions
compose :: (ASTNode, ASTNode) -> (ASTNode, ASTNode) -> [(ASTNode, ASTNode)]
compose x y
    | null composed && not (null (compose2 y x)) = compose2 y x
    | null composed && null (compose2 y x) = []
    | otherwise = composed
    where composed = compose2 x y

compose2 :: (ASTNode, ASTNode) -> (ASTNode, ASTNode) -> [(ASTNode, ASTNode)]
compose2 (PredVariable x1, PredVariable x2) (PredVariable y1, y2)
    | x2 == y1 = [(PredVariable x1, y2)]
    | otherwise = []
compose2 (PredVariable x1, Predicate x2 []) _ = []
compose2 (PredVariable x1, Predicate x2 args) (PredVariable y1, y2)
    | replaced == args = []
    | otherwise = [(PredVariable x1, Predicate x2 replaced)]
    where replaced = applyReplacement (PredVariable y1, y2) args

-- Returns true if 2 lists are equal. 2 lists are considered equal if they 
-- have the same items. Their order doesn't matter
equal :: Eq a => [a] -> [a] -> Bool
equal [] _ = True
equal (x:xs) y
    | member x y = equal xs y
    | otherwise = False
----------------------------------------
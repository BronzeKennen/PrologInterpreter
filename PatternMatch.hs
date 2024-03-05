module PatternMatch where
import PrologParser
import PrologLexer

-- Unification algorithm reference: https://www.javatpoint.com/ai-unification-in-first-order-logic
-- Find the MGU using unify algorithm. Unify returns a list of 2-tuples: (ASTNode1, ASTNode2)
-- This means that we assign ASTNode2 to ASTNode1. For example, if MGU = [(X, s(15)), (Y,X)], we assign
-- the predicate s(15) to the variable X and assign variable X to variable Y
-- Empty MGUs are represented by the empty list: []
-- When an instance of unify fails, it returns [(PredVariable "FAIL", PredVariable "FAIL")]

unify :: ASTNode -> ASTNode -> [(ASTNode, ASTNode)]
-- When unify will be called from the main program, both of the arguements will be Facts (maybe not idk)
unify (Fact x) (Fact y) = unify x y

-- Case: Unify 2 variables
-- If they are the same variable, MGU is empty. Else, assign y to x 
unify (PredVariable x) (PredVariable y)
    | x == y = []
    | otherwise = [(PredVariable x, PredVariable y)]

-- Case: Unify a variable with a predicate
-- If x occurs in y, unifying is impossible. Else, assign the predicate to x
unify (PredVariable x) (Predicate y yargs)
    | foundInArgs x yargs = [(PredVariable "FAIL", PredVariable "FAIL")]
    | otherwise = [(PredVariable x, Predicate y yargs)]
-- If the variable is on the right side, symmetrically call the function
unify (Predicate y yargs) (PredVariable x) = unify (PredVariable x) (Predicate y yargs)

-- Case: Unify 2 predicates
-- If they have different names or different number of arguements, return FAIL
-- Otherwise, find substitution set and return it
unify (Predicate x xargs) (Predicate y yargs)
    | x /= y || length xargs /= length yargs = [(PredVariable "FAIL", PredVariable "FAIL")]
    | otherwise = fillSubstitutionSet xargs yargs

-- Apo edw kai meta ksekinaei to Step 5 apo to site
-- -- Find substitution set and return it
fillSubstitutionSet [] [] = []
fillSubstitutionSet (x:xs) (y:ys) 
    | length (unified) == 0 = []
    -- | length (unify x y) /= 0 = unify x y ++ fillSubstitutionSet xs ys
    | length (unified) /= 0 = unified ++ fillSubstitutionSet (replaceOccurence unified xs) (replaceOccurence unified ys)
    | otherwise = [(PredVariable "FAIL", PredVariable "FAIL")]
    where unified  = unify x y

replaceOccurence _ [] = []
replaceOccurence [(var,atom)] (x:xs)
    | x == var = atom: replaceOccurence [(var,atom)] xs
    | otherwise = x: replaceOccurence [(var,atom)] xs

-- findSubSet :: [ASTNode] -> [ASTNode] -> [(ASTNode, ASTNode)] -> [(ASTNode, ASTNode)]
-- findSubSet (x:xs) (y:ys) currentSet = 

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
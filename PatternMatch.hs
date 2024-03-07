module PatternMatch where
import PrologParser
import PrologLexer
import Debug.Trace

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

-- Returns true when a is a member of the given list. False otherwise
member :: (Eq a) => a -> [a] -> Bool
member x [] = False
member x (y:ys) | x==y = True
                | otherwise = member x ys

unify2 :: ASTNode -> ASTNode -> [(ASTNode, ASTNode)]
-- When trying to unify Facts, ignore the 'Fact' prefix
unify2 (Fact x) (Fact y) = unify2 x y
-- pattern match a rule head
unify2 (Fact x) (Rule y _) = unify2 x y
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


unify2 (Predicate "[]" x) (Predicate "[]" [(PredVariable "|"),PredVariable y])
    = [(PredVariable y,Predicate "[]" x)] 

unify2 (Predicate "[]" [(PredVariable "|"),PredVariable y]) (Predicate "[]" x)
    = [(PredVariable y,Predicate "[]" x)] 

unify2 (Predicate "[]" (x:xargs)) (Predicate "[]" (y:yargs))
   | (unify2 x y /= [(PredVariable "FALSE", PredVariable "FALSE")]) = (unify2 x y) ++ unify2 (Predicate "[]" xargs) (Predicate "[]" yargs)
   | otherwise = unify2 (Predicate "[]" xargs) (Predicate "[]" yargs)
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


-- if substitutions are more than 1 we add them here
fillSubstitutionSet [] [] = []
fillSubstitutionSet (x:xs) (y:ys)
    | null unified = fillSubstitutionSet xs ys
    | not (null unified) = unified ++ fillSubstitutionSet (replaceOccurence unified xs) (replaceOccurence unified ys)
    | otherwise = [(PredVariable "FALSE", PredVariable "FALSE")]
    where unified  = unify x y

-- replace every occurence of a variable with the atom that we unified it with
replaceOccurence _ [] = []

replaceOccurence [(var,atom)] (Predicate s (x:xs):ys)
    | s == "[]" = Predicate s (x:xs): replaceOccurence [(var,atom)] ys
    | otherwise = [Predicate s (replaceOccurence [(var,atom)] (x:xs))]

replaceOccurence [(var,atom)] (Predicate x [] : xs) = Predicate x []: replaceOccurence [(var,atom)] xs

replaceOccurence [(var,atom)] (x:xs)
    | x == var = atom: replaceOccurence [(var,atom)] xs
    | otherwise = x: replaceOccurence [(var,atom)] xs

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
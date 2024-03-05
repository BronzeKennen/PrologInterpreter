module PrologParser where
import PrologLexer
import Data.String

-- ASTNode data type
data ASTNode
    = Fact ASTNode
    | Rule ASTNode [ASTNode]
    | Predicate String [ASTNode]
    | PredVariable String
    deriving(Show)

-- Get the type of a token
getTokenType :: Token -> TokenType
getTokenType (Token tokentype _) = tokentype

-- Turn a token to string
getIdentifier :: Token -> String
getIdentifier (Token _ (Just s)) = s

-- Check if a token is a rule
isRule :: [Token] -> Bool
isRule [] = False
isRule (x:xs)
    | getTokenType x == PredOperator = True
    | otherwise = isRule xs

-- Check if all tokens are valid
checkIsValid :: [[Token]] -> Bool
checkIsValid [] = True
checkIsValid (x:xs)
    -- Before checking if a token is valid make sure that it doesn't start with a variable
    | getTokenType (head x) /= Lower = False
    | isRule x && (ruleInitCheck first 0)&& ruleIsValid second 0 = checkIsValid xs
    | factIsValid x 0 = checkIsValid xs
    | otherwise = False
    where (first, second) = splitAt (indexOf PredOperator x) x


-- Handle case where '.' is missing somewhere
terminatorAbsenceCheck [x] = True
terminatorAbsenceCheck (x:xs) = if (getTokenType x /= Terminator) 
                                then False 
                                else True
-- Check if the syntax of the Fact is valid
factIsValid :: [Token] -> Int -> Bool
factIsValid [x] n  
    | (n == 0 && getTokenType x == Terminator) = True
    | otherwise = False

-- Facts are invalid if the contain variables, wrongly placed commas or wrong parenthesis pairs
factIsValid (x:y:xs) n
    -- Check if parenthesis are well balanced
    | getTokenType x == LeftParen = factIsValid (y:xs) (n+1)
    | (getTokenType x == RightParen && n == 1) = terminatorAbsenceCheck (y:xs)
    | getTokenType x == RightParen = factIsValid (y:xs) (n-1)
    -- Lower and upper case strings are acceptable
    | getTokenType x == Lower || getTokenType x == Upper = factIsValid (y:xs) n
    -- Integers are acceptable
    | getTokenType x == Int = factIsValid (y:xs) n
    -- After a comma, an integer or a string must follow
    | getTokenType x == CommaOperator && (getTokenType y == Int || getTokenType y == Lower || getTokenType y == Upper) = factIsValid (y:xs) n
    | otherwise = False

-- Check if the syntax of the Rule is valid
-- FOR NOW IGNORE RULES

-- based on call check for pred operator, comma, or terminator
operatorAbsenceCheck (x:xs) = if (getTokenType x == CommaOperator || getTokenType x == Terminator) 
                              then True
                              else False 
    --might add terminator to remove terminatoAbsenceCheck later

ruleInitCheck [x] n = if (n == 0 && (getTokenType x) == PredOperator) then True else False
ruleInitCheck (x:y:xs) n
    | getTokenType x == LeftParen = ruleInitCheck (y:xs) (n+1)
    | getTokenType x == RightParen = ruleInitCheck (y:xs) (n-1)
    | getTokenType x == CommaOperator && (getTokenType y == Int || getTokenType y == Lower || getTokenType y == Upper) = ruleInitCheck (y:xs) n
    | getTokenType x == Lower || getTokenType x == Upper = ruleInitCheck (y:xs) n
    -- Integers are acceptable
    | getTokenType x == Int = ruleInitCheck (y:xs) n
    | otherwise = False

ruleIsValid :: [Token] -> Int -> Bool
ruleIsValid [] _ = True
ruleIsValid (x:y:xs) n 
    | getTokenType x == LeftParen = ruleIsValid (y:xs) (n+1)
    | (getTokenType x == RightParen && n == 1) = operatorAbsenceCheck (y:xs) 
    | getTokenType x == Upper && n > 0= ruleIsValid (y:xs) n
    | getTokenType x == Lower || getTokenType x == Upper = ruleIsValid (y:xs) n
    | getTokenType x == RightParen = ruleIsValid (y:xs) (n-1)
    | getTokenType x == Int = ruleIsValid (y:xs) n
    | getTokenType x == CommaOperator && (getTokenType y == Int || getTokenType y == Lower || getTokenType y == Upper) = ruleIsValid (y:xs) n
-- Parse the given tokens and create the Abstract Syntax Tree
parse :: [[Token]] -> [ASTNode]
parse [] = []
parse (x:xs)
    -- If token is a rule, parse it by utilizing parseAllArgs
    | isRule x = Rule (predInit first) (parseAllArgs (second ++ [Token RightParen Nothing]) 1) : parse xs
    -- Else if a token is simply a fact, parse it using predInit
    | otherwise = Fact (predInit x): parse xs
    where (first, second) = splitAt (indexOf PredOperator x) x

-- Get zero based index of element x inside list (y:ys)
indexOf :: TokenType -> [Token] -> Int
indexOf x (y:ys)
    | x /= getTokenType y = indexOf x ys +1
    | otherwise = 1

-- Parse a predicate to an ASTNode
predInit :: [Token] -> ASTNode
predInit (x:xs)
    | getTokenType x == LeftParen || getTokenType x == RightParen = predInit xs -- Ignore left right parenthesis
    | getTokenType x == Int || (getTokenType x == Lower && getTokenType (head xs) /= LeftParen) = Predicate (getIdentifier x) []                    -- Integers parsing: Predicate "Int" []
    | getTokenType x == Upper = PredVariable (getIdentifier x)                  -- Variables parsing: PRedVariable "X"
    | getTokenType x == Lower = Predicate (getIdentifier x) (parseAllArgs xs 0) -- Predicates parsing: Predicate "Name" [Arguements]

-- Parse all arguements of a predicate
parseAllArgs :: [Token] -> Int -> [ASTNode]
parseAllArgs (x:xs) n
    | getTokenType x == LeftParen = parseAllArgs xs (n+1)   -- Raise parenthesis counter when entering predicates inside predicates
    | getTokenType x == RightParen && n == 1 = []           -- If at the end arguement, return
    | getTokenType x == RightParen = parseAllArgs xs (n-1)  -- Loweer parenthesis counter when exiting predicates inside predicates
    | n == 1 && getTokenType x == Int || (getTokenType x == Lower && getTokenType (head xs) /= LeftParen) = Predicate (getIdentifier x) [] : parseAllArgs xs n                      -- Parse ints
    | n == 1 && getTokenType x == Upper = PredVariable (getIdentifier x) : parseAllArgs xs n                    -- Parse vars
    | n == 1 && getTokenType x == Lower = Predicate (getIdentifier x) (parseAllArgs xs 0) : parseAllArgs xs n   -- Parse pred
    | otherwise = parseAllArgs xs n     -- Used to ignore commas

-- MALAKA OUTE MIA EKFWNHSH DEN MPOROUN NA GRAPSOUN SWSTA
-- AYTO EINAI TO SWSTO OUTPUT TOU PARSER. TO BRHKA STO DISC

-- a(X) :- a(s(X)), b(Y).
-- b(15).
-- [
--     Rule (Predicate "a" [PredVariable "X"]) [
--       Predicate "a" [
--         Predicate "s" [PredVariable "X"]
--       ],
--       Predicate "b" [PredVariable "Y" ] 
--      ],
--        Fact (Predicate "b" [Predicate "15" []])
-- ] 

-- b(mike).
-- Fact (Predicate "b" [Predicate "mike" []])
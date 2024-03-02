module PrologParser where
import PrologLexer
import Data.Char (isLower)
import Data.Char (isDigit)
import Data.Char (isUpper)
import Data.String
import System.Exit

data ASTNode
    = Fact ASTNode
    | Rule ASTNode [ASTNode]
    | Predicate String [ASTNode]
    | PredVariable String
    deriving(Show)
    
getTokenType :: Token -> TokenType
getTokenType (Token tokentype _) = tokentype

getIdentifier :: Token -> String
getIdentifier (Token _ (Just s)) = s

-- Check if a token is a rule
isRule :: [Token] -> Bool
isRule [] = False
isRule (x:xs)
    | getTokenType x == PredOperator = True
    | otherwise = isRule xs

-- parse :: [[Token]] -> [ASTNode]
-- parse [] = []
-- parse (x:xs)
--     | isRule x = ruleInit x : parse xs
--     | otherwise = Fact predInit x : parse xs


-- Check if the syntax of the Fact is correct
correctSyntax :: [Token] -> Int -> Bool
correctSyntax [Token Terminator Nothing] n
    | n == 0 = True
    | otherwise = False
-- Facts are incorrect if the contain variables, wrongly placed commas or wrong parenthesis pairs
correctSyntax (x:y:xs) n
    | getTokenType x == LeftParen = correctSyntax (y:xs) (n+1)
    | getTokenType x == RightParen = correctSyntax (y:xs) (n-1)
    | getTokenType x == Identifier && not (isVariable (getIdentifier x)) = correctSyntax (y:xs) n
    | getTokenType x == Int = correctSyntax (y:xs) n
    | getTokenType x == CommaOperator && (getTokenType y == Int || getTokenType y == Identifier) = correctSyntax (y:xs) n
    | otherwise = False

-- Check if given string is a variable
isVariable :: String -> Bool
isVariable (x:xs) = isUpper x

-- parse :: [Token] -> [ASTNode]
-- parse [] = []
-- parse (x:xs)
    -- | (getTokenType x) == Identifier = parseIdentifier (x:xs)  
    -- | (getTokenType x == PredOperator) = parse xs 
    -- | otherwise = parse xs 
-- 
-- parseIdentifier (x:xs) 
    -- | (isLower s) =  [Predicate (s:ls) (parse xs)]++ parse xs 
    -- | (isUpper s || isDigit s) =  [PredVariable (s:ls)] ++ parse xs 
    -- | otherwise = parse xs
    -- where
        -- (s:ls) = getIdentifier x
-- 
-- parseNext (x:xs)
    -- | (getTokenType x == CommaOperator) = parseIdentifier xs
    -- | (getTokenType x == RightParen) = []
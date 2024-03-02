module PrologParser where
import PrologLexer
import Data.Char (isLower)
import Data.Char (isDigit)
import Data.Char (isUpper)
import Data.String


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
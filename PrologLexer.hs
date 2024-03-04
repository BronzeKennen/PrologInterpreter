module PrologLexer where
import Data.Char (isDigit, isLower, isUpper, isAlphaNum) 
import Data.String
import Data.Maybe

-- These are all the available tokens
data TokenType
  = Int
  | Lower
  | Upper
  | CommaOperator
  | Terminator
  | TailOperator
  | PredOperator 
  | LeftParen
  | RightParen
  | LeftBracket
  | RightBracket
  deriving (Show,Eq)
data Token = Token TokenType (Maybe String) deriving (Show)

-- Tokenize the input file. The lexer produces a list containing lists of tokens
-- Each list of tokens corresponds to a single statement
tokenizeInput :: String -> [[Token]]
tokenizeInput [] = []
tokenizeInput (x:xs) = if(length (tokenize (x:xs)) == 0) 
                       then tokenizeInput xs
                       else tokenize(x:xs) : tokenizeInput ys 
  where
    (y:ys)= dropWhile (/= '.') xs

-- Tokenize a statement
tokenize :: String -> [Token]
tokenize [] = []
tokenize (':':'-':xs) = Token PredOperator Nothing : tokenize xs 
tokenize (x:xs)
  | isLower x = Token Lower (Just (buffer Lower (x:xs))) : tokenize remaining 
  | isUpper x = Token Upper (Just (buffer Upper (x:xs))) : tokenize remaining 
  | isDigit x = Token Int (Just (buffer Int (x:xs))) : tokenize remaining
  | x == ',' = Token CommaOperator Nothing: tokenize xs 
  | x == '.' = [Token Terminator Nothing] 
  | x == '|' = Token TailOperator Nothing :tokenize xs 
  | x == '[' = Token LeftBracket Nothing : tokenize xs
  | x == ']' = Token RightBracket Nothing : tokenize xs
  | x == '(' = Token LeftParen Nothing :tokenize xs
  | x == ')' = Token RightParen Nothing :tokenize xs
  | otherwise = tokenize xs 
  where
    remaining = dropWhile isAlphaNum xs

buffer :: TokenType -> String -> String-- String -> Token 
buffer _ ""  = ""
buffer t (x:xs) 
  | t == Int || t == Lower || t == Upper = 
    if not (isAlphaNum x) 
      then buffer t ""
      else x : buffer t xs
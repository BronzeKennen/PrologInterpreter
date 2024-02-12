module PrologLexer where
import Data.Char (isDigit) 
import Data.Char (isLower)
import Data.Char (isUpper)
import Data.Char (isAlphaNum)
import Data.String
data TokenType
  = Int
  | Identifier
  | Operator -- .
  | LeftParen
  | RightParen
  | LeftBracket
  | RightBracket
  deriving (Show,Eq)
data Token = Token TokenType String deriving (Show)

tokenize :: String -> [Token]-- String -> Token
tokenize [] = []

tokenize (':':'-':xs) = Token Operator (":-") : tokenize xs 
tokenize (x:xs)
  | isUpper x || isLower x = Token Identifier (buffer Identifier (x:xs)) : tokenize remaining
  | isDigit x = Token Int (buffer Int (x:xs)) : tokenize remaining
  | x == ',' || x == '.' || x == '|' = (Token Operator (show x)) :tokenize xs 
  | x == '[' = Token LeftBracket ("[") : tokenize xs
  | x == ']' = Token RightBracket ("]") : tokenize xs
  | x == '(' = Token LeftParen ("(") :tokenize xs
  | x == ')' = Token RightParen (")") :tokenize xs
  | otherwise = tokenize xs 
  where
    remaining = dropWhile isAlphaNum xs


buffer :: TokenType -> String -> String-- String -> Token 
buffer _ ""  = ""
buffer t (x:xs) 
  | t == Int || t == Identifier = 
    if not (isAlphaNum x) 
      then buffer t ""
      else [x] ++ buffer t xs
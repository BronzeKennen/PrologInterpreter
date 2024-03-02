module PrologLexer where
import Data.Char (isDigit) 
import Data.Char (isLower)
import Data.Char (isUpper)
import Data.Char (isAlphaNum)
import Data.String
import Data.Maybe
data TokenType
  = Int
  | Identifier
  | CommaOperator -- .
  | Terminator
  | TailOperator
  | PredOperator 
  | LeftParen
  | RightParen
  | LeftBracket
  | RightBracket
  deriving (Show,Eq)
data Token = Token TokenType (Maybe String) deriving (Show)

tokenize2 (x:xs) = [tokenize (x:xs)] ++ [(tokenize ys)]
  where
    (y:ys)= dropWhile (/= '.') xs

tokenize :: String -> [Token]-- String -> Token
tokenize [] = []

tokenize (':':'-':xs) = Token PredOperator Nothing : tokenize xs 
tokenize (x:xs)
  | isUpper x || isLower x = Token Identifier (Just (buffer Identifier (x:xs))) : tokenize remaining 
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
  | t == Int || t == Identifier = 
    if not (isAlphaNum x) 
      then buffer t ""
      else [x] ++ buffer t xs
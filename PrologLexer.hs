module PrologLexer where
import Data.Char (isDigit)
import Data.String
data TokenType
  = Int
  | String
  deriving (Show)
data Token = Token TokenType String deriving (Show)

tokenize :: String -> [Token]-- String -> Token
tokenize [] = []
tokenize (' ':xs) =  tokenize xs
tokenize ('\t':xs) =  tokenize xs
tokenize ('\n':xs) =  tokenize xs
tokenize (x:xs) = if isDigit x 
                    then Token Int (buffer (x:xs)) : tokenize rest
                    else tokenize xs
                  where
                    rest = dropWhile isDigit xs

buffer :: String -> String-- String -> Token 
buffer "" = ""
buffer (' ':xs) = buffer ""
buffer (x:xs) =  [x] ++ buffer xs 
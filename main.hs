import PrologLexer
import PrologParser
main = do
  file_content <- getContents
  let tokens = tokenize2 file_content
  print tokens
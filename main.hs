import PrologLexer
import PrologParser
main = do
  file_content <- getContents
  let tokens = tokenize file_content
  let parseTree = parse tokens
  print parseTree
import PrologLexer
import PrologParser
main = do
  file_content <- getContents
  let tokens = tokenize2 file_content
  -- print tokens
  let errorcheck = correctSyntax (last tokens) 0
  print errorcheck
  -- let parsed = parse tokens
  -- print parsed
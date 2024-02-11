import PrologLexer
main = do
  file_content <- getContents
  let tokens = tokenize file_content
  print tokens
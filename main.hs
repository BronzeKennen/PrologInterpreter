import PrologLexer
import PrologParser
import System.Exit

main = do
  -- Get file from input
  file_content <- getContents
  -- Apply lexer to file to get the tokens
  let tokens = tokenizeInput file_content
  
  -- Check if the given program has the correct syntax
  let isValid = checkIsValid tokens
  if not isValid then die "ERROR: Program's syntax is not correct. Exiting..."
  
  else do
    -- Print in a pretty way :D
    putStr "\n-----These are the tokens-----\n"
    mapM_ print tokens

    putStr "\n"

    -- Print in a pretty way :D
    let parsed = parse tokens
    putStr "-----These are the parsed tokens-----\n"
    mapM_ print parsed
import PrologLexer
import PrologParser
import PatternMatch
import System.Exit
import System.IO

main = do
  -- Get file from input
  filename <- getLine
  file_content <- readFile filename
  -- Apply lexer to file to get the tokens
  let tokens = tokenizeInput file_content
  
  let parsed = parse tokens
  -- Check if the given program has the correct syntax
  let isValid = checkIsValid tokens
  if not isValid then die "ERROR: Program's syntax is not correct. Exiting..."
  
  else do
    userInput parsed 

userInput:: [a0] -> IO b
userInput parsed = do
  inp <- getLine
  if (inp == "halt.") then die "Program Exit." 
  else do 
    let tokenedInp = tokenizeInput inp
    let validity = checkIsValid tokenedInp
    if (validity == False) then putStr "Invalid input.\n"
    else do
      let parsedInp = parse tokenedInp 
      -- s(X)),Y). parsed
      print "input:\n"
      print parsedInp
  userInput parsed
      
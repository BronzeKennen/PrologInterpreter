import PrologLexer
import PrologParser
import PatternMatch
-- import TopDownEval
import System.Exit
import System.IO

main = do
  -- Get file from input
  filename <- getLine
  file_content <- readFile filename
  -- Apply lexer to file to get the tokens
  let tokens = tokenizeInput file_content
-- 
  let parsed = parse tokens
  -- Check if the given program has the correct syntax
  let isValid = checkIsValid tokens
  if not isValid then die "ERROR: Program's syntax is not correct. Exiting..."
-- 
  else do
    print parsed
    -- userInput parsed
-- 
-- userInput:: [a0] -> IO b
-- Read user input
userInput :: [ASTNode] -> IO b
userInput parsed = do
  putStr "\nI'm trying to match p(s(s(X)),Y): "
  print (head parsed)
  -- If user inputs 'halt.', exit the program
  inp <- getLine
  if inp == "halt." then die "Program Exit."
  -- Else, apply lexer, parser and unify to user input
  else do
    let tokenedInp = tokenizeInput inp
    let validity = checkIsValid tokenedInp
    -- Error message incase user input is invalid
    if not validity then putStr "Invalid input.\n"
    else do
      let parsedInp = head (parse tokenedInp)
      let mgu = unify parsedInp (head parsed)
      mapM_ print mgu
  userInput parsed
-- 
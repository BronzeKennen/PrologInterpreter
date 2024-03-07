import PrologLexer
import PrologParser
import PatternMatch
import TopDownEval
import System.Exit
import System.IO

main = do
  putStr "\nEnter a prolog file or enter \"halt.\" to exit: "
  hFlush stdout
  -- Get file from input
  filename <- getLine
  if filename == "halt." then die "Program Exit."
  else do
    file_content <- readFile filename
    -- Apply lexer to file to get the tokens
    let tokens = tokenizeInput file_content
  -- 
    let parsedFile = parse tokens
    -- Check if the given program has the correct syntax
    let isValid = checkIsValid tokens
    if not isValid then die "ERROR: Program's syntax is not correct. Exiting..."
  -- 
    else do
      mapM_ print parsedFile
      userInput parsedFile
-- 
-- userInput:: [a0] -> IO b
-- Read user input
userInput :: [ASTNode] -> IO b
userInput parsedFile = do
  putStr "\nEnter a query or enter \"halt.\" to exit: "
  hFlush stdout
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
      let answer = topDownEvaluate ((parsedInp)) parsedFile
      if(answer ==  [(PredVariable "FALSE", PredVariable "FALSE")]) 
          then print "no" 
          else if(answer == []) 
          then print "yes"
          else mapM_ print answer
  userInput parsedFile
-- 
import PrologLexer
import PrologParser
import PatternMatch
import TopDownEval
import System.Exit
import System.IO

main :: IO b
main = do
  -- Read a file from user 
  putStr "Enter a prolog file or enter \"halt.\" to exit: "
  hFlush stdout
  filename <- getLine
  if filename == "halt." then die "Program Exit."
  
  -- Read contents of file. Apply lexer and parser to file 
  else do
    file_content <- readFile filename
    let tokens = tokenizeInput file_content
    let parsedFile = parse tokens

    -- Check if the given program has the correct syntax
    let isValid = checkIsValid tokens
    if not isValid then die "ERROR: Program's syntax is not correct. Exiting..."

    -- If the file has the correct syntax, get queries from user until they enter 'halt.'
    else do
      readQueries parsedFile

-- Read queries from user
readQueries :: [ASTNode] -> IO b
readQueries parsedFile = do
  -- Read query/halt from user
  putStr "\nEnter a query or enter \"halt.\" to exit: "
  hFlush stdout
  inp <- getLine
  if inp == "halt." then die "Program Exit."

  -- Parse user query
  else do
    let userInput = tokenizeInput inp
    let validity = checkIsValid userInput
    if not validity then putStr "Invalid input.\n"
    else do

      -- If user entered a valid query, apply top down evaluation to it
      let query = head (parse userInput)
      let answer = topDownEvaluate query parsedFile
      let formattedAnswer = formatAnswer answer (getVariables [query])
      print formattedAnswer
      
  -- Repeat until user enters 'halt.'
  readQueries parsedFile
-- 
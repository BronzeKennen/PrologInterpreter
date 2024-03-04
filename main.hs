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
  
  -- Check if the given program has the correct syntax
  let isValid = checkIsValid tokens
  if not isValid then die "ERROR: Program's syntax is not correct. Exiting..."
  
  else do
    putStr "----- PARSED TOKENS -----\n\n"
    mapM_ print tokens
    let parsed = parse tokens
    putStr "-----These are the parsed tokens-----\n\n"
    mapM_ print parsed
    userInput

-- apo edw kai pera mike >:( gia na peraseis to file tha prepei na 
-- to grapseis me to pou treksei to programma :)
-- kaneis ./main kai molis treksei grafeis to file name.
userInput = do
  inp <- getLine
  if (inp == "halt.") then die "Program Exit." else do 
    putStrLn inp
    userInput
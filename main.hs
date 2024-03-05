import PrologLexer
import PrologParser
import PatternMatch
import System.Exit
import System.IO

main = do
  -- This should return empty
  let mgu = unify (PredVariable "X") (PredVariable "X")
  putStr "MGU1: "
  print mgu

  -- This should return {X/Y}
  let mgu = unify (PredVariable "X") (PredVariable "Y")
  putStr "MGU2: "
  print mgu

  -- This should return {X/s(0)}
  let mgu = unify (PredVariable "X") (Predicate "s" [PredVariable "0"])
  putStr "MGU3: "
  print mgu

  -- This should fail
  let mgu = unify (PredVariable "X") (Predicate "s" [PredVariable "X"])
  putStr "MGU4: "
  print mgu

  -- This should fail
  let mgu = unify (PredVariable "X") (Predicate "s" [PredVariable "15", PredVariable "X"])
  putStr "MGU5: "
  print mgu

  -- This should return {X/s(s(0))}
  let mgu = unify (Predicate "s" [Predicate "s" [PredVariable "0"]]) (PredVariable "X")
  putStr "MGU6: "
  print mgu

  -- This should fail
  let mgu = unify (Predicate "p" [PredVariable "0"]) (Predicate "s" [PredVariable "0"])
  putStr "MGU7: "
  print mgu

  -- why not ¯\_(ツ)_/¯
  let mgu = unify (Fact (PredVariable "X")) (Fact (PredVariable "Y"))
  putStr "MGU8: "
  print mgu



-- main = do
--   -- Get file from input
--   filename <- getLine
--   file_content <- readFile filename
--   -- Apply lexer to file to get the tokens
--   let tokens = tokenizeInput file_content

--   let parsed = parse tokens
--   -- Check if the given program has the correct syntax
--   let isValid = checkIsValid tokens
--   if not isValid then die "ERROR: Program's syntax is not correct. Exiting..."

--   else do
--     userInput parsed

-- -- userInput:: [a0] -> IO b
-- -- Read user input
-- userInput :: [ASTNode] -> IO b
-- userInput parsed = do
--   putStr "\nI'm trying to match p(s(s(X)),Y): "
--   print (head parsed)
--   -- If user inputs 'halt.', exit the program
--   inp <- getLine
--   if inp == "halt." then die "Program Exit."
--   -- Else, apply lexer, parser and unify to user input
--   else do
--     let tokenedInp = tokenizeInput inp
--     let validity = checkIsValid tokenedInp
--     -- Error message incase user input is invalid
--     if not validity then putStr "Invalid input.\n"
--     else do
--       let parsedInp = head (parse tokenedInp)
--       let mgu = unify parsedInp (head parsed)
--       mapM_ print mgu
--   userInput parsed

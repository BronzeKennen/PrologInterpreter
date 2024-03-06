import PrologLexer
import PrologParser
import PatternMatch
-- import TopDownEval
import System.Exit
import System.IO

main = do
--   This should return empty
  let mgu = unify (PredVariable "X") (PredVariable "X")
  putStr "MGU1: "
  print mgu

--   This should return {X/Y}
  let mgu = unify (PredVariable "X") (PredVariable "Y")
  putStr "MGU2: "
  print mgu

--   This should return {X/s(0)}
  let mgu = unify (PredVariable "X") (Predicate "s" [PredVariable "0"])
  putStr "MGU3: "
  print mgu

--   This should fail
  let mgu = unify (PredVariable "X") (Predicate "s" [PredVariable "X"])
  putStr "MGU4: "
  print mgu

--   This should fail
  let mgu = unify (PredVariable "X") (Predicate "s" [PredVariable "15", PredVariable "X"])
  putStr "MGU5: "
  print mgu

--   This should return {X/s(s(0))}
  let mgu = unify (Predicate "s" [Predicate "s" [PredVariable "0"]]) (PredVariable "X")
  putStr "MGU6: "
  print mgu

--   This should fail
  let mgu = unify (Predicate "p" [PredVariable "0"]) (Predicate "s" [PredVariable "0"])
  putStr "MGU7: "
  print mgu

--   why not ¯\_(ツ)_/¯
  let mgu = unify (Fact (PredVariable "X")) (Fact (PredVariable "Y"))
  putStr "MGU8: "
  print mgu

--   paradeigma koubarakh
  let mgu = unify (Predicate "p" [PredVariable "X", Predicate "b" [], PredVariable "Y", PredVariable "W", PredVariable "U"]) (Predicate "p" [Predicate "a" [], PredVariable "Y", PredVariable "Z", PredVariable "X", Predicate "f" [PredVariable "X"]])
  putStr "MGU9"
  print mgu
                   
  let mgu = unify (Predicate "p" [PredVariable "X", PredVariable "U"]) (Predicate "p" [Predicate "a" [], Predicate "f" [PredVariable "X"]])

  putStr "MGU10"
  print mgu
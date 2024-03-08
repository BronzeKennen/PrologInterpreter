import PrologLexer
import PrologParser
import PatternMatch
import System.Exit
import System.IO

main = do
--   This should return empty
  let mgu = unify (PredVariable "X") (PredVariable "X")
  putStr "MGU1: "
  print mgu
  putStr "\n"

--   This should return {X/Y}
  let mgu = unify (PredVariable "X") (PredVariable "Y")
  putStr "MGU2: "
  print mgu
  putStr "\n"

--   This should return {X/s(0)}
  let mgu = unify (PredVariable "X") (Predicate "s" [PredVariable "0"])
  putStr "MGU3: "
  print mgu
  putStr "\n"

--   This should fail
  let mgu = unify (PredVariable "X") (Predicate "s" [PredVariable "X"])
  putStr "MGU4: "
  print mgu
  putStr "\n"

--   This should fail
  let mgu = unify (PredVariable "X") (Predicate "s" [PredVariable "15", PredVariable "X"])
  putStr "MGU5: "
  print mgu
  putStr "\n"

--   This should return {X/s(s(0))}
  let mgu = unify (Predicate "s" [Predicate "s" [PredVariable "0"]]) (PredVariable "X")
  putStr "MGU6: "
  print mgu
  putStr "\n"

--   This should fail
  let mgu = unify (Predicate "p" [PredVariable "0"]) (Predicate "s" [PredVariable "0"])
  putStr "MGU7: "
  print mgu
  putStr "\n"

--   why not ¯\_(ツ)_/¯
  let mgu = unify (Fact (PredVariable "X")) (Fact (PredVariable "Y"))
  putStr "MGU8: "
  print mgu
  putStr "\n"

  -- This should return empty
  let mgu = unify (Predicate "b" [Predicate "15" [],Predicate "15" []]) (Predicate "b" [Predicate "15" [],Predicate "15" []]) 
  putStr "MGU9: "
  print mgu
  putStr "\n"
--   paradeigma koubarakh
  let mgu = unify (Predicate "p" [PredVariable "X", Predicate "b" [], PredVariable "Y", PredVariable "W", PredVariable "U"]) (Predicate "p" [Predicate "a" [], PredVariable "Y", PredVariable "Z", PredVariable "X", Predicate "f" [PredVariable "X"]])
  putStr "MGU10: "
  print mgu
  putStr "\n"
                   
  let mgu = unify (Predicate "q" [Predicate "[]" [],Predicate "[]" [],Predicate "0" []]) (Predicate "q" [PredVariable "A", PredVariable "B", PredVariable "N"])
  putStr "MGU11: "
  print mgu
  putStr "\n"

  let mgu = unify (Predicate "q" [Predicate "[]" [Predicate "1" [],Predicate "2" [], Predicate "3" []],Predicate "[]" [Predicate "1" [],Predicate "2" [], Predicate "3" []],Predicate "0" []]) (Predicate "q" [PredVariable "A", PredVariable "B", PredVariable "N"])
  putStr "MGU12: "
  print mgu
  putStr "\n"
  
  let mgu = unify (Fact (Predicate "q" [Predicate "[]" [Predicate "1" [], Predicate "2" [], Predicate "3" []]])) (Fact (Predicate "q" [Predicate "[]" [PredVariable "A",PredVariable "|",PredVariable "B"]]))
  putStr "MGU13: "
  print mgu
  putStr "\n"
  

  let mgu = unify (Fact (Predicate "q" [Predicate "[]" [Predicate "1" [], Predicate "2" [], Predicate "3" []]])) (Fact (Predicate "q" [Predicate "[]" [PredVariable "A",PredVariable "B"]]))
  putStr "Mgu14: "
  print mgu
  putStr "\n"

  let mgu = unify (Predicate "smurfers" [PredVariable "L",PredVariable "A"]) (Predicate "smurfers" [PredVariable "X", PredVariable "Y"])
  putStr "MGU15: "
  print mgu
  putStr "\n"
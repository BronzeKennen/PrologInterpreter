module TopDownEval where
import PrologParser
import PatternMatch

--for reference, diavase vivlio takh selida 38
findMatches query [] = []
findMatches query (pred:predicates) = initializeEval query pred

--step 1,2,3
initializeEval query (Rule x (r:reqs)) =
    --step 5,6 check for unification
    if (unify query r) != [(PredVariable "FAIL", PredVariable "FAIL")] 
                            --G  GS   VL SS
        then evaluate query [(r:reqs),[],[]] --will need to try and unify with all
                                             --rule requirements
        else False

--step 4
-- if we talking bout a fact the GS list is empty therefore fullfilled query
intializeEval query (Fact x) = unify query x
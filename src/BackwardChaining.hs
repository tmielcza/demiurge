module BackwardChaining
(
  launchResolution
    ) where

import Types
import Inference

combineStates :: State -> State -> Either String State
combineStates (NotUnknown _) s2 = Right s2
combineStates s1 (NotUnknown _) = Right s1
combineStates (Unknown _) s2 = Right s2
combineStates s1 (Unknown _) = Right s1
combineStates Ambiguous s2 = Right s2
combineStates s1 Ambiguous = Right s1
combineStates s1 s2
    | s1 == s2 = Right s1
    | otherwise = Left ("Incoherent rules and/or initial facts")

combinePair :: Either String ([FactState], State) -> Either String ([FactState], State) -> Either String ([FactState], State)
combinePair p1 p2 = do
  (k1, s1) <- p1
  (k2, s2) <- p2
  fmap ((,) $ k1 ++ k2) $ combineStates s1 s2

-- | loop that check the coherence of results
resolveRules :: [Relation] -> [Relation] -> [FactState] ->  Either String ([FactState], State)
resolveRules  concernedRules rules knowledge =
  let
    evalGoal :: Relation -> Either String ([FactState], State)
    evalGoal (lhs `Imply` rhs) = ((specialCase rhs) `mapSnd`) <$> (eval knowledge rules lhs)
    evalGoal _ = error "Unreachable code"
  in
  (foldl combinePair (Right ([], (Unknown (Fact "")))) . map (evalGoal)) concernedRules


-- ATTENTION C DU LOURDS
 -- false
 -- a => !a
 -- b => !a

 -- true
 -- !a => a

 -- ambiguous
 -- !b => a


specialCase :: Expr -> State -> State
specialCase (Not rhs)  (Unknown expr)
  | expr == rhs =  (Known False) -- a => !a
  | otherwise =  Unknown expr
specialCase rhs  (NotUnknown expr)
  | expr == rhs =  (Known True) -- !a => a
  | otherwise =  Ambiguous -- !b => a
specialCase (Not rhs) (Known True) = (Known False)
specialCase rhs state = state

-- | Look for q fact in the knowledge or search it with the rules
resolveFact :: Expr -> [FactState] -> [Relation] -> Either String ([FactState], State)
resolveFact subgoal knowledge rules =
  case (lookup subgoal knowledge)  of
    Just st -> Right ([], st)
    Nothing  -> searchFact subgoal knowledge rules

-- | the function that evaluate an Expression
eval :: [FactState] -> [Relation] -> Expr -> Either String ([FactState], State)
eval knowledge rulesList expr =
  let shortEval = (eval knowledge rulesList) -- eval shortened
      applyOpe ope p1 p2 =
        do
          (k1, st1) <- p1
          (k2, st2) <- p2
          return ((k1 ++ k2), (st1 `ope` st2)) -- gather the pair concatening the left list and applying the ope on the right States
      applyNot p1 = do { (k, st) <- p1; return (k, (t_not st)) }
  in case expr of
      fact@(Fact _) -> (resolveFact fact knowledge rulesList)
      Not e -> applyNot (shortEval e)
      And e1 e2  -> applyOpe (@+) (shortEval e1) (shortEval e2)
      Or e1 e2  -> applyOpe (@|) (shortEval e1) (shortEval e2)
      Xor e1 e2  -> applyOpe (@^) (shortEval e1) (shortEval e2)


-- | Filter rules concerning the goal and resolve it
searchFact :: Expr -> [FactState] -> [Relation] -> Either String ([FactState], State)
searchFact goal knowledge rules =
  let
    concernedRules = inferRules rules goal
    searchKnown = (goal, Unknown goal):knowledge -- We set our goal at Unknown to avoid looping on it
    result = resolveRules concernedRules rules searchKnown
  in case result of
    Left err -> Left err
    Right (newknown, Unknown _) -> Right ((goal, Known False):newknown, Known False)
    Right (newknown, goalState) -> Right ((goal, goalState):newknown, goalState)

-- | function called with the result of file parsing to start resolution
launchResolution :: ([Relation], Init, Query) -> Either String [FactState]
launchResolution (rules, Init init, Query query) =
  let translateToState (Not fact) = (fact, Known False)
      translateToState fact = (fact, Known True)
      knowledge = map translateToState init
  in  loopOnQuery rules query knowledge

-- | loop the resolution on each query sent
loopOnQuery :: [Relation] -> [Expr] -> [FactState] -> Either String [FactState]
loopOnQuery rules (q:qs) knowledge = do
  let  ret = resolveFact q knowledge rules
  case ret of
    Right(newknowledge, result) -> (:)(q, result) <$> loopOnQuery rules qs (knowledge ++ newknowledge)
    Left err -> Left err
loopOnQuery _ [] _ = Right []

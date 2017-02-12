module BackwardChaining
(
  launchResolution
    ) where

import Types
import Inference
import Prelude hiding ( not)

combineStates :: State -> State -> Either String State
combineStates (Unsolved a) (Unsolved b) = Right (Unsolved a @| Unsolved b)
combineStates (Unsolved a) (Unprovable b) = Right (Unsolved a @| Unprovable b)
combineStates (Unprovable a) (Unsolved b) = Right (Unprovable a @| Unsolved b)
combineStates (Unprovable a) (Unprovable b) = Right (Unprovable a @| Unprovable b)

combineStates (Unsolved _) s2 = Right s2
combineStates s1 (Unsolved _) = Right s1
combineStates (Unprovable _) s2 = Right s2
combineStates s1 (Unprovable _) = Right s1
combineStates s1 s2
    | s1 == s2 = Right s1
    | otherwise = Left "Incoherent rules and/or initial facts"

combinePair :: Either String ([FactState], State) -> Either String ([FactState], State) -> Either String ([FactState], State)
combinePair p1 p2 = do
  (k1, s1) <- p1
  (k2, s2) <- p2
  ((,) $ k1 ++ k2) <$> combineStates s1 s2

-- | loop that check the coherence of results
resolveRules :: [Relation] -> [Relation] -> [FactState] ->  Either String ([FactState], State)
resolveRules [] _ knowledge = Right (knowledge, (snd . head) knowledge)
resolveRules concernedRules rules knowledge =
  let
    evalGoal :: Relation -> Either String ([FactState], State)
    evalGoal (lhs `Imply` rhs) = (specialCase rhs `mapSnd`) <$> eval knowledge rules lhs
    evalGoal _ = error "Unreachable code"
  in
  (foldl1 combinePair . map evalGoal) concernedRules


conjunctionContainsInverseExpr :: Expr -> Expr -> Bool
conjunctionContainsInverseExpr goal (lhs `And` rhs) =
  conjunctionContainsInverseExpr goal lhs || conjunctionContainsInverseExpr goal rhs
conjunctionContainsInverseExpr goal expr = goal == Not expr || Not goal == expr


specialCase :: Expr -> State -> State
specialCase (Not rhs)  (Unsolved expr)
  | conjunctionContainsInverseExpr (Not rhs) expr = Unsolved expr -- Q: When is it run ?
  | expr == rhs = Types.False -- a => !a
  | otherwise =  Unsolved expr
specialCase rhs  (Unsolved (Not expr))
  | expr == rhs = Types.True -- !a => a
  | otherwise =  Unprovable (Not expr) -- !b => a
specialCase rhs  (Unsolved expr)
  | conjunctionContainsInverseExpr rhs expr = Unprovable expr
specialCase (Not _) Types.True = Types.False
specialCase rhs Types.False = Unsolved rhs -- problem expr dans rhs
specialCase _ state = state

-- | Look for q fact in the knowledge or search it with the rules
resolveFact :: Expr -> [FactState] -> [Relation] -> Either String ([FactState], State)
resolveFact subgoal knowledge rules =
  case lookup subgoal knowledge of
    Just st -> Right ([], st)
    Nothing  -> searchFact subgoal knowledge rules

-- | the function that evaluate an Expression
eval :: [FactState] -> [Relation] -> Expr -> Either String ([FactState], State)
eval knowledge rulesList expr =
  let shortEval = eval knowledge rulesList -- eval shortened
      applyOpe ope p1 p2 =
        do
          (k1, st1) <- p1
          (k2, st2) <- p2
          return (k1 ++ k2, st1 `ope` st2) -- gather the pair concatening the left list and applying the ope on the right States
      applyNot p1 = do { (k, st) <- p1; return (k, not st) }
  in case expr of
      fact@(Fact _) -> resolveFact fact knowledge rulesList
      Not e -> applyNot (shortEval e)
      And e1 e2  -> applyOpe (@+) (shortEval e1) (shortEval e2)
      Or e1 e2  -> applyOpe (@|) (shortEval e1) (shortEval e2)
      Xor e1 e2  -> applyOpe (@^) (shortEval e1) (shortEval e2)


-- | Filter rules concerning the goal and resolve it
searchFact :: Expr -> [FactState] -> [Relation] -> Either String ([FactState], State)
searchFact goal knowledge rules =
  let
    concernedRules = inferRules rules goal
    searchKnown = (goal, Unsolved goal):knowledge -- We set our goal at Unsolved to avoid looping on it
    result = resolveRules concernedRules rules searchKnown
  in case result of
    Left err -> Left err
    Right (newknown, Unsolved _) -> Right ((goal, Types.False):newknown, Types.False)
    Right (newknown, goalState) -> Right ((goal, goalState):newknown, goalState)

-- | function called with the result of file parsing to start resolution
launchResolution :: ([Relation], Init, Query) -> Either String [FactState]
launchResolution (rules, Init init, Query query) =
  let translateToState (Not fact) = (fact, Types.False)
      translateToState fact = (fact, Types.True)
      knowledge = map translateToState init
  in  loopOnQuery rules query knowledge

-- | loop the resolution on each query sent
loopOnQuery :: [Relation] -> [Expr] -> [FactState] -> Either String [FactState]
loopOnQuery rules (query:queries) knowledge = do
      (newKnowledge, result) <- resolveFact query knowledge rules
      (:)(query, result) <$> loopOnQuery rules queries (knowledge ++ newKnowledge)
loopOnQuery _ [] _ = return []

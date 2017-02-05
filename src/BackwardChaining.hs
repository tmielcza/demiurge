module BackwardChaining
(
  launchResolution
    ) where

import Types
import Inference

-- | loop that check the coherence of results
resolveRules :: [Relation] -> [Relation] -> [FactState] ->  Maybe([FactState], State)
resolveRules  concernedRules rules knowledge =
  (foldl combinePair (Just ([], Unknown)) . map evalGoal) concernedRules
  where
    evalGoal (lhs `Imply` (Not _)) = (t_not `mapSnd`) <$> (eval knowledge rules lhs)
    evalGoal (lhs `Imply` _) = eval knowledge rules lhs
    evalGoal _ = error "Unreachable code"
    combinePair p1 p2 = do
      (k1, s1) <- p1
      (k2, s2) <- p2
      fmap ((,) $ k1 ++ k2) $ combineStates s1 s2
    combineStates Unknown s2 = Just s2
    combineStates s1 Unknown = Just s1
    combineStates s1 s2 | s1 == s2 = Just s1
                        | otherwise = Nothing

-- | Look for q fact in the knowledge or search it with the rules
resolveFact :: Expr -> [FactState] -> [Relation] -> Maybe([FactState], State)
resolveFact subgoal knowledge rules =
  case (lookup subgoal knowledge)  of
    Just st -> Just ([], st)
    Nothing  -> searchFact subgoal knowledge rules

-- | the function that evaluate an Expression
eval :: [FactState] -> [Relation] -> Expr -> Maybe ([FactState], State)
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
searchFact :: Expr -> [FactState] -> [Relation] -> Maybe ([FactState], State)
searchFact goal knowledge rules =
  let
    concernedRules = inferRules rules goal
    searchKnown = (goal, Unknown):knowledge -- We set our goal at Unknown to avoid looping on it
    result = resolveRules concernedRules rules searchKnown
  in case result of
    Nothing -> Nothing
    Just (newknown, Unknown) -> Just ((goal, Types.False):newknown, Types.False)
    Just (newknown, goalState) -> Just ((goal, goalState):newknown, goalState)

-- | function called withe the result of file parsing to start resolution
launchResolution :: ([Relation], Init, Query) -> Maybe [FactState]
launchResolution (rules, Init init, Query query) =
  let translateToState (Not fact) = (fact, Types.False)
      translateToState fact = (fact, Types.True)
      knowledge = map translateToState init
  in  loopOnQuery rules query knowledge

-- | loop the resolution on each query sent
loopOnQuery :: [Relation] -> [Expr] -> [FactState] -> Maybe [FactState]
loopOnQuery rules (q:qs) knowledge = do
  let  ret = resolveFact q knowledge rules
  case ret of
    Just(newknowledge, result) -> (:)(q, result) <$> loopOnQuery rules qs (knowledge ++ newknowledge)
    Nothing -> Nothing


loopOnQuery _ [] _ = Just []

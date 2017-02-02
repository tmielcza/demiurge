module BackwardChaining
(
  launchResolution
    ) where

import Types

-- | loop that check the coherence of results
resolveRules :: [Relation] -> [Relation] -> [FactState] ->  Maybe([FactState], State)
resolveRules  concernedRules rules knowledge =
  (foldl combinePair (Just ([], Unknown)) . map ((eval knowledge rules) . lhs) ) concernedRules
  where
        combinePair maybe maybe' = do { (k, s) <- maybe;(k', s') <- maybe'; fmap ((,) $ k ++ k') $ combineStates s s'}
        combineStates Unknown s' = Just s'
        combineStates s Unknown = Just s
        combineStates s s' | s == s' = Just s
                           | otherwise = Nothing

-- | Look for q fact in the knowledge or search it with the rules
resolveFact :: Expr -> [FactState] -> [Relation] -> Maybe([FactState], State)
resolveFact subgoal knowledge rules =
  case (lookup subgoal knowledge)  of
    Just st -> Just ([], st)
    Nothing  -> searchFact subgoal knowledge rules

eval :: [FactState] -> [Relation] -> Expr -> Maybe ([FactState], State)
eval knowledge rulesList expr =
  let shortEval = (eval knowledge rulesList) -- eval shortened
      applyOpe ope p1 p2 =
        do
          (k1, st1) <- p1
          (k2, st2) <- p1
          return ((k1 ++ k2), (st1 `ope` st2)) -- gather the pair concatening the left list and applying the ope on the right States
      applyNot p1 = do { (k, st) <- p1; return (k, (t_not st)) }
  in case expr of
      fact@(Fact _) -> (resolveFact fact knowledge rulesList)
      Not e -> applyNot (shortEval e)
      And e1 e2  -> applyOpe (@+) (shortEval e1) (shortEval e2)
      Or e1 e2  -> applyOpe (@|) (shortEval e1) (shortEval e2)
      Xor e1 e2  -> applyOpe (@^) (shortEval e1) (shortEval e2)

rhs (Eq _ r) = r
rhs (Imply _ r) = r

lhs (Eq l _) = l
lhs (Imply l _) = l


-- | Filter rules concerning the goal and resolve it
searchFact :: Expr -> [FactState] -> [Relation] -> Maybe ([FactState], State)
searchFact goal knowledge rules =
  let
    concernedRules = filter (\r -> (rhs r == goal) || (rhs r == Not(goal))) rules
    searchKnown = (goal, Unknown):knowledge -- We set our goal at Unknown to avoid looping on it
    result = resolveRules concernedRules rules searchKnown
  in case result of
    Nothing -> Nothing
    Just (newknown, Unknown) -> Just ((goal, Types.False):newknown, Types.False)
    Just (newknown, goalState) -> Just ((goal, goalState):newknown, goalState)

--searchEquivalentRule :: [Relation] -> [Relation]

launchResolution :: (Either String ([Relation], Init, Query)) -> IO ()
launchResolution (Right (rules, Init init, Query query)) =
  let translateToState (Not fact) = (fact, Types.False)
      translateToState fact = (fact, Types.True)
      knowledge = map translateToState init
  in  loopOnQuery rules query knowledge

launchResolution (Left err) = print err

loopOnQuery :: [Relation] -> [Expr] -> [FactState] -> IO ()
loopOnQuery rules (q:qs) knowledge = do
  let  ret = resolveFact q knowledge rules
  case ret of
    Just(newknowledge, result) -> print (" " ++ (show q) ++ ":" ++ (show result))--; loopOnQuery rules qs (knowledge ++ newknowledge)
    Nothing -> print ("Ambiguous case or Incoherent rules about the fact " ++ (show q))


loopOnQuery _ [] _ = return ()

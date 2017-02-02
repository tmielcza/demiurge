import Types

-- | loop that check the coherence of results
resolveRules :: [Relation] -> [Relation] -> [FactState] -> ([FactState], State)
resolveRules  (rule:concernedRules) rules knowledge =
  let Imply lhs _ = rule
      (newknown, ret) = eval knowledge rules lhs
      (nextKnown, nextRet) = resolveRules concernedRules rules knowledge
  in case ret of
    Unknown -> (newknown ++ nextKnown, nextRet)
    nextRet -> (newknown ++ nextKnown, ret)
    otherwise -> Unknown


resolveRules [] _ _ = ([], Types.False)

-- | Look for q fact in the knowledge or search it with the rules
resolveFact :: Expr -> [FactState] -> [Relation] -> ([FactState], State)
resolveFact subgoal knowledge rules =
  case (lookup subgoal knowledge)  of
    Just st -> ([], st)
    Nothing  -> searchFact subgoal knowledge rules

eval :: [FactState] -> [Relation] -> Expr -> ([FactState], State)
eval knowledge rulesList expr =
  let shortEval = (eval knowledge rulesList) -- eval shortened
      applyOpe ope (k1, st1) (k2, st2) = ((k1 ++ k2), (st1 `ope` st2)) -- gather the pair concatening the left list and applying the ope on the right States
      applyNot (k, st) = (k, (t_not st))
  in case expr of
      fact@(Fact _) -> (resolveFact fact knowledge rulesList)
      Not e -> applyNot (shortEval e)
      And e1 e2  -> applyOpe (@+) (shortEval e1) (shortEval e2)
      Or e1 e2  -> applyOpe (@|) (shortEval e1) (shortEval e2)
      Xor e1 e2  -> applyOpe (@^) (shortEval e1) (shortEval e2)

rhs (Eq _ r) = r
rhs (Imply _ r) = r

-- | Filter rules concerning the goal and resolve it
searchFact :: Expr -> [FactState] -> [Relation] -> ([FactState], State)
searchFact goal knowledge rules =
  let
    concernedRules = filter (\r -> rhs r == goal) rules
    searchKnown = (goal, Unknown):knowledge -- We set our goal at Unknown to avoid looping on it
    (newknown, goalState) = resolveRules concernedRules rules searchKnown
  in case goalState of
    Unknown -> ((goal, Types.False):newknown, goalState)
    otherwise -> ((goal, goalState):newknown, goalState)

--searchEquivalentRule :: [Relation] -> [Relation]

launchResolution :: ([Relation], Init, Query) -> IO ()
launchResolution (rules, Init init, Query query) =
  let translateToState (Not fact) = (fact, Types.False)
      translateToState fact = (fact, Types.True)
      knowledge = map translateToState init
  in  loopOnQuery rules query knowledge

loopOnQuery :: [Relation] -> [Expr] -> [FactState] -> IO ()
loopOnQuery rules (q:qs) knowledge = do
  let  (newknowledge, result) = resolveFact q knowledge rules
  print (" " ++ (show q) ++ ":" ++ (show result))
  loopOnQuery rules qs (knowledge ++ newknowledge)

loopOnQuery _ [] _ = return ()

module BackwardChaining
(
  resolveQueries
    ) where

import Types
import Inference
import Prelude hiding ( not)
import Data.Foldable

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

combinePair :: Either String Resolved -> Either String Resolved -> Either String Resolved
combinePair p1 p2 = do
  Resolved (k1, s1) <- p1
  Resolved (k2, s2) <- p2
  (Resolved . ((,) $ k1 ++ k2)) <$> combineStates s1 s2

-- | loop that check the coherence of results
resolveRules :: [Relation] -> [Relation] -> [FactState] ->  Either String Resolved
resolveRules [] _ knowledge = Right (Resolved (knowledge, (snd . head) knowledge))
resolveRules concernedRules rules knowledge =
  let
    mapResolvedState f (Resolved (a, b)) = Resolved (a, f b)
    evalGoal :: Relation -> Either String Resolved
    evalGoal (lhs `Imply` rhs) = (specialCase rhs `mapResolvedState`) <$> eval rules knowledge lhs
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
resolveFact :: [Relation] -> [FactState] -> Expr -> Either String Resolved
resolveFact rules knowledge subgoal =
  case lookup subgoal knowledge of
    Just st -> Right (Resolved (knowledge, st))
    Nothing -> searchFact rules knowledge subgoal

-- | the function that evaluate an Expression
eval :: [Relation] -> [FactState] -> Expr -> Either String Resolved
eval rulesList knowledge expr = do
  foldExprM (resolveFact rulesList knowledge) expr


-- | Filter rules concerning the goal and resolve it
searchFact :: [Relation] -> [FactState] -> Expr -> Either String Resolved
searchFact rules knowledge goal =
  let
    concernedRules = inferRules rules goal
    searchKnown = (goal, Unsolved goal):knowledge -- We set our goal at Unsolved to avoid looping on it
  in do
    r <- resolveRules concernedRules rules searchKnown
    case r of
      Resolved (newknown, Unsolved _) -> return (Resolved ((goal, Types.False):newknown, Types.False))
      Resolved (newknown, goalState) -> return (Resolved ((goal, goalState):newknown, goalState))


-- | loop the resolution on each query sent
resolveQueries :: ([Relation], Init, Query) -> Either String [FactState]
resolveQueries (rules, knowledge, queries) =
  foldlM (\k e -> fmap resolvedKnowledge (resolveFact rules k e)) knowledge queries

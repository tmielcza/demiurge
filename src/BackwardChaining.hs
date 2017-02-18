module BackwardChaining
(
  getStateOfQueries,
  resolveFact,
  evalGoal,
  resolveRules,
  evalImplication
    ) where

import Types
import Inference
import Prelude hiding ( not)
import Data.Foldable
import Debug.Trace


-- | loop that check the coherence of results
resolveRules :: [Relation] -> [FactState] -> Expr -> Either String Resolved
resolveRules rules knowledge goal =
  let
    concernedRules = inferRules rules goal
    evalRule :: Resolved -> Relation -> Either String Resolved
    evalRule ((knowledge', state)) relation = do
        (knowledge'', state') <- eval rules knowledge' relation
        return (knowledge'', state @| state')
  in
    foldlM evalRule (((goal, Unsolved goal):knowledge, Unsolved goal)) concernedRules


conjunctionContainsInverseExpr :: Expr -> Expr -> Bool
conjunctionContainsInverseExpr goal (lhs `And` rhs) =
  conjunctionContainsInverseExpr goal lhs || conjunctionContainsInverseExpr goal rhs
conjunctionContainsInverseExpr goal expr = goal == Not expr || Not goal == expr

evalImplication :: Expr -> State -> State
evalImplication (Not rhs)  (Unsolved expr)
  | conjunctionContainsInverseExpr (Not rhs) expr = Unsolved expr -- Q: When is it run ?
  | expr == rhs = Types.True -- a => !a
  | otherwise =  Unsolved expr
evalImplication rhs  (Unsolved (Not expr))
  | expr == rhs = Types.True -- !a => a
  | otherwise =  Unprovable (Not expr) -- !b => a
evalImplication rhs  (Unsolved expr)
  | conjunctionContainsInverseExpr rhs expr = Unprovable expr
evalImplication (Not _) Types.True = Types.True
evalImplication (Not rhs) Types.False = Unsolved rhs
evalImplication rhs Types.False = Unsolved rhs -- problem expr dans rhs
evalImplication _ state = state

-- | Look for q fact in the knowledge or search it with the rules
resolveFact :: [Relation] -> [FactState] -> Expr -> Either String Resolved
resolveFact rules knowledge subgoal =
  case lookup subgoal knowledge of
    Just st -> Right (knowledge, st)
    Nothing -> evalGoal rules knowledge subgoal


-- | the function that evaluate an Expression
eval :: [Relation] -> [FactState] -> Relation -> Either String Resolved
eval rulesList knowledge (lhs `Imply` rhs) = do
  (k, s) <- foldExprM (resolveFact rulesList knowledge) lhs
  return (k, evalImplication rhs s)
eval _ _ _ = error "Unreachable Code"

evalGoal :: [Relation] -> [FactState] -> Expr -> Either String Resolved
evalGoal rules knowledge goal =
  let
    combineGoalAndOposite Types.True Types.True = Left "Incoherent rules and/or initial facts"
    combineGoalAndOposite _ Types.True = Right Types.False
    combineGoalAndOposite _ (Unprovable u) = Right (Unprovable u)
    combineGoalAndOposite (Unsolved _) _ = Right Types.False
    combineGoalAndOposite goal _oposite = Right goal
  in do
    (k, goalState) <- resolveRules rules knowledge goal
    (finalKnowledge, opositeState) <- resolveRules rules k (Not goal)
    resultState <- goalState `combineGoalAndOposite` opositeState
    return ((goal, resultState):finalKnowledge, resultState)



-- | Filter rules concerning the goal and resolve it
searchFact :: [Relation] -> [FactState] -> Expr -> Either String Resolved
searchFact rules knowledge goal = do
  r <- evalGoal rules knowledge goal
  case r of
    (newknown, Unsolved _) -> return ((goal, Types.False):newknown, Types.False)
    (newknown, goalState) -> return ((goal, goalState):newknown, goalState)

getStateOfQueries :: ([Relation], Init, Query) -> Either String [FactState]
getStateOfQueries triple@(_, _, queries) = do
  collectedKnowledge <- resolveQueries triple
  return (filter (\(fact, state) -> elem fact queries) collectedKnowledge)

-- | loop the resolution on each query sent
resolveQueries :: ([Relation], Init, Query) -> Either String [FactState]
resolveQueries (rules, knowledge, queries) =
  foldlM (\k e -> fmap fst (resolveFact rules k e)) knowledge queries

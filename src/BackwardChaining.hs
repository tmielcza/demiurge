module BackwardChaining (
  resolve
  ) where

import Control.Monad.Trans.State.Lazy as S (
  get,
  modify,
  runState
  )

import Control.Monad.Trans.Reader (
  ask,
  runReaderT
  )

import Control.Monad.Except (
  throwError,
  runExceptT
  )

import Control.Monad.Trans.Class (lift)

import Types as T

import Inference

import Logic


resolveRules :: Expr -> Resolution T.State
resolveRules goal = do
  (lift . lift . modify) ((goal, Unsolved goal):)
  rules <- lift ask
  let concernedRules = inferRules rules goal
  let evalRule state relation = do
        s <- state
        s' <- eval relation
        return (s @| s')
  foldl evalRule (return (Unsolved goal)) concernedRules

eval :: Relation -> Resolution T.State
eval (lhs `Imply` rhs) = do
  s <- evalExpr resolveFact lhs
  return (evalImplication rhs s)
eval _ = error "Unreachable Code"


evalGoal :: Expr -> Resolution T.State
evalGoal goal = do
    s <- resolveRules goal
    ns <- resolveRules (Not goal)
    let resultState = s `combineGoalAndOposite` ns
    either (throwError) (\s -> do {lift $ lift $ modify ((goal, s):) ; return s}) resultState


resolveFact :: Expr -> Resolution T.State
resolveFact fact = do
  knowledge <- lift $ lift $ get
  maybe (evalGoal fact) (return) (lookup fact knowledge)


getStateOfQueries :: [Expr] -> Resolution [FactState]
getStateOfQueries queries = do
  mapM_ resolveFact queries
  collectedKnowledge <- lift $ lift get
  return $ filter (\(fact, _) -> elem fact queries) (collectedKnowledge)

resolve :: ([Relation], [FactState], [Expr]) -> Either String [FactState]
resolve (rules, knowledge, queries) =
  fst $ runState (runReaderT (runExceptT (getStateOfQueries queries)) rules) knowledge

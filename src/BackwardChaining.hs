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

import Prelude hiding(lookup, filter)

import Data.Map(insert, lookup, toList, fromList)

resolveRules :: Expr -> Resolution T.State
resolveRules goal = do
  rules <- getRules
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
evalGoal goal@(Fact c) = do
    modifyKnowledge (insert c (Unsolved goal))
    s <- resolveRules goal
    ns <- resolveRules (Not goal)
    let resultState = s `combineGoalAndOposite` ns
    either (throwError) (\s -> do {modifyKnowledge (insert c s) ; return s}) resultState


resolveFact :: Expr -> Resolution T.State
resolveFact fact@(Fact c) = do
  knowledge <- getKnowledge
  maybe (evalGoal fact) (return) (lookup c knowledge)


getStateOfQueries :: [Expr] -> Resolution [(String, State)]
getStateOfQueries queries = do
  mapM_ resolveFact queries
  knowledge <- getKnowledge
  return $ [ x | x@(f, s) <- toList knowledge, elem (Fact f) queries]
  --filterWithKey (\fact _ -> elem fact queries) (collectedKnowledge)

resolve :: ([Relation], [(String, State)], [Expr]) -> Either String [(String, State)]
resolve (rules, init, queries) =
  let knowledge = fromList init in
  fst $ runState (runReaderT (runExceptT (getStateOfQueries queries)) rules) knowledge

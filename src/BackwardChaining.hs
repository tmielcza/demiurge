module BackwardChaining (
  resolve
  ) where

import Control.Monad.State.Class (
  get,
  modify
  )

import Control.Monad.Trans.State.Lazy as S (
  runState
  )

import Control.Monad.Reader.Class (
  ask
  )

import Control.Monad.Trans.Reader (
  runReaderT
  )

import Control.Monad.Trans.Writer (
  runWriterT
  )

import Control.Monad.Writer.Class (
  tell
  )

import Control.Monad.Except (
  throwError,
  runExceptT
  )

import ReasoningVisualisation

import Control.Monad.Trans.Class (lift)

import Types as T

import Inference

import Logic

import Prelude hiding(lookup, filter)

import Data.Map(insert, lookup, toList, fromList)

resolveRules :: Expr -> Resolution (T.State, Proof)
resolveRules goal = do
  rules <- ask
  let concernedRules = inferRules rules goal
  let evalRule resolution (ruleStack, relation) = do
        (s, RuleProof log) <- resolution
        s'<- eval relation
        let log' = case s' of Unsolved _ -> log
                              _ -> ruleStack
        let news = s @| s'
        case log of
          [] -> return (s', RuleProof ruleStack)
          _ -> return (news, RuleProof log')
  foldl evalRule (return (Unsolved goal, RuleProof [])) concernedRules

eval :: Relation -> Resolution T.State
eval (lhs `Imply` rhs) = do
  s <- evalExpr resolveFact lhs
  return (evalImplication rhs s)
eval _ = error "Unreachable Code"

evalGoal :: Expr -> Resolution T.State
evalGoal goal@(Fact c) = do
    modify (insert c (Unsolved goal, RuleProof []))
    s <- resolveRules goal
    ns <- resolveRules (Not goal)
    knowledge <- get
    let resultState = s `combineGoalAndOposite` ns
    either (throwError . runShowProof knowledge goal) (\(s, p) -> do{modify (insert c (s, p)); return s}) resultState

resolveFact :: Expr -> Resolution T.State
resolveFact fact@(Fact c) = do
  knowledge <- get
  maybe (evalGoal fact) (\(st, _pr) -> return st) (lookup c knowledge)

getStateOfQueries :: [Expr] -> Resolution Knowledge
getStateOfQueries queries = do
    mapM_ (resolveFact) queries
    collectedKnowledges <- get
    return collectedKnowledges

resolve :: ([Relation], [(String, State)], [Expr]) -> Either String Knowledge
resolve (rules, init, queries) =
  let knowledge = fromList $ map (\(k, st) -> (k, (st, Known st))) init
      results = fst $ runState (runReaderT (runExceptT (getStateOfQueries queries)) rules) knowledge
  in results

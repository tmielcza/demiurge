module BackwardChaining (
  resolve2 {-, resolve-}
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
  --evalRule :: Resolution (T.State, Proof) -> ([Relation], Relation) -> Resolution (T.State, Proof)
  let evalRule {-(state, RuleProof log)-} resolution (ruleStack, relation) = do
        (s, RuleProof log) <- resolution
        s'<- eval relation
        let log' = if (s' == T.True || s' == T.False) then log ++ ruleStack else log
        let news = s @| s'
        return (news, RuleProof log')
        --return ((s @| s'), RuleProof log')
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
    either (throwError . showProof knowledge goal) (\(s, p) -> do{modify (insert c (s, p)); return s}) resultState

resolveFact :: Expr -> Resolution T.State
resolveFact fact@(Fact c) = do
  knowledge <- get
  maybe (evalGoal fact) (\(st, _pr) -> return st) (lookup c knowledge)


getStateOfQueries :: [Expr] -> Resolution [(String, (T.State, Proof))]
getStateOfQueries queries = do
  knowledge <- get
  mapM_ (resolveFact) queries
  return $ toList knowledge
  --filterWithKey (\fact _ -> elem fact queries) (collectedKnowledge)



{-resolve :: ([Relation], [(String, State)], [Expr]) -> Either String [(String, State)]
resolve (rules, init, queries) =
  let knowledge = fromList init
      results = fst $ runState (runReaderT (runExceptT (runWriterT (getStateOfQueries queries))) rules) knowledge
  in
  fmap fst results-}
  --trace (show $ fmap snd results) (fmap fst results)

resolve2 :: ([Relation], [(String, State)], [Expr]) -> Either String ([(String, (T.State, Proof) )], String)
resolve2 (rules, init, queries) =
  let knowledge = fromList $ map (\(k, st) -> (k, (st, Known st))) init
      results = fst $ runState (runReaderT (runExceptT (getStateOfQueries queries)) rules) knowledge
  in
  results

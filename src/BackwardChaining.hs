module BackwardChaining (
  resolve, resolve2
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

import Control.Monad.Trans.Class (lift)

import Types as T

import Inference

import Logic

import Prelude hiding(lookup, filter)

import Data.Map(insert, lookup, toList, fromList)


resolveRules :: Expr -> Resolution T.State
resolveRules goal = do
  rules <- ask
  let concernedRules = inferRules rules goal
  let evalRule (state, RuleProof log) (stack, relation) = do
        s <- state
        s' <- eval relation
        let log' = if (s' == T.True || s' == T.False) then log ++ stack else log
        return ((s @| s'), RuleProof log)
  foldl evalRule (return (Unsolved goal, RuleProof [])) concernedRules


eval :: Relation -> Resolution T.State
eval (lhs `Imply` rhs) = do
  s <- evalExpr resolveFact lhs
  return (evalImplication rhs s)
eval _ = error "Unreachable Code"


evalGoal :: Expr -> Resolution T.State
evalGoal goal@(Fact c) = do
    modify (insert c (Unsolved goal))
    tell "\n"
    s <- resolveRules goal
    ns <- resolveRules (Not goal)
    let resultState = s `combineGoalAndOposite` ns
    either (throwError) (\s -> do {tell (c ++ " is " ++ show s ++ "\n") ; modify (insert c s) ; return s}) resultState


resolveFact :: Expr -> Resolution T.State
resolveFact fact@(Fact c) = do
  tell ("searching " ++ c ++ " ")
  knowledge <- get
  maybe (evalGoal fact) (\x -> do{tell (show x ++ "\n"); return x}) ( lookup c knowledge)


getStateOfQueries :: [Expr] -> Resolution [(String, State)]
getStateOfQueries queries = do
  mapM_ resolveFact queries
  knowledge <- get
  return $ [ x | x@(f, s) <- toList knowledge, elem (Fact f) queries]
  --filterWithKey (\fact _ -> elem fact queries) (collectedKnowledge)

resolve :: ([Relation], [(String, State)], [Expr]) -> Either String [(String, State)]
resolve (rules, init, queries) =
  let knowledge = fromList init
      results = fst $ runState (runReaderT (runExceptT (runWriterT (getStateOfQueries queries))) rules) knowledge
  in
  trace (show $ fmap snd results) (fmap fst results)

resolve2 :: ([Relation], [(String, State)], [Expr]) -> Either String ([(String, State)], String)
resolve2 (rules, init, queries) =
  let knowledge = fromList init
      results = fst $ runState (runReaderT (runExceptT (runWriterT (getStateOfQueries queries))) rules) knowledge
  in
  results

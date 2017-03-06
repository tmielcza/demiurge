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


resolveRules :: Expr -> Resolution (T.State, Proof)
resolveRules goal = do
  rules <- ask
  let concernedRules = inferRules rules goal
  let evalRule (state, RuleProof log) (_, relation) = do
        s <- state
        (s', RuleProof stack) <- eval relation
        let log' = if (s' == T.True || s' == T.False) then log ++ stack else log
        return ((s @| s'), RuleProof log')
  foldl evalRule (return (Unsolved goal, RuleProof [])) concernedRules


eval :: Relation -> Resolution (T.State, Proof)
eval (lhs `Imply` rhs) = do
  s <- evalExpr resolveFact lhs
  return (evalImplication rhs s)
eval _ = error "Unreachable Code"


evalGoal :: Expr -> Resolution (T.State, Proof)
evalGoal goal@(Fact c) = do
    modify (insert c (Unsolved goal))
    tell "\n"
    s <- resolveRules goal
    ns <- resolveRules (Not goal)
    let resultState = s `combineGoalAndOposite` ns
    either (throwError) (\s -> do {tell (c ++ " is " ++ show s ++ "\n") ; modify (insert c s) ; return s}) resultState


resolveFact :: Expr -> Resolution (T.State, Proof)
resolveFact fact@(Fact c) = do
  tell ("searching " ++ c ++ " ")
  knowledge <- get
  maybe (evalGoal fact) (\x -> return (x, Known x)) (lookup c knowledge)


getStateOfQueries :: [Expr] -> Resolution [(String, (T.State, Proof))]
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
  fmap fst results
  --trace (show $ fmap snd results) (fmap fst results)

resolve2 :: ([Relation], [(String, State)], [Expr]) -> Either String [(String, (T.State, Proof) )]
resolve2 (rules, init, queries) =
  let knowledge = fromList init
      results = fst $ runState (runReaderT (runExceptT (runWriterT (getStateOfQueries queries))) rules) knowledge
  in
  results

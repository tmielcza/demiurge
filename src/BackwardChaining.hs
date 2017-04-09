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

import Logic(combineGoalAndOposite, evalImplication, evalExpr, Logical(..))

import Debug.Trace

import Prelude hiding(lookup, filter)

import Data.Map(insert, lookup, toList, fromList, foldWithKey, map)

resolveRules :: Expr -> Resolution (T.State, Proof)
resolveRules goal = do
  rules <- ask
  let concernedRules = inferRules rules goal
  let evalRule resolution (ruleStack, relation) = do
        (s, RuleProof log) <- resolution
        ret <- eval relation
        case ret of
          (s'@(Unsolved _), _) -> if (null log) then return (s', RuleProof ruleStack) else return (s, RuleProof log)
          (s', Prelude.True) -> return (s', Contradiction ruleStack)
          (s', Prelude.False) -> if (null log) then return (s', RuleProof ruleStack) else return (s'@| s, RuleProof ruleStack)
  foldl evalRule (return (Unsolved goal, RuleProof [])) concernedRules

eval :: Relation -> Resolution (T.State, Bool)
eval (lhs `Imply` rhs) = do
  s <- evalExpr resolveFact lhs
  evalImplication rhs s
eval _ = error "Unreachable Code"

evalGoal :: Expr -> Resolution T.State
evalGoal goal@(Fact c) = do
    modify (insert c (Unsolved goal, RuleProof []))
    s <- resolveRules goal
    ns <- resolveRules (Not goal)
    knowledge <- get
    let resultState = s `combineGoalAndOposite` ns
    either (\pr -> throwError (knowledge, goal, pr)) (\(s, p) -> do{modify (insert c (s, p)); return s}) resultState

resolveFact :: Expr -> Resolution T.State
resolveFact fact@(Fact c) = do
  knowledge <- get
  maybe (evalGoal fact) (\(st, _pr) -> return st) (lookup c knowledge)

getStateOfQueries :: [Expr] -> Resolution Knowledge
getStateOfQueries queries =
  let
  lastResolution k (e, p) prev defaultValue = do
    s <- evalExpr resolveFact e
    case s of
      Unsolved _ -> modify (insert k (defaultValue, p))
      _ -> modify (insert k (s, p))
    p <- prev
    return (k ++ p)
  unsolvedToFalse k (Unsolved e , p) prev = lastResolution k (e, RuleProof []) prev T.False
  unsolvedToFalse k (Unprovable e , p) prev = lastResolution k (e, p) prev (Unprovable e)
  unsolvedToFalse k pr prev = do { p <- prev ;return (k ++ p)}
  in do
    mapM_ (resolveFact) queries
    collectedKnowledges <- get
    Data.Map.foldWithKey unsolvedToFalse (return "") collectedKnowledges
    newKnowledge <- get
    return newKnowledge


resolve :: Bool -> ([Relation], [(String, State)], [Expr]) -> Either String Knowledge
resolve isVerbose (rules, init, queries) =
  let knowledge = fromList $ Prelude.map (\(k, st) -> (k, (st, Known st))) init
      results = fst $ runState (runReaderT (runExceptT (getStateOfQueries queries)) rules) knowledge
  in case results of
    Left (k, g, p) -> Left (runShowInvalid k g p isVerbose)
    Right r -> Right r

import Data.Tuple

import Control.Monad.Trans.State.Lazy as S (
  State,
  state,
  execState,
  runState,
  get,
  put,
  modify
  )

import Control.Monad.Trans.Reader (
  ReaderT(ReaderT),
  runReaderT,
  ask
  )

import Control.Monad.Except (
  ExceptT,
  runExceptT,
  throwError
  )

import Control.Monad.Trans.Class (lift)

import Types as T

import BackwardChaining (resolveFact, evalGoal, resolveRules)

type Resolution =  ExceptT String (ReaderT [Relation] (S.State [FactState])) T.State

resolveRules' :: Expr -> Resolution
resolveRules' f = do
  rules <- lift $ ask
  knowledge <- lift $ lift $ get
  let e = resolveRules rules knowledge f
  either throwError (\(k, s) -> do
                        lift $ lift $ put k
                        return s) e

evalGoal' goal =
  let
    combineGoalAndOposite T.True T.True = Left "Incoherent rules and/or initial facts"
    combineGoalAndOposite _ T.True = Right T.False
    combineGoalAndOposite _ (Unprovable u) = Right (Unprovable u)
    combineGoalAndOposite (Unsolved _) _ = Right T.False
    combineGoalAndOposite goal _oposite = Right goal
  in do
    s <- resolveRules' goal
    ns <- resolveRules' (Not goal)
    let resultState = s `combineGoalAndOposite` ns
    either (throwError) (\s -> do {lift $ lift $ modify ((goal, s):) ; return s}) resultState
  

resolveFact' :: Expr -> Resolution
resolveFact' fact = do
  knowledge <- lift $ lift $ get
  maybe (evalGoal' fact) (return) (lookup fact knowledge)

evalExpr' :: Expr -> Resolution

evalExpr' (lhs `Xor` rhs) = do
  l <- evalExpr' lhs
  r <- evalExpr' rhs
  return (l @^ r)

evalExpr' (lhs `Or` rhs) = do
  l <- evalExpr' lhs
  r <- evalExpr' rhs
  return (l @| r)

evalExpr' (lhs `And` rhs) = do
  l <- evalExpr' lhs
  r <- evalExpr' rhs
  return (l @+ r)

evalExpr' (Not e) = do {s <- evalExpr' e; return (T.not s)}

evalExpr' (Fact fact) = resolveFact' (Fact fact)


main = do
  let rules = [(Fact "A" `Xor` Fact "B" `Xor` Fact "C" `Xor` Fact "D") `Imply` (Fact "E")]
      expr = (Fact "A" `Xor` Fact "B" `Xor` Fact "C")
    in
    (print . show . runState (runReaderT (runExceptT (evalExpr' expr)) rules) ) [(Fact "A", T.True)]

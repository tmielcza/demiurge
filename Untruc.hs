import Data.Tuple

import Control.Monad.Trans.State.Lazy as S (
  State,
  state,
  execState,
  runState,
  get,
  put
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

import BackwardChaining (resolveFact)

type Resolution =  ExceptT String (ReaderT [Relation] (S.State [FactState])) T.State

resolveFact' :: Expr -> Resolution
resolveFact' fact = do
  rules <- lift $ ask
  knowledge <- lift $ lift $ get
  let e = resolveFact rules knowledge fact
  either throwError (\(k, s) -> do
                        lift $ lift $ put k
                        return s) e


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

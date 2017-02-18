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

fromRight (Right a) = a

resolveFact' :: [Relation] -> [FactState] -> Expr -> Either String Resolved
resolveFact' = resolveFact


evalExpr' :: Expr -> Resolution

evalExpr' (Fact fact) = do
  rules <- lift $ ask
  knowledge <- lift $ lift $ get
  let e = resolveFact' rules knowledge (Fact fact)
  either throwError (\(k, s) -> do
                        lift $ lift $ put k
                        return s) e

evalExpr' (lhs `Xor` rhs) = do
  l <- evalExpr' lhs
  r <- evalExpr' rhs
  return (l @^ r)



main = do
  let rules = [(Fact "A" `Xor` Fact "B" `Xor` Fact "C" `Xor` Fact "D") `Imply` (Fact "E")]
      expr = (Fact "A" `Xor` Fact "B" `Xor` Fact "C")
    in
    (print . show . runState (runReaderT (runExceptT (evalExpr' expr)) rules) ) [(Fact "A", T.True)]

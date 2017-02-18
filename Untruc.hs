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

import Control.Monad.Trans.Class (lift)

import Types as T

import BackwardChaining (resolveFact)

type Resolution =  ReaderT [Relation] (S.State [FactState]) T.State

fromRight (Right a) = a

resolveFact' :: [Relation] -> [FactState] -> Expr -> Resolved
resolveFact' r k = fromRight . resolveFact r k


evalExpr' :: Expr -> Resolution

evalExpr' (Fact fact) = do
  rules <- ask
  knowledge <- lift $ get
  let (k, s) = resolveFact' rules knowledge (Fact fact)
  lift $ put k
  return s

evalExpr' (lhs `Xor` rhs) = do
  l <- evalExpr' lhs
  r <- evalExpr' rhs
  return (l @^ r)



main = do
  let rules = [Fact "Q" `Imply` Fact "A"]
      expr = (Fact "Q" `Xor` ((Fact "A") `Xor` (Fact "B")))
    in
    (print . show . runState (runReaderT (evalExpr' expr) rules) ) [(Fact "Q", T.True)]

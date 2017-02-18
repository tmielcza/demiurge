import Data.Tuple

import Control.Monad.Trans.State.Lazy as S (
  State,
  state,
  execState,
  runState
  )

import Control.Monad.Trans.Reader (
  ReaderT(ReaderT),
  runReaderT
  )

import Control.Monad.Trans.Class (lift)

import Types as T

import BackwardChaining (resolveFact)

type Resolution =  ReaderT [Relation] (S.State [FactState]) T.State

fromRight (Right a) = a


resolveFact' :: [Relation] -> [FactState] -> Expr -> Resolved
resolveFact' r k = fromRight . resolveFact r k

evalFact :: Expr -> Resolution

evalFact fact = ReaderT (\rules ->
                           state (\s ->
                                    swap (resolveFact' rules s fact)))


evalExpr' :: Expr -> Resolution

evalExpr' (Fact fact) = evalFact (Fact fact)

evalExpr' (lhs `Xor` rhs) = do
  l <- evalExpr' lhs
  r <- evalExpr' rhs
  return (l @| r)



main = do
  let rules = [Fact "Q" `Imply` Fact "A"]
      expr = (Fact "Q" `Xor` ((Fact "A") `Xor` (Fact "B")))
    in
    (print . show . runState (runReaderT (evalExpr' expr) rules) ) [(Fact "Q", T.True)]

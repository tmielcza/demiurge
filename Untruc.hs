import Data.Tuple

import Control.Monad.Trans.State.Lazy as S (
  State,
  state,
  execState,
  runState
  )

import Types as T


resolveFact :: [Char] -> [FactState] -> Expr -> Resolved
resolveFact _ k e = ((e, T.True):k, Unsolved e)

evalFact :: Expr -> S.State [FactState] T.State
evalFact fact =
  state (\s -> swap (resolveFact [] s fact))


evalExpr' :: Expr -> S.State [FactState] T.State

evalExpr' (Fact fact) = evalFact (Fact fact)

evalExpr' (lhs `Xor` rhs) = do
  l <- evalExpr' lhs
  r <- evalExpr' rhs
  return (l @| r)



main = do
  (print . show . runState (evalExpr' (Fact "Q" `Xor` ((Fact "A") `Xor` (Fact "B"))))) []

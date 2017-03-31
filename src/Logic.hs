module Logic
(
  conjunctionContainsInverseExpr,
  evalImplication,
  combineGoalAndOposite,
  evalExpr,
  Logical (..),
  ) where

import Types
import Prelude hiding (not, True, False)
import Debug.Trace

class Logical a where
  (@|) :: a -> a -> a
  (@+) :: a -> a -> a
  not :: a -> a

  (@^) :: a -> a -> a
  lhs @^ rhs = (lhs @| rhs) @+ not (lhs @+ rhs)

conjunctionContainsInverseExpr :: Expr -> Expr -> Bool
conjunctionContainsInverseExpr goal (lhs `And` rhs) =
  conjunctionContainsInverseExpr goal lhs || conjunctionContainsInverseExpr goal rhs
conjunctionContainsInverseExpr goal expr = goal == Not expr || Not goal == expr

debug str rhs expr ret = trace (str ++ " " ++ show rhs ++ " " ++ show expr) ret

evalImplication :: Expr -> State -> State
evalImplication (Not rhs)  (Unsolved expr)
  | expr == rhs = debug ("pop") (Not rhs)  (Unsolved expr) (Types.True) -- a => !a
  | otherwise =  debug "yyy1= " (Not rhs)  (Unsolved expr) (Unsolved expr)
evalImplication rhs  (Unsolved (Not expr))
  | expr == rhs = debug  "youp" rhs  (Unsolved (Not expr)) (Types.True) -- !a => a
  | otherwise = debug  "eeee" rhs  (Unsolved (Not expr)) (Types.True)-- !b => a
evalImplication rhs  (Unsolved expr)
  | conjunctionContainsInverseExpr rhs expr = debug "trace"  rhs  (Unsolved expr) (Unprovable expr)
evalImplication rhs@(Not _) Types.True =debug "efw" rhs Types.True Types.True
evalImplication (Not rhs) Types.False = debug "444" (Not rhs) Types.False (Unsolved rhs)
evalImplication rhs Types.False = debug "fw" rhs Types.False (Unsolved rhs) -- problem expr dans rhs
evalImplication rhs state = debug "des" rhs state state

combineGoalAndOposite :: (State, Proof) -> (State, Proof) -> Either Proof (State, Proof)
combineGoalAndOposite (Types.True, RuleProof g) (Types.True, RuleProof ng) = Left (Invalid g ng)
combineGoalAndOposite _ (Types.True, ng) = Right (Types.False, ng)
combineGoalAndOposite _ (Unprovable u, ng) = Right (Unprovable u, ng)
combineGoalAndOposite (Unsolved _, g) _ = Right (Types.False, g) -- in the cases where we don't find any answer, the fact is considered a False
combineGoalAndOposite goal _oposite = Right goal


evalExpr :: (Expr -> Resolution Types.State) -> Expr -> Resolution Types.State

evalExpr f (Fact fact) = f (Fact fact)

evalExpr f (Not e) = do {s <- evalExpr f e; return (not s)}

evalExpr f (lhs `Xor` rhs) = do
  l <- evalExpr f lhs
  r <- evalExpr f rhs
  return (l @^ r)

evalExpr f (lhs `Or` rhs) = do
  l <- evalExpr f lhs
  r <- evalExpr f rhs
  return (l @| r)

evalExpr f (lhs `And` rhs) = do
  l <- evalExpr f lhs
  r <- evalExpr f rhs
  return (l @+ r)

instance Logical State where

  False @+ _ = False
  _ @+ False = False
  True @+ b = b
  a @+ True = a
  Unprovable a @+ Unsolved b = compareExprInAnd Unsolved a b
  Unsolved a @+ Unprovable b = compareExprInAnd Unsolved a b
  Unprovable a @+ Unprovable b = compareExprInAnd Unprovable a b
  Unsolved a @+ Unsolved b = compareExprInAnd Unsolved a b


  True @| _ = True
  _ @| True = True
  False @| b = b
  a @| False = a
  Unprovable a @| Unsolved b = compareExprInOr Unprovable a b
  Unsolved a @| Unprovable b = compareExprInOr Unprovable a b
  Unprovable a @| Unprovable b = compareExprInOr Unprovable a b
  Unsolved a @| Unsolved b = compareExprInOr Unsolved a b

  not True = False
  not False = True
  not (Unprovable (Not a)) = Unprovable a
  not (Unprovable a) = Unprovable (Not a)
  not (Unsolved (Not a)) = Unsolved a
  not (Unsolved a) = Unsolved (Not a)

compareExprInAnd :: (Expr -> State) -> Expr -> Expr -> State
compareExprInAnd defaultconstructor a b
  | a == Not b = False
  | Not a == b = False
  | a == b = defaultconstructor a
  | otherwise = defaultconstructor (a `And` b)
compareExprInOr :: (Expr -> State) -> Expr -> Expr -> State
compareExprInOr defaultconstructor a b
  | a == Not b = True
  | Not a == b = True
  | a == b = defaultconstructor a
  | otherwise = defaultconstructor (a `Or` b)


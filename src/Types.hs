-----------------------------------------------------------------------------
-- |
-- Module      :  Types
-- author      : cdannapp & tmielcza
--
-- Types returned by parsing and used for Resolution
--
-----------------------------------------------------------------------------
module Types
(
  Expr(Xor, Or, And, Fact, Not),
  Relation(Eq, Imply),
  Init,
  Query,
  State(..),
  not, (@+), (@|), (@^),
  FactState,
  mapSnd,
  foldExprM,
  Resolved(Resolved),
  Logical,
  exprToFactState,
  resolved,
  resolvedKnowledge
    ) where

import Prelude hiding (Bool(..), not)
import qualified Prelude (Bool(..), not)
import Data.List

-- | the type of expressions all constructors are recursives except Fact
data Expr = Xor Expr Expr |
            Or Expr Expr |
            And Expr Expr |
            Fact String |
            Not Expr

data State = Unsolved Expr | True | False | Unprovable Expr
  deriving (Show, Eq)

-- | Association of an Expr (Mostly a Fact) to a State
type FactState = (Expr, State)

-- | The type of the relations between the Exprs. They form rules.
data Relation = Eq Expr Expr | Imply Expr Expr

-- | this type contains an Expr array. They are init Fact obtained by parsing.
type Init = [FactState]

-- | this type contains an Expr array. They are queries obtained by parsing.
type Query = [Expr]

-- | this type is used for resolution, it contains the knowledges first and then the state of the goal
newtype Resolved = Resolved ([FactState], State)

infixl 4 `Xor`
infixl 5 `Or`
infixl 6 `And`
infixl 7 `Imply`
infixl 8 `Eq`

class Logical a where
  (@|) :: a -> a -> a
  (@+) :: a -> a -> a
  not :: a -> a

  (@^) :: a -> a -> a
  lhs @^ rhs = (lhs @| rhs) @+ not (lhs @+ rhs)

instance Eq Expr where
    (And a1 b1) == (And a2 b2) = cmpBinaryExprSides a1 a2 b1 b2
    (Or a1 b1) == (Or a2 b2) = cmpBinaryExprSides a1 a2 b1 b2
    (Xor a1 b1) == (Xor a2 b2) = cmpBinaryExprSides a1 a2 b1 b2
    a == Not (Not b) = a == b
    Not (Not a) == b = b == a
    (Not a) == (Not b) = a == b
    (Fact a) == (Fact b) = a == b
    a == b = Prelude.False

cmpBinaryExprSides lhs1 rhs1 lhs2 rhs2
  | lhs1 == lhs2 && rhs1 == rhs2 = Prelude.True
  | lhs1 == rhs2 && lhs2 == rhs1 = Prelude.True
  | otherwise = Prelude.False

instance Show Expr where
    show (Xor e1 e2) = "(" ++ show e1 ++ "^" ++ show e2 ++ ")"
    show (Or e1 e2) = "(" ++ show e1 ++ "|" ++ show e2 ++ ")"
    show (And e1 e2) = "(" ++ show e1 ++ "+" ++ show e2 ++ ")"
    show (Fact c) = c
    show (Not e) = "!" ++ show e

instance Show Relation where
    show (Imply e1 e2) = "{" ++ show e1 ++ "=>" ++ show e2 ++ "}"
    show (Eq e1 e2) = "{" ++ show e1 ++ "<=>" ++ show e2 ++ "}"

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
  Unprovable a @| Unsolved b = compareExprInOr Unsolved a b
  Unsolved a @| Unprovable b = compareExprInOr Unsolved a b
  Unprovable a @| Unprovable b = compareExprInOr Unprovable a b
  Unsolved a @| Unsolved b = compareExprInOr Unsolved a b

  not True = False
  not False = True
  not (Unprovable (Not a)) = Unprovable a
  not (Unprovable a) = Unprovable (Not a)
  not (Unsolved (Not a)) = Unsolved a
  not (Unsolved a) = Unsolved (Not a)


instance Logical Resolved where
  Resolved (lknowledge, lstate) @+ Resolved (rknowledge, rstate) =
    Resolved (lknowledge `union` rknowledge, lstate @+ rstate)
  Resolved (lknowledge, lstate) @| Resolved (rknowledge, rstate) =
    Resolved (lknowledge `union` rknowledge, lstate @| rstate)
  Resolved (lknowledge, lstate) @^ Resolved (rknowledge, rstate) =
    Resolved (lknowledge `union` rknowledge, lstate @^ rstate)
  not (Resolved (knowledge, state)) = Resolved (knowledge, not state)

-- Functions of types

exprToFactState :: Expr -> FactState
exprToFactState (Not f) = (f, False)
exprToFactState f = (f, True)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b)

displayEitherFactStates :: Either String [FactState] -> IO ()
displayEitherFactStates (Right [(fact, status)] ) = print (show fact ++" is "++ show status)
displayEitherFactStates (Right((fact, status):rs)) = do
  print (show fact ++" is "++ show status)
  displayEitherFactStates (Right rs)
displayEitherFactStates (Left err) = print $ "Error : " ++ show err

foldExpr :: Logical a => (Expr -> a) -> Expr -> a
foldExpr f (lhs `Xor` rhs) = (foldExpr f lhs) @^ (foldExpr f rhs)
foldExpr f (lhs `Or` rhs) = (foldExpr f lhs) @| (foldExpr f rhs)
foldExpr f (lhs `And` rhs) = (foldExpr f lhs) @+ (foldExpr f rhs)
foldExpr f (Not e) = not (foldExpr f e)
foldExpr f (Fact e) = f (Fact e)

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

foldExprM :: (Monad m, Logical a) => (Expr -> m a) -> Expr -> m a
foldExprM f (lhs `Xor` rhs) = do
  l <- foldExprM f lhs
  r <- foldExprM f rhs
  return (l @^ r)
foldExprM f (lhs `Or` rhs) = do
  l <- foldExprM f lhs
  r <- foldExprM f rhs
  return (l @| r)
foldExprM f (lhs `And` rhs) = do
  l <- foldExprM f lhs
  r <- foldExprM f rhs
  return (l @+ r)
foldExprM f (Not e) = do {e' <- foldExprM f e; return (not e')}
foldExprM f (Fact e) = f (Fact e)

resolved :: Resolved -> ([FactState], State)
resolved (Resolved r) = r

resolvedKnowledge :: Resolved -> [FactState]
resolvedKnowledge = fst . resolved

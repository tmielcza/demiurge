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
  Types.State(..),
  FactState,
  Resolved,
  Resolution,
  exprToFactState
    ) where

import Control.Monad.Trans.State.Lazy as S (State)

import Control.Monad.Trans.Reader (ReaderT)

import Control.Monad.Except (ExceptT)

import Prelude hiding (Bool(..), not)
import qualified Prelude (Bool(..), not)

-- | the type of expressions all constructors are recursives except Fact
data Expr = Xor Expr Expr |
            Or Expr Expr |
            And Expr Expr |
            Fact String |
            Not Expr

data State = Unsolved Expr | True | False | Unprovable Expr
  deriving (Show, Eq)

-- | Association of an Expr (Mostly a Fact) to a State
type FactState = (Expr, Types.State)

-- | The type of the relations between the Exprs. They form rules.
data Relation = Eq Expr Expr | Imply Expr Expr

-- | this type contains an Expr array. They are init Fact obtained by parsing.
type Init = [FactState]

-- | this type contains an Expr array. They are queries obtained by parsing.
type Query = [Expr]

-- | this type is used for resolution, it contains the knowledges first and then the state of the goal
type Resolved = ([FactState], Types.State)

type Resolution a =  ExceptT String (ReaderT [Relation] (S.State [FactState])) a


instance Eq Expr where
    (And a1 b1) == (And a2 b2) = cmpBinaryExprSides a1 a2 b1 b2
    (Or a1 b1) == (Or a2 b2) = cmpBinaryExprSides a1 a2 b1 b2
    (Xor a1 b1) == (Xor a2 b2) = cmpBinaryExprSides a1 a2 b1 b2
    a == Not (Not b) = a == b
    Not (Not a) == b = b == a
    (Not a) == (Not b) = a == b
    (Fact a) == (Fact b) = a == b
    _ == _ = Prelude.False

instance Show Expr where
    show (Xor e1 e2) = "(" ++ show e1 ++ "^" ++ show e2 ++ ")"
    show (Or e1 e2) = "(" ++ show e1 ++ "|" ++ show e2 ++ ")"
    show (And e1 e2) = "(" ++ show e1 ++ "+" ++ show e2 ++ ")"
    show (Fact c) = c
    show (Not e) = "!" ++ show e

instance Show Relation where
    show (Imply e1 e2) = "{" ++ show e1 ++ "=>" ++ show e2 ++ "}"
    show (Eq e1 e2) = "{" ++ show e1 ++ "<=>" ++ show e2 ++ "}"


-- Functions of types

cmpBinaryExprSides lhs1 rhs1 lhs2 rhs2
  | lhs1 == lhs2 && rhs1 == rhs2 = Prelude.True
  | lhs1 == rhs2 && lhs2 == rhs1 = Prelude.True
  | otherwise = Prelude.False


exprToFactState :: Expr -> FactState
exprToFactState (Not f) = (f, False)
exprToFactState f = (f, True)


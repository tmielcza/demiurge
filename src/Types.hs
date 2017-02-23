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
  Knowledge,
  Resolution,
  exprToStringState
    ) where

import Control.Monad.Trans.State.Lazy as S (State)
import Control.Monad.State.Class

import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Reader.Class

import Control.Monad.Except (ExceptT)

import Data.Map

import Prelude hiding (Bool(..), not)
import qualified Prelude (Bool(..), not)

type Query = [Expr]
type Init = [(String, Types.State)]

-- | the type of expressions all constructors are recursives except Fact
data Expr = Xor Expr Expr |
            Or Expr Expr |
            And Expr Expr |
            Fact String |
            Not Expr

data State = Unsolved Expr | True | False | Unprovable Expr
  deriving (Show, Eq)

-- | Association of an Expr (Mostly a Fact) to a State
--type FactState = (Expr, Types.State)
type Knowledge = Map String Types.State

-- | The type of the relations between the Exprs. They form rules.
data Relation = Eq Expr Expr | Imply Expr Expr

type Resolution a = ExceptT String (ReaderT [Relation] (S.State Knowledge)) a


{-getKnowledge :: (MonadState s m) => m s
getKnowledge = get


 quelle difference avec le modify normal
 modify :: MonadState s m => (s -> s) -> m ()
modify f = state (\s -> ((), f s))

modifyKnowledge :: MonadState s m => (s -> s) -> m ()
modifyKnowledge = modify

getRules :: (MonadReader r m) => m r
getRules = ask
-}




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

exprToStringState :: Expr -> (String, Types.State)
exprToStringState (Not (Not (e))) = exprToStringState e
exprToStringState (Not (Fact f)) = (f, False)
exprToStringState (Fact f) = (f, True)


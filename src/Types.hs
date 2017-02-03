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
  Init(Init),
  Query(Query),
  State(..),
  Trilean(..),
  FactState,
  rhs, lhs,
    ) where

import Prelude hiding  (True, False, (+), (||), (^))


-- | the type of expressions all constructors are recursives except Fact
data Expr = Xor Expr Expr |
            Or Expr Expr |
            And Expr Expr |
            Fact String |
            Not Expr
            deriving (Eq)

-- | The type of the relations between the Exprs. They form rules.
data Relation = Eq Expr Expr | Imply Expr Expr

-- | this type contains an Expr array. They are init Fact obtained by parsing.
newtype Init = Init [Expr]

-- | this type contains an Expr array. They are queries obtained by parsing.
newtype Query = Query [Expr]

data State = True | False | Unknown deriving (Show, Eq)

type FactState = (Expr, State)

instance Show Expr where
    show (Xor e1 e2) = "(" ++ show e1 ++ "^" ++ show e2 ++ ")"
    show (Or e1 e2) = "(" ++ show e1 ++ "|" ++ show e2 ++ ")"
    show (And e1 e2) = "(" ++ show e1 ++ "+" ++ show e2 ++ ")"
    show (Fact c) = c
    show (Not e) = "!" ++ show e

instance Show Relation where
    show (Imply e1 e2) = "{" ++ show e1 ++ "=>" ++ show e2 ++ "}"
    show (Eq e1 e2) = "{" ++ show e1 ++ "<=>" ++ show e2 ++ "}"

instance Show Init where
    show (Init facts) = "Init: "++ show facts

instance Show Query where
    show (Query facts) = "Query: "++ show facts

rhs (Eq _ r) = r
rhs (Imply _ r) = r

lhs (Eq l _) = l
lhs (Imply l _) = l

class (Eq t) => Trilean t where
  true, false, unknown :: t
  t_not :: t -> t
  (@+), (@|), (@^) :: t -> t -> t
  a @+ b
    | a == false = false
    | b == false = false
    | a == true && b == true = true
    | otherwise = unknown
  a @| b
    | a == true = true
    | b == true = true
    | a == false && b == false = false
    | otherwise = unknown
  t_not a
    | a == true = false
    | a == false = true
    | otherwise = unknown
  a @^ b = (a @| b) @+ t_not (a @+ b)


instance Trilean State where
    true = True
    false = False
    unknown = Unknown

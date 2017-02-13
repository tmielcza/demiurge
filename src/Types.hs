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
  not, (@+), (@|), (@^),
  FactState,
  mapSnd,
    ) where

import Prelude hiding (Bool(..),not)
import qualified Prelude (Bool(..))

-- | the type of expressions all constructors are recursives except Fact
data Expr = Xor Expr Expr |
            Or Expr Expr |
            And Expr Expr |
            Fact String |
            Not Expr

infixl 4 `Xor`
infixl 5 `Or`
infixl 6 `And`
infixl 7 `Imply`
infixl 8 `Eq`

-- | The type of the relations between the Exprs. They form rules.
data Relation = Eq Expr Expr | Imply Expr Expr

-- | this type contains an Expr array. They are init Fact obtained by parsing.
newtype Init = Init [Expr]

-- | this type contains an Expr array. They are queries obtained by parsing.
newtype Query = Query [Expr]

-- | Used to browse and find he state of a fact, Unknown is manda
data State = Unsolved Expr | True | False | Unprovable Expr
  deriving (Show, Eq)

type FactState = (Expr, State)

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

instance Show Init where
    show (Init facts) = "Init: "++ show facts

instance Show Query where
    show (Query facts) = "Query: "++ show facts

(@+) :: State -> State -> State

False @+ _ = False
_ @+ False = False
True @+ b = b
a @+ True = a
Unprovable a @+ Unsolved b = Unsolved (a `And` b)
Unsolved a @+ Unprovable b = Unsolved (a `And` b)
Unprovable a @+ Unprovable b
  | a == Not b = False
  | Not b == a = False
  | a == b = Unprovable a
  | otherwise = Unprovable (a `And` b)
Unsolved a @+ Unsolved b
  | a == Not b = False
  | Not a == b = False
  | a == b = Unsolved a
  | otherwise = Unsolved (a `And` b)

(@|) :: State -> State -> State

True @| _ = True
_ @| True = True
False @| b = b
a @| False = a
Unprovable a @| Unsolved b = Unsolved (a `Or` b)
Unsolved a @| Unprovable b = Unsolved (a `Or` b)
Unprovable a @| Unprovable b
  | a == Not b = True
  | Not a == b = True
  | a == b = Unprovable a
  | otherwise = Unprovable (a `Or` b)
Unsolved a @| Unsolved b
  | a == Not b = True
  | Not a == b = True
  | a == b = Unsolved a
  | otherwise = Unsolved (a `Or` b)

not :: State -> State

not True = False
not False = True
not (Unprovable (Not a)) = Unprovable a
not (Unprovable a) = Unprovable (Not a)
not (Unsolved (Not a)) = Unsolved a
not (Unsolved a) = Unsolved (Not a)

(@^) :: State -> State -> State
a @^ b = (a @| b) @+ not (a @+ b)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b)

displayEitherFactStates (Right((fact, status):[])) = print (show fact ++" is "++ show status)
displayEitherFactStates (Right((fact, status):rs)) = do
  print (show fact ++" is "++ show status)
  displayEitherFactStates (Right rs)
displayEitherFactStates (Left err) = print $ "Error : " ++ show err


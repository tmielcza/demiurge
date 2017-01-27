
module BackwardChaining
  ( bc
  ) where

import Parse
import Prelude hiding  (True, False, (+), (||), (^))
import Types

data State = True | False | Unknown deriving (Show, Eq)
class (Eq t) => Trilean t where
  true, false, unknown :: t
  t_not :: t -> t
  (+), (||), (^) :: t -> t -> t
  a + b
    | a == false = false
    | b == false = false
    | a == true && b == true = true
    | otherwise = unknown
  a || b
    | a == true = true
    | b == true = true
    | a == false && b == false = false
    | otherwise = unknown
  t_not a
    | a == true = false
    | a == false = true
    | otherwise = unknown
  a ^ b = (a || b) + t_not (a + b)


instance Trilean State where
    true = True
    false = False
    unknown = Unknown

bc :: ([Relation], Init, Query) -> [Expr]
bc (relations, init, query) = []

isFlagged :: [(Expr, State)] -> Expr -> Bool
isFlagged ((f, _):xs) expect
  | f == expect = True
  | otherwise = isFlagged xs expect
isFlagged [] _ = False

{-
(A + B) ^ (C + D)
E + F => D


B + D => A
C => B
B => D

=C

?A

-}

{-fc :: [(Expr, State)] -> [Relation] -> Expr -> Bool
fc flags rels goal
  | isFlagged flags goal = fc
-}

-- | replaceExpression takes an expression e and a fact f and an expression r it browses
-- the expression e and find the fact f to replace it with r and return the result
replaceExpression :: Expr -> Expr -> Expr -> Expr
replaceExpression e f r = exprMap e (\x -> if x == f then r else e)

-- | Browses Expr to apply a function to each fact
exprMap :: Expr -> (Expr -> Expr) -> Expr
exprMap e@(Fact _) func = func e
exprMap (Xor lhs rhs) func = Xor (func lhs) (func rhs)
exprMap (Or lhs rhs) func = Or (func lhs) (func rhs)
exprMap (And lhs rhs) func = And (func lhs) (func rhs)
exprMap (Not e) func = Not (func e)


-- Prend une expression, l'evalue a l'aide des regles et des connaissances,
-- et renvoie les connaissances acquises, ainsi que l'etat de l'expression
eval :: Expr -> [(Expr, State)] -> [Relation] -> ([(Expr, State)], State)


 -- | Loops on the rules to find the ones that concern our goal
-- loopOnRule goal = filter (\(Imply _ rhs) -> rhs == goal )

loopOnRule goal = filter cmp where
  cmp (Imply _ rhs) = rhs == goal



{-
pour une query on va rassembler toutes les possibilites
pour ca on va parcourir toutes les regles et en trouver une dont la partie droite est un fait present dans nos possibilites
-}

--f kb (Imply lhs rhs):rels = 


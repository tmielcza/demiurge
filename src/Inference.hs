module Inference
  (
    inferRules
  ) where

import Types

infer :: [Relation] -> Expr -> [([Relation], Relation)]
infer stack@((premices `Imply` (lhs `And` rhs)):_) goal =
  infer ((premices `Imply` rhs):stack) goal ++
  infer ((premices `Imply` lhs):stack) goal

infer stack@(((lhs `Or` rhs) `Imply` conclusion):_) goal =
  infer ((lhs `Imply` conclusion):stack) goal ++
  infer ((rhs `Imply` conclusion):stack) goal

infer stack@((premices `Imply` (lhs `Or` rhs)):_) goal =
  infer (((premices `And` opposite lhs) `Imply` rhs):stack) goal ++
  infer (((premices `And` opposite rhs) `Imply` lhs):stack) goal

infer stack@((premices `Imply` (lhs `Xor` rhs)):_) goal =
  infer ((premices `Imply` ((lhs `Or` rhs) `And` ((Not lhs) `Or` (Not rhs)))):stack) goal


-- inference of not rules
infer stack@((rhs `Imply` Not (Not lhs)):_) goal =
  infer ((rhs `Imply` lhs):stack) goal

infer stack@((premices `Imply` Not (lhs `And` rhs)):_) goal =
  infer ((premices `Imply` (opposite (lhs `And` rhs))):stack) goal

infer stack@((premices `Imply` Not (lhs `Or` rhs)):_) goal =
  infer ((premices `Imply` (opposite (lhs `Or` rhs))):stack) goal

infer stack@((premices `Imply` Not (lhs `Xor` rhs)):_) goal =
  infer ((premices `Imply` (opposite (lhs `Xor` rhs))):stack) goal

-- return the rule sent if the rhs is the fact we are looking for
infer list@(r@(_ `Imply` fact):_) goal
  | goal == fact = [(list, r)]
 -- | Not goal == fact = [r]
  | otherwise    = []


launchInferences :: [Relation] -> Expr -> [([Relation], Relation)]
--modus tollens or transposition
-- here the xs is the possible equivalence (or an empty list)
launchInferences list@((lhs `Imply` rhs):xs) goal
  | rhs == Not lhs = infer (( lhs `Imply` rhs):xs) goal
  | otherwise = infer (( lhs `Imply` rhs):xs) goal ++ infer (( Not rhs `Imply` Not lhs):list) goal

-- distribution of Equivalence in implications
launchInferences eq@[(rhs `Eq` lhs)] goal =
  launchInferences ((rhs `Imply` lhs):eq) goal ++ launchInferences ((lhs `Imply` rhs):eq) goal

inferRules :: [Relation] -> Expr -> [([Relation], Relation)]
inferRules rules goal = foldr (\r -> (++) ((launchInferences [r] goal))) [] rules

opposite :: Expr -> Expr
opposite (Fact f) = Not (Fact f)
opposite (Not e) = e
opposite (lhs `And` rhs) = (opposite lhs) `Or` (opposite rhs)
opposite (lhs `Or` rhs) = (opposite lhs) `And` (opposite rhs)
opposite (lhs `Xor` rhs) = ((opposite lhs) `And` (opposite rhs)) `Or` (lhs `And` rhs)

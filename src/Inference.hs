module Inference
  (
    inferRules
  ) where

import Types
import Debug.Trace

infer :: [Relation] -> Expr -> [([Relation], Relation)]
infer stack@((premices `Imply` (lhs `And` rhs)):_) goal =
  infer ((premices `Imply` rhs):stack) goal ++
  infer ((premices `Imply` lhs):stack) goal

infer stack@(((lhs `Or` rhs) `Imply` conclusion):_) goal =
  infer ((lhs `Imply` conclusion):stack) goal ++
  infer ((rhs `Imply` conclusion):stack) goal

infer stack@((premices `Imply` (lhs `Or` rhs)):_) goal =
  infer (((toDnf (premices `And` (Not lhs))) `Imply` rhs):stack) goal ++
  infer (((toDnf (premices `And` (Not rhs))) `Imply` lhs):stack) goal

{-infer stack@((premices `Imply` (lhs `Xor` rhs)):_) goal =
  infer ((premices `Imply` ((lhs `Or` rhs) `And` ((Not lhs) `Or` (Not rhs)))):stack) goal
-}

-- inference of not rules
infer stack@((rhs `Imply` Not (Not lhs)):_) goal =
  infer ((rhs `Imply` lhs):stack) goal

infer stack@((premices `Imply` Not (lhs `And` rhs)):_) goal =
  infer ((premices `Imply` ((toCnf . Not) (lhs `And` rhs))):stack) goal

infer stack@((premices `Imply` Not (lhs `Or` rhs)):_) goal =
  infer ((premices `Imply` ((toCnf . Not) (lhs `Or` rhs))):stack) goal

infer stack@((premices `Imply` Not (lhs `Xor` rhs)):_) goal =
  infer ((premices `Imply` ((toCnf . Not) (lhs `Xor` rhs))):stack) goal

-- return the rule sent if the rhs is the fact we are looking for
infer list@(r@(_ `Imply` fact):_) goal
  | goal == fact = [(list, r)]
 -- | Not goal == fact = [r]
  | otherwise    = []


launchInferences :: [Relation] -> Expr -> [([Relation], Relation)]
--modus tollens or transposition
-- here the xs is the possible equivalence (or an empty list)
launchInferences list@((lhs `Imply` rhs):xs) goal
  | rhs == Not lhs =  infer ((toDnf lhs `Imply` toCnf rhs):(lhs `Imply` rhs):xs) goal
  | otherwise = infer ((toDnf lhs `Imply` toCnf rhs):(lhs `Imply` rhs):xs) goal
    ++ infer ((toDnf (Not rhs) `Imply` toCnf (Not lhs)):(Not rhs `Imply` Not lhs):list) goal

-- distribution of Equivalence in implications
launchInferences eq@[(rhs `Eq` lhs)] goal =
  launchInferences ((rhs `Imply` lhs):eq) goal ++ launchInferences ((lhs `Imply` rhs):eq) goal

inferRules :: [Relation] -> Expr -> [([Relation], Relation)]
inferRules rules goal = foldr (\r -> (++) ((launchInferences [r] goal))) [] rules

opposite :: Expr -> Expr
opposite (Fact f) = Not (Fact f)
opposite (Not (Not e)) = opposite e
opposite (Not e) = e
opposite (lhs `And` rhs) = (opposite lhs) `Or` (opposite rhs)
opposite (lhs `Or` rhs) = (opposite lhs) `And` (opposite rhs)
-- opposite e = Not e

transformCnf (a `Or` (b `And` c)) =
  let abr = toCnf a in ((abr `Or` toCnf b) `And` (abr `Or` toCnf c))
transformCnf (a `Xor` b) = ((toCnf a) `Or` (toCnf (Not b))) `And` (((toCnf (Not a)) `Or` (toCnf b)))
transformCnf not@(Not (Fact _)) = not
transformCnf not@(Not e) = (toCnf . opposite) (toDnf e)
transformCnf e = e

transformDnf (a `And` (b `Or` c)) =
  let abr = toDnf a in ((abr `And` toDnf b) `Or` (abr `And` toDnf c))
transformDnf (a `Xor` b) = (toDnf a) `And` (toDnf (Not b)) `Or` ((toDnf (Not a)) `And` (toDnf b))
transformDnf not@(Not (Fact _)) = not
transformDnf not@(Not e) = (toDnf . opposite) (toCnf e)
transformDnf e = e

browse transform (a `And` b) = transform (browse transform a `And` browse transform b)
browse transform (a `Or` b) = transform (browse transform a `Or` browse transform b)
browse transform (a `Xor` b) = transform (browse transform a `Xor` browse transform b)
browse transform e = transform e

toCnf :: Expr -> Expr
toCnf = browse transformCnf
toDnf :: Expr -> Expr
toDnf = browse transformDnf




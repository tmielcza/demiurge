module Inference
  (
    inferRules
  ) where

import Types


infer :: Relation -> Expr -> [Relation]
infer (premices `Imply` (lhs `And` rhs)) goal =
  infer (premices `Imply` rhs) goal ++
  infer (premices `Imply` lhs) goal

infer (premices `Imply` (lhs `Or` rhs)) goal =
  infer ((premices `And` Not lhs) `Imply` rhs) goal ++
  infer ((premices `And` Not rhs) `Imply` lhs) goal

infer (premices `Imply` (lhs `Xor` rhs)) goal =
 infer (premices `Imply` ((lhs `Or` rhs) `And` Not (lhs `And` rhs))) goal


-- inference of not rules
infer (rhs `Imply` Not (Not lhs)) goal =
  infer (rhs `Imply` lhs) goal

infer (premices `Imply` Not (lhs `And` rhs)) goal =
  infer (premices `Imply` (Not lhs `Or` Not rhs)) goal

infer (premices `Imply` Not (lhs `Or` rhs)) goal =
  infer (premices `Imply` (Not lhs `And` Not rhs)) goal

infer (premices `Imply` Not (lhs `Xor` rhs)) goal =
  infer (premices `Imply` (Not (lhs `Or` rhs) `Or` (lhs `And` rhs))) goal

-- return the rule sent if the rhs is the fact we are looking for
infer r@(_ `Imply` fact) goal
  | goal == fact = [r]
  | Not goal == fact = [r]
  | otherwise    = []

--modus tollens or transposition
launchInferences ( lhs `Imply` rhs) goal =
  infer ( lhs `Imply` rhs) goal ++ infer ( Not rhs `Imply` Not lhs) goal

-- distribution of Equivalence in implications
launchInferences (rhs `Eq` lhs) goal =
  launchInferences (rhs `Imply` lhs) goal ++ launchInferences (lhs `Imply` rhs) goal

-- inferRules :: [Relation] -> Expr -> [Relation]
inferRules rules goal = foldr (\r -> (++) (launchInferences r goal)) [] rules

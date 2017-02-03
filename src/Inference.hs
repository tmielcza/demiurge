module Inference
  (
  ) where

import Types

infer :: Relation -> Expr -> [Relation]
infer (rhs `Eq` lhs) goal =
  infer (rhs `Imply` lhs) goal ++ infer (lhs `Imply` rhs) goal

infer (premices `Imply` (lhs `And` rhs)) goal =
  infer (premices `Imply` rhs) goal ++
  infer (premices `Imply` lhs) goal

infer (premices `Imply` (lhs `Or` rhs)) goal =
  infer (premices `And` Not lhs `Imply` rhs) goal ++
  infer (premices `And` Not rhs `Imply` lhs) goal

infer (premices `Imply` (lhs `Xor` rhs)) goal =
 infer (premices `Imply` ((lhs `Or` rhs) `Or` Not (lhs `And` rhs))) goal
  -- infer (premices `And` Not lhs `Imply` rhs) goal ++
  -- infer (premices `And` Not rhs `Imply` lhs) goal ++
  -- infer (premices `And` lhs `Imply` Not rhs) goal ++
  -- infer (premices `And` rhs `Imply` Not lhs) goal

infer (rhs `Imply` (Not (Not lhs))) goal =
  infer (rhs `Imply` lhs) goal

infer (premices `Imply` Not (lhs `And` rhs)) goal =
  infer (premices `Imply` (Not lhs `Or` Not rhs)) goal
--  infer (premices `And` lhs `Imply` Not rhs) goal ++
--  infer (premices `And` rhs `Imply` Not lhs) goal

infer (premices `Imply` Not (lhs `Or` rhs)) goal =
  infer (premices `Imply` (Not lhs `And` Not rhs)) goal

infer (premices `Imply` Not (lhs `Xor` rhs)) goal =
  infer (premices `Imply` (Not (lhs `Or` rhs) `Or` (lhs `And` rhs))) goal

infer r@(_ `Imply` fact) goal
  | goal == fact = [r]
  | goal == Not fact = [r]
  | otherwise    = []


{-
getInferedRules :: [Relation] -> Expr -> [Relation]
getInferedRules rules goal =
-}


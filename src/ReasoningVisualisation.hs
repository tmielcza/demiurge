module ReasoningVisualisation(
  showFactResolution,
  showProof
  ) where

import Types(
  Knowledge,
  State(..),
  Proof(..),
  Expr(..),
  Relation(..))

import Data.Map  as M (lookup)


getExistantInKnowledge:: Knowledge -> String -> ((State, Proof) -> String) -> String
getExistantInKnowledge k fact func = maybe (show (Unsolved (Fact fact))) (func) (M.lookup fact k)

showResolvedExpr :: Knowledge -> Expr -> String
showResolvedExpr k (Xor e1 e2) = "(" ++ showResolvedExpr k e1 ++ "^" ++ showResolvedExpr k e2 ++ ")"
showResolvedExpr k (Or e1 e2) = "(" ++ showResolvedExpr k e1 ++ "|" ++ showResolvedExpr k e2 ++ ")"
showResolvedExpr k (And e1 e2) = "(" ++ showResolvedExpr k e1 ++ "+" ++ showResolvedExpr k e2 ++ ")"
showResolvedExpr k (Not e) = "!" ++ showResolvedExpr k e
showResolvedExpr k (Fact fact) = getExistantInKnowledge k fact (\(st, _) -> show st)

getFacts :: Expr -> [Expr]
getFacts (Xor e1 e2) = getFacts e1 ++ getFacts e2
getFacts (And e1 e2) = getFacts e1 ++ getFacts e2
getFacts (Or e1 e2)  = getFacts e1 ++ getFacts e2
getFacts (Not e)     = getFacts e
getFacts f           = [f]

showRulesTransformation :: [Relation] -> String
showRulesTransformation list@(r:_) =
  let intro = "the rule " ++ (show r) ++ "have been infered through this steps:"
      transformations = foldr (\new prev -> prev ++ "\n\t" ++ show new) "" list
  in intro ++ transformations ++ "\n"

rulesReasoning :: Knowledge -> [Relation] -> String
rulesReasoning knowledge list@(rule@(lft `Imply` _):_)=
  let
    rulesInference = showRulesTransformation list
    showFactAndProof f (st, p) = "Fact "++ f ++ " is" ++ (show st) ++ ", here is the reasoning:\n" ++ showProof knowledge (Fact f) p
    reasoning = foldl (\prev (Fact x) -> (getExistantInKnowledge knowledge x (showFactAndProof x)) ++ prev) "" (getFacts lft)
    conclusion = "so the expr " ++ (show lft) ++ "is equal to " ++ showResolvedExpr knowledge lft
  in rulesInference ++ reasoning ++ conclusion


showProof :: Knowledge -> Expr -> Proof -> String
showProof knowledge goal (Invalid list1@(rule1:_) list2@(rule2:_)) =
  "Two have different result for " ++ show goal ++ ":\n"++
   rulesReasoning knowledge list1 ++ "\n" ++
   rulesReasoning knowledge list2 ++ "\n" ++
   show rule1 ++ " has a different result from " ++ show rule2 ++ "\n"

showProof knowledge goal (Contradiction list@(rule:_) []) =
  "Their is a contradiction in the rule " ++ show rule ++ rulesReasoning knowledge list

showProof knowledge goal (Tautology list@(rule:_) []) =
  "Their is a tautology in the rule " ++ show rule ++ rulesReasoning knowledge list

showProof knowledge goal (RuleProof list@(rule:_)) =
  "The rule used to get the result for" ++ show goal ++ "is: " ++
  show rule ++ rulesReasoning knowledge list ++ "\n"

showProof knowledge goal (RuleProof []) =
  "No rule match the goal " ++ show goal ++ "\n"

showProof knowledge goal (Known st) =
  "The goal " ++ show goal ++ " is initiate to " ++ show st ++ "\n"

showFactResolution :: Knowledge -> Expr -> String
showFactResolution k (Fact goal) =
  let func (st, pr) = "We are looking for " ++ show goal ++  " here is its resolution: \n" ++
                      showProof k (Fact goal) pr ++ "So " ++ show goal ++ " is " ++ show st ++ "\n"
  in getExistantInKnowledge k goal func


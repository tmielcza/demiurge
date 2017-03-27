module ReasoningVisualisation(
  showFactResolution,
  runShowProof
  ) where

import Types(
  Knowledge,
  State(..),
  Proof(..),
  Expr(..),
  Relation(..))

import Control.Monad(MonadPlus(..))

import Data.Map  as M (lookup, insert)
import qualified Control.Monad.Trans.State.Lazy as S(State(..), get, runState, modify)

type KnowledgeState a = S.State Knowledge a

infixr 5 *++*
(*++*) :: (KnowledgeState String )-> (KnowledgeState String ) -> (KnowledgeState String)
(*++*) kn1 kn2 = do
  str1 <- kn1
  str2 <- kn2
  return (str1 ++ str2)

-- concat a string with a knowledgeState string
-- Operator with the Same precedence as the ++ used for the String
-- The star is in the side of the knowledge
infixr 5 *++
(*++) :: (KnowledgeState String )-> String -> (KnowledgeState String)
(*++) kn str = do
  str2 <- kn
  return (str2 ++ str)

(++*) :: String -> (KnowledgeState String) -> (KnowledgeState String)
infixr 5 ++*
(++*) str kn = do
  str2 <- kn
  return (str ++ str2)

getExistantInKnowledge:: String -> ((State, Proof) -> KnowledgeState String) -> KnowledgeState String
getExistantInKnowledge fact func = do
  k <- S.get
  maybe (return "Unreachable code") (func) (M.lookup fact k)

resolvedToString :: Expr -> Expr -> String -> KnowledgeState String
resolvedToString e1 e2 opeSign = ("(" ++* showResolvedExpr e1) *++* (opeSign ++* showResolvedExpr e2 *++ ")")

showResolvedExpr :: Expr  -> KnowledgeState String
showResolvedExpr (Xor e1 e2) = resolvedToString e1 e2 "^"
showResolvedExpr (Or e1 e2) = resolvedToString e1 e2 "|"
showResolvedExpr (And e1 e2) = resolvedToString e1 e2 "+"
showResolvedExpr (Not e) = "!" ++* showResolvedExpr e
showResolvedExpr (Fact fact) = getExistantInKnowledge fact (\(_, Known st) -> (return . show) st)

getFacts :: Expr -> [Expr]
getFacts (Xor e1 e2) = getFacts e1 ++ getFacts e2
getFacts (And e1 e2) = getFacts e1 ++ getFacts e2
getFacts (Or e1 e2)  = getFacts e1 ++ getFacts e2
getFacts (Not e)     = getFacts e
getFacts f           = [f]

showRulesTransformation :: [Relation] -> String
showRulesTransformation list@(r:_) =
  let intro = "the rule " ++ (show r) ++ " have been infered through this steps:"
      transformations = foldr (\new prev -> prev ++ "\n\t" ++ show new) "" list
  in intro ++ transformations ++ "\n"

rulesReasoning :: [Relation] -> KnowledgeState String
rulesReasoning list@(rule@(lft `Imply` _):_)=
  let
    rulesInference = if (length list > 1) then showRulesTransformation list else ""
    showSubgoal f (_st, Known b) = return ("Fact "++ f ++ " is known as " ++ (show b) ++ "\n")
    showSubgoal f (st, p) = ("Fact " ++ f ++ " is " ++ (show st) ++ ":\n") ++* (consumeProof (Fact f) p)
    reasoning = foldl (\prev (Fact x) -> (getExistantInKnowledge x (showSubgoal x)) *++* prev) (return "") (getFacts lft)
    conclusion = "so the goal is equal to " ++* (showResolvedExpr lft) *++ "\n"
  in rulesInference ++* reasoning *++* conclusion

rulesReasoning [] = return "Unreachable Code, showProof check if the list is empty"

-- Here it's mandatory to return a string because getExistantKnowledge returns a string
setAsKnown:: Expr -> KnowledgeState String
setAsKnown (Fact f) =
  getExistantInKnowledge f (\(state, _proof) -> do { S.modify (M.insert f (state, Known state)); return f})

setAsUnknown:: Expr -> KnowledgeState String
setAsUnknown (Fact f) =
  getExistantInKnowledge f (\(state, _proof) -> do { S.modify (M.insert f (state, Known (Unsolved (Fact f)))); return f})

showProof :: Expr -> Proof -> KnowledgeState String
showProof goal (Invalid list1@(rule1:_) list2@(rule2:_)) =
  (("There are different results for " ++ show goal ++ ":\n" ++ show rule1 ++ "\n") ++* rulesReasoning list1 *++ (show rule2 ++ "\n")) *++*
   rulesReasoning list2 *++ (show rule1 ++ " has a different result from " ++ show rule2 ++ "\n")

showProof goal (Contradiction list@(rule:_) []) =
  ("Their is a contradiction in the rule " ++ show rule) ++* rulesReasoning list

showProof goal (Tautology list@(rule:_) []) =
  ("Their is a tautology in the rule " ++ show rule) ++* rulesReasoning list

showProof goal (RuleProof list@(rule:_)) = do
  k <- S.get
  ("The rule used for " ++ show goal ++ " is: " ++
     show rule ++ "\n") ++* rulesReasoning list *++ "\n"

showProof goal (RuleProof []) =
  return $ "No rule matches the goal " ++ show goal ++ "\n"

showProof goal (Known st) =
  return $ "The fact " ++ show goal ++ " is known as " ++ show st ++ "\n"

--showProof goal kn = return ("$$$$" ++ show kn)

consumeProof :: Expr -> Proof -> KnowledgeState String
consumeProof goal proof = do
  setAsUnknown goal
  s <- showProof goal proof
  setAsKnown goal
  return s

runShowProof :: Knowledge -> Expr -> Proof -> String
runShowProof k g p = fst $ S.runState (consumeProof g p) k

showFactResolution :: Knowledge -> Expr -> (String, Knowledge)
showFactResolution k (Fact goal) =
  let
    func :: (State, Proof) -> KnowledgeState String
    func (st, pr) = return ("We are looking for " ++ show goal ++  " here is its resolution: \n" ++
                      runShowProof k (Fact goal) pr ++ "So " ++ show goal ++ " is " ++ show st ++ "\n")
  in S.runState (getExistantInKnowledge goal func) k


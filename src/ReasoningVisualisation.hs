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
showResolvedExpr (Fact fact) = getExistantInKnowledge fact (\(st, _) -> (return . show) st)

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
    showSubgoal f (st, Known b) = return ("Fact "++ f ++ " has been initialised at " ++ (show st) ++ "\n")
    showSubgoal f (st, p) = ("Fact " ++ f ++ " is " ++ (show st) ++ ":\n") ++* (showProof (Fact "P") p)
    reasoning = foldl (\prev (Fact x) -> (getExistantInKnowledge x (showSubgoal x)) *++* prev) (return "") (getFacts lft)
    conclusion = "so the goal is equal to " ++* (showResolvedExpr lft) *++ "\n"
  in rulesInference ++* reasoning *++* conclusion

rulesReasoning [] = return "Unreachable Code, showProof check if the list is empty"

-- Here it's mandatory to return a string because getExistantKnowledge returns a string
setAsKnown:: Expr -> KnowledgeState String
setAsKnown (Fact f) =
  getExistantInKnowledge f (\(state, _proof) -> do { S.modify (M.insert f (state, Known state)); return f})


showProof :: Expr -> Proof -> KnowledgeState String
showProof goal (Invalid list1@(rule1:_) list2@(rule2:_)) = do
  setAsKnown goal;
  (("There are different results for " ++ show goal ++ ":\n" ++ show rule1 ++ "\n") ++* rulesReasoning list1 *++ (show rule2 ++ "\n")) *++*
   rulesReasoning list2 *++ (show rule1 ++ " has a different result from " ++ show rule2 ++ "\n")

showProof goal (Contradiction list@(rule:_) []) = do
  setAsKnown goal;
  ("Their is a contradiction in the rule " ++ show rule) ++* rulesReasoning list

showProof goal (Tautology list@(rule:_) []) = do
  setAsKnown goal;
  ("Their is a tautology in the rule " ++ show rule) ++* rulesReasoning list

showProof goal (RuleProof list@(rule:_)) = do
  setAsKnown goal;
  ("The rule used for " ++ show goal ++ " is: " ++
     show rule ++ "\n") ++* rulesReasoning list *++ "\n"

showProof goal (RuleProof []) = do
  setAsKnown goal;
  return $ "No rule matches the goal " ++ show goal ++ "\n"

showProof goal (Known st) = do
  setAsKnown goal;
  return $ "The fact " ++ show goal ++ " is known as " ++ show st ++ "\n"

--showProof goal kn = return ("$$$$" ++ show kn)

runShowProof :: Knowledge -> Expr -> Proof -> String
runShowProof k g p = fst $ S.runState (showProof g p) k

showFactResolution :: Knowledge -> Expr -> String
showFactResolution k (Fact goal) =
  let
    func :: (State, Proof) -> KnowledgeState String
    func (st, pr) = return ("We are looking for " ++ show goal ++  " here is its resolution: \n" ++
                      runShowProof k (Fact goal) pr ++ "So " ++ show goal ++ " is " ++ show st ++ "\n")
  in fst $ S.runState (getExistantInKnowledge goal func) k


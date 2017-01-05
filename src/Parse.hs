module Parse
( parse,
  checkReturn,
  tokenize,
  astXor
) where

import Debug.Trace
import Data.Char
import Control.Monad

data Token = Letter Char | Operator Ope | Bang | LParen | RParen | InitTk | QueryTk
--    deriving(Show)
data Ope = Xor | Or | And | Eq | Imply
    deriving(Eq, Ord)
data Expr = Grp Ope Expr Expr | Fact Char | Not Expr
    deriving(Eq)
data Line = Rule Expr | Init [Expr] | Query [Expr]

instance Show Line where
    show (Rule exp) = show exp
    show (Init facts) = "Init" ++ show facts
    show (Query facts) = "Query" ++ show facts

instance Show Ope where
    show Or = "|"
    show And = "+"
    show Xor = "^"
    show Eq = "<=>"
    show Imply = "=>"

instance Show Expr where
    show (Grp o e1 e2) = "(" ++ show e1 ++ show o ++ show e2 ++ ")"
    show (Fact c) = [c]
    show (Not expr) = "!" ++ show expr

instance Show Token where
    show (Letter c) = [c]
    show (Operator op) = show op
    show Bang = "!"
    show LParen = "("
    show RParen = ")"
    show InitTk = "="
    show QueryTk = "?"
    showList xs = (++ "[" ++ showListTokens xs "" ++ "]")

showListTokens (x:xs) str = showListTokens xs (str ++ show x)
showListTokens [] str = str

addExpr op Nothing r = Just r
addExpr op (Just l) r = Just (Grp op l r)

astEq :: Maybe Expr -> [Token] -> (Maybe Expr, [Token])
astEq expr rest =
  case astXor Nothing rest of
    (Just expr_d, Operator Eq:rest_d) -> case astXor Nothing rest_d of
      (Just expr_d2, rest_d) -> (Just (Grp Eq expr_d expr_d2), rest_d)
      other -> other
    (Just expr_d, Operator Imply:rest_d) -> case astXor Nothing rest_d of
      (Just expr_d2, rest_d) -> (Just (Grp Imply expr_d expr_d2), rest_d)
      other -> other
    (Just _, []) -> (Nothing, [Operator Imply])
    other -> other

astXor :: Maybe Expr -> [Token] -> (Maybe Expr, [Token])
astXor expr rest =
  case astOr Nothing rest of
    (Just expr_d, Operator Xor:rest_d) -> astXor (addExpr Xor expr expr_d) rest_d
    (Just expr_d, rest_d) -> (addExpr Xor expr expr_d, rest_d)
    other -> other

astOr :: Maybe Expr -> [Token] -> (Maybe Expr, [Token])
astOr expr rest =
  case astAnd Nothing rest of
    (Just expr_d, Operator Or:rest_d) -> astOr (addExpr Or expr expr_d) rest_d
    (Just expr_d, rest_d) -> (addExpr Or expr expr_d, rest_d)
    other -> other

astAnd :: Maybe Expr -> [Token] -> (Maybe Expr, [Token])
astAnd expr rest =
  case astNot rest of
    (Just expr_d, Operator And:rest_d) -> astAnd (addExpr And expr expr_d) rest_d
    (Just expr_d, rest_d) -> (addExpr And expr expr_d, rest_d)
    other -> other

astNot :: [Token] -> (Maybe Expr, [Token])
astNot (Bang:rest) =
  case astNot rest of
    (Just expr_d, rest_d) -> (Just (Not expr_d), rest_d)
    other -> other
astNot rest = astParen rest

astParen :: [Token] -> (Maybe Expr, [Token])
astParen tokens@(LParen:rest) =
  case astXor Nothing rest of
     (Just expr_d, RParen:rest_d) -> (Just expr_d, rest_d)
     other -> (Nothing, tokens)
astParen x = astFact x

astFact :: [Token] -> (Maybe Expr, [Token])
astFact (Letter f:rest) = (Just (Fact f), rest)
astFact rest = (Nothing, rest)

-- Fait des tokens une expression pour les Rules
ast :: [Token] -> Either String Expr
ast = checkReturn . (astEq Nothing)

-- Fait des tokens un tableau de fact pour les Inits et Queries
extractFacts:: [Expr] -> Token -> Either String [Expr]
extractFacts acc (Letter c) = Right (Fact c:acc)
extractFacts _ tk = Left ("The token should be a letter and it's " ++ show tk)


-- Prends une liste de Tokens pour les transformer en Line dans l'ordre: Init, Query, Rule
tokensToLine (InitTk:tokens) = fmap (Init . reverse) (foldM extractFacts [] tokens)
tokensToLine (QueryTk:tokens) = fmap (Query . reverse) (foldM extractFacts [] tokens)
tokensToLine tokens = fmap Rule (ast tokens)

checkReturn (Just expr, [])                       = Right expr
checkReturn (Nothing , [Operator Imply])          = Left ("Missing relation operator")
checkReturn (Nothing , [])                        = Left ("Missing relation operator")
checkReturn (_, LParen:RParen:_)                  = Left ("Empty parentheses")
checkReturn (_, tokens@(LParen:_))                = Left ("Mismatched parenthesis")
checkReturn (_, tokens@(RParen:_))                = Left ("Unexpected closing parentheses")
checkReturn (_, faulty:_)                         = Left ("Unexpected token : " ++ show faulty)
-- les lignes vides ne doivent pas provoquer d'erreur
--checkReturn _                       = Left "Empty expression"


tokenize :: String -> Either String [Token]
tokenize str = case foldM toToken  ("", []) ( filter (/= ' ') str) of
  Right ("", res) -> Right (reverse res)
  Right ("#", res) -> Right (reverse res)
  Right (_, res) -> Left "Lexical error"
  Left error -> Left error

toToken:: (String, [Token]) -> Char -> Either String (String, [Token])
toToken ("" , acc) '<' = Right ("<", acc)
toToken ("<" , acc) '=' = Right ("<=", acc)
toToken ("<=" , acc) '>' = Right ("", Operator Eq:acc)
toToken ("" , []) '=' = Right ("", [InitTk])
toToken ("" , []) '?' = Right ("", [QueryTk])
toToken ("" , acc) '=' = Right ("=", acc)
toToken ("=" , acc) '>' = Right ("", Operator Imply:acc)
toToken ("#" , acc) _ = Right ("#", acc)
toToken ("" , acc) c
  | c == '+' = Right ("", Operator And:acc)
  | c == '|' = Right ("", Operator Or:acc)
  | c == '^' = Right ("", Operator Xor:acc)
  | c == '!' = Right ("", Bang:acc)
  | c == '(' = Right ("", LParen:acc)
  | c == ')' = Right ("", RParen:acc)
  | c == '#' = Right ("#", acc)
  | isAlpha c = Right ("", Letter c:acc)
  | otherwise = Left ("Lexical error near" ++ [c])
toToken _ c = Left ("Lexical error near " ++ [c])

parse :: String -> Either String Line
parse str = tokenize str >>= tokensToLine

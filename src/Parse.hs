module Parse
( parse
) where

import Debug.Trace
import Data.Char
import Control.Monad

data Token = Letter Char | Operator Ope | Bang | LParen | RParen
    deriving(Show)
data Ope = Xor | Or | And | Eq | Imply
    deriving(Eq, Ord)
data Expr = Grp Ope Expr Expr | Fact Char | Not Expr
    deriving(Eq)


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
    (Just _, []) -> (Nothing, [])
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
astFact (LParen:rest) = astXor Nothing rest
astFact (Letter f:rest) = (Just (Fact f), rest)
astFact rest = (Nothing, rest)

ast :: [Token] -> Either String Expr
ast tokens =
  case astXor Nothing tokens of
    (Just expr, []) -> Right expr
    (_, LParen:RParen:_) -> Left ("Empty parentheses")
    (_, tokens@(LParen:_)) -> Left ("Mismatched parenthesis")
    (_, tokens@(RParen:_)) -> Left ("Unexpected closing parentheses")
    (_, faulty:_) -> Left ("Unexpected token : " ++ show faulty)
    _ -> Left "Empty expression"

tokenize :: String -> Either String [Token]
--tokenize = mapM charToToken . filter (/= ' ')
tokenize str = fmap (reverse . snd)  (foldM toToken  ("", []) ( filter (/= ' ') str))

toToken:: (String, [Token]) -> Char -> Either String (String, [Token])


toToken ("" , acc) c
  | c == '+' = Right ("", Operator And:acc)
  | c == '|' = Right ("", Operator Or:acc)
  | c == '^' = Right ("", Operator Xor:acc)
  | c == '!' = Right ("", Bang:acc)
  | c == '(' = Right ("", LParen:acc)
  | c == ')' = Right ("", RParen:acc)
  | c == '<' = Right ("<", acc)
  | c == '=' = Right ("=", acc)
  | isAlpha c = Right ("", Letter c:acc)
  | otherwise = Left ("Lexical error near" ++ [c])
--toToken ("" , acc) '<' = Right ("<", acc)
toToken ("<" , acc) '=' = Right ("<=", acc)
toToken ("<=" , acc) '>' = Right ("", Operator Eq:acc)
--toToken ("" , acc) '=' = Right ("=", acc)
toToken ("=" , acc) '>' = Right ("", Operator Imply:acc)
toToken _ c = Left ("Lexical error near" ++ [c])

parse :: String -> Either String Expr
parse str = tokenize str >>= ast

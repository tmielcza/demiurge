module Parse
( parse
) where

import Debug.Trace
import Data.Char

data Token = Letter Char | Operator Ope | Bang | LParen | RParen
    deriving(Show)
data Ope = Xor | Or | And
    deriving(Eq, Ord)
data Expr = Grp Ope Expr Expr | Fact Char | Not Expr
    deriving(Eq)

instance Show Ope where
    show Or = "|"
    show And = "+"
    show Xor = "^"

instance Show Expr where
    show (Grp o e1 e2) = "(" ++ show e1 ++ show o ++ show e2 ++ ")"
    show (Fact c) = [c]
    show (Not expr) = "!" ++ show expr

charToToken :: Char -> Either String Token
charToToken c
    | c == '|' = Right (Operator Or)
    | c == '+'  = Right (Operator And)
    | c == '^' = Right (Operator Xor)
    | c == '!' = Right Bang
    | c == '(' = Right LParen
    | c == ')' = Right RParen
    | isAlpha c = Right (Letter c)
    | otherwise = Left $ "Lexing error near: " ++ show c

addExpr op Nothing r = Just r
addExpr op (Just l) r = Just (Grp op l r)

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
    (_, tokens@(LParen:rest)) -> Left ("Mismatched parenthesis : " ++ show tokens)
    (_, faulty:_) -> Left ("Unexpected token : " ++ show faulty)
    _ -> Left "Empty expression"

tokenize :: String -> Either String [Token]
tokenize = mapM charToToken . filter (/= ' ')

parse :: String -> Either String Expr
parse str = tokenize str >>= ast

module Parse
( parse
) where

import Debug.Trace
import Data.Char
import Test.HUnit

data Tokens = Letter Char | Operator Ope | Bang
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
    show (Grp o e1 e2) = "(" ++ (show e1) ++ (show o) ++ (show e2) ++ ")"
    show (Fact c) = [c]
    show (Not expr) = "!" ++ (show expr)

charToToken :: Char -> Either String Tokens
charToToken c
    | c == '|' = Right (Operator Or)
    | c == '+'  = Right (Operator And)
    | c == '^' = Right (Operator Xor)
    | c == '!' = Right Bang
    | isAlpha c = Right (Letter c)
    | otherwise = Left $ "Lexing error near: " ++ show c

add_expr op Nothing r = Just r
add_expr op (Just l) r = Just (Grp op l r)

ast_xor :: Maybe Expr -> [Tokens] -> (Maybe Expr, [Tokens])
ast_xor expr rest =
  case ast_or Nothing rest of
    (Just expr_d, (Operator Xor):rest_d) -> ast_xor (add_expr Xor expr expr_d) rest_d
    (Just expr_d, rest_d) -> ((add_expr Xor expr expr_d), rest_d)
    other -> other

ast_or :: Maybe Expr -> [Tokens] -> (Maybe Expr, [Tokens])
ast_or expr rest =
  case ast_and Nothing rest of
    (Just expr_d, (Operator Or):rest_d) -> ast_or (add_expr Or expr expr_d) rest_d
    (Just expr_d, rest_d) -> ((add_expr Or expr expr_d), rest_d)
    other -> other

ast_and :: Maybe Expr -> [Tokens] -> (Maybe Expr, [Tokens])
ast_and expr rest =
  case ast_not rest of
    (Just expr_d, (Operator And):rest_d) -> ast_and (add_expr And expr expr_d) rest_d
    (Just expr_d, rest_d) -> ((add_expr And expr expr_d), rest_d)
    other -> other

ast_not :: [Tokens] -> (Maybe Expr, [Tokens])
ast_not (Bang:rest) =
  case ast_not rest of
    (Just expr_d, rest_d) -> (Just (Not expr_d), rest_d)
    other -> other
ast_not rest = ast_fact rest

ast_fact :: [Tokens] -> (Maybe Expr, [Tokens])
ast_fact (Letter f:rest) = (Just (Fact f), rest)
ast_fact rest = (Nothing, rest)

ast tokens =
  case ast_xor Nothing tokens of
    (Just expr, []) -> Right expr
    (_, faulty:_) -> Left ("Unexpected token : " ++ show faulty)
    _ -> Left "Empty expression"

parse :: String -> Either String Expr
parse str =
  case mapM charToToken (filter (/= ' ') str) of
    Right tokens -> ast tokens
    Left err -> Left err

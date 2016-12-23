module Parse
( parse,
  unwrap,
  checkResult
) where

import Debug.Trace
import Data.Char
import Test.HUnit

data Tokens = Letter Char | Operator Ope
    deriving(Show)
data Ope = Xor | Or | And 
    deriving(Eq, Ord)
data Expr = Grp Ope Expr Expr | Fact Char
    deriving(Eq)

data Result a = Success a | Error String

instance Show Ope where
    show Or = "|"
    show And = "+"
    show Xor = "^"

instance Show Expr where
    show (Grp o e1 e2) = "(" ++ (show e1) ++ (show o) ++ (show e2) ++ ")"
    show (Fact c) = [c]

instance Show a => Show (Result a) where
    show (Success e) = "Success : " ++ (show e)
    show (Error err) = "Error : " ++ err

unwrap (Success a) = a
unwrap (Error str) = error str

checkResult (Success _) = True
checkResult (Error _) = False

charToToken c
    | c == '|' = Success (Operator Or)
    | c == '+'  = Success (Operator And)
    | c == '^' = Success (Operator Xor)
    | isAlpha c = Success (Letter c)
    | otherwise = Error $ "Lexing error near: " ++ show c

strToTokens :: [Tokens] -> String -> Result [Tokens]
strToTokens result (c:rest)
  | c == '|' = concat (Operator Or)
  | c == '+' = concat (Operator And)
  | c == '^' = concat (Operator Xor)
  | isAlpha c = concat (Letter c)
  | otherwise = Error $ "Lexing error near: " ++ show c
  where concat x = strToTokens (x:result) rest

strToTokens result [] = Success (reverse result)

headAst:: Maybe Expr -> [Tokens] -> Result Expr

-- if the begining expression just a fact
headAst Nothing [Letter c] = Success (Fact c)

-- At the begining there is just a token list
headAst Nothing (Letter c : Operator op : Letter b : tail)=
    case ast (Fact b) op tail of
      Success (expr_down, tail_down) -> headAst (Just (Grp op (Fact c) expr_down)) tail_down
      Error err -> Error err

-- Concatenate the right expression with the head
headAst (Just expr) (Operator op : Letter b : tail) =
    case ast (Fact b) op tail of
      Success (expr_down, tail_down) -> headAst (Just (Grp op expr expr_down)) tail_down
      Error err -> Error err

-- The end
headAst (Just expr) [] = Success expr

-- Empty expression
headAst _ [] = Error "Empty expression"

-- Unexpected token
headAst _ (token:_) = Error ("Unexpected token : " ++ (show token))

ast:: Expr -> Ope -> [Tokens] -> Result (Expr, [Tokens])

ast expr_up op_up [] = Success (expr_up, [])

ast expr_up op_up tokens@((Operator op_cur):(Letter c):remain) =
    let expr_cur = (Fact c)
    in
        if op_up >= op_cur
        then Success (expr_up, tokens)
        else
            case ast expr_cur op_cur remain of
                Success (expr_down, remain_down@((Operator op_down):tail)) ->
                    if op_down < op_cur
                    then Success ((Grp op_cur expr_up expr_down), remain_down)
                    else ast (Grp op_cur expr_up expr_down) op_up remain
                Success (expr_down, []) ->
                    Success ((Grp op_cur expr_up expr_down), [])
                Success (_, token:_) -> Error $ "Unknown error with token : " ++ show token
                error -> error

ast _ _ [token] = Error $ "Unexpected token: " ++ show token

mapResult func (head:tail) =
  case func head of
    Success elem -> case mapResult func tail of
      Success child -> Success $ elem : child
      Error err -> Error err
    Error err -> Error err

mapResult func [] = Success []

parse str =
--    case mapResult charToToken (filter (/= ' ') str) of
    case strToTokens [] $ filter (/= ' ') str of
      Success tokens -> headAst Nothing tokens
      Error err -> Error err

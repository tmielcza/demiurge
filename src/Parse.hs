module Parse
( parse
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

instance Show Ope where
    show Or = "|"
    show And = "+"
    show Xor = "^"

instance Show Expr where
    show (Grp o e1 e2) = "(" ++ (show e1) ++ (show o) ++ (show e2) ++ ")"
    show (Fact c) = [c]

charToToken :: Char -> Either String Tokens
charToToken c
    | c == '|' = Right (Operator Or)
    | c == '+'  = Right (Operator And)
    | c == '^' = Right (Operator Xor)
    | isAlpha c = Right (Letter c)
    | otherwise = Left $ "Lexing error near: " ++ show c

headAst:: Maybe Expr -> [Tokens] -> Either String Expr

-- Extract first fact (if exists)
headAst Nothing (Letter c:remain) = headAst (Just (Fact c)) remain

-- Concatenate the right expression with the head
headAst (Just expr) (Operator op : Letter b : tail) =
    case ast (Fact b) op tail of
      Right (expr_down, tail_down) -> headAst (Just (Grp op expr expr_down)) tail_down
      Left err -> Left err

-- The end
headAst (Just expr) [] = Right expr

-- Empty expression
headAst Nothing [] = Left "Empty expression"

-- Unexpected token
headAst _ (token:_) = Left ("Unexpected token : " ++ (show token))

ast:: Expr -> Ope -> [Tokens] -> Either String (Expr, [Tokens])

ast expr_up op_up [] = Right (expr_up, [])

ast expr_up op_up tokens@((Operator op_cur):(Letter c):remain) =
    let expr_cur = (Fact c)
    in
        if op_up >= op_cur
        then Right (expr_up, tokens)
        else
            case ast expr_cur op_cur remain of
                Right (expr_down, remain_down@((Operator op_down):tail)) ->
                    if op_down < op_cur
                    then Right ((Grp op_cur expr_up expr_down), remain_down)
                    else ast (Grp op_cur expr_up expr_down) op_up remain
                Right (expr_down, []) ->
                    Right ((Grp op_cur expr_up expr_down), [])
                Right (_, token:_) -> Left $ "Unknown error with token : " ++ show token
                error -> error

ast _ _ (token:_) = Left $ "Unexpected token: " ++ show token

parse str =
  case mapM charToToken (filter (/= ' ') str) of
    Right tokens -> headAst Nothing tokens
    Left err -> Left err

import Debug.Trace
import Data.Char

data Tokens = Letter Char | Operator Ope
    deriving(Show)
data Ope = Xor | Or | And 
    deriving( Eq, Ord)
data Expr = Grp Ope Expr Expr | Fact Char


instance Show Ope where
    show Or = "|"
    show And = "+"
    show Xor = "^"

instance Show Expr where
    show (Grp o e1 e2) = "(" ++ (show o) ++ (show e1) ++ (show e2) ++ ")"
    show (Fact c) = [c]

char_to_token c 
    | c == '|' = Operator Or 
    | c == '+'  = Operator And 
    | c == '^' = Operator Xor 
    | isAlpha c = Letter c
    | otherwise = error "Pauvre merde"

head_ast:: Maybe Expr -> [Tokens] -> Expr

-- if the begining expression just a fact
head_ast Nothing (Letter c:[]) = (Fact c)

-- At the begining there is just a token list
head_ast Nothing (Letter c : Operator op : Letter b : tail)=
    let (expr_down, tail_down) = ast (Fact b) op tail
    in head_ast (Just (Grp op (Fact c) expr_down)) tail_down

-- Concatenate the right expression with the head
head_ast (Just expr) (Operator op : Letter b : tail) = 
    let (expr_down, tail_down) = ast (Fact b) op tail
    in head_ast (Just (Grp op expr expr_down)) tail_down

-- The end
head_ast (Just expr) [] = expr


ast:: Expr -> Ope -> [Tokens] -> (Expr, [Tokens])

ast expr_up op_up [] = (expr_up, [])

ast expr_up op_up tokens@((Operator op_cur):(Letter c):remain) =
    let expr_cur = (Fact c)
    in
        if op_up >= op_cur
        then (expr_up, tokens)
        else
            case ast expr_cur op_cur remain of 
                (expr_down, remain_down@((Operator op_down):tail)) ->  
                    if op_down < op_cur 
                    then ((Grp op_cur expr_up expr_down), remain_down)
                    else ast (Grp op_cur expr_up expr_down) op_up remain
                (expr_down, []) -> 
                    ((Grp op_cur expr_up expr_down), [])



parse str =
    let tokens = map char_to_token (filter (\x -> x /= ' ') str) 
    in head_ast Nothing tokens

main = print 3


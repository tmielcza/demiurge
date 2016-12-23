module Parse
( parse,
  unwrap
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

headAst:: Maybe Expr -> [Tokens] -> Result Expr

-- if the begining expression just a fact
headAst Nothing (Letter c:[]) = Success (Fact c)

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

ast _ _ (token:[]) = Error $ "Unexpected token: " ++ show token

mapResult func (head:tail) =
  case func head of
    Success elem -> case mapResult func tail of
      Success child -> Success $ elem : child
      Error err -> Error err
    Error err -> Error err

mapResult func [] = Success []

parse str =
    case mapResult charToToken (filter (\x -> x /= ' ') str) of
      Success tokens -> headAst Nothing tokens
      Error err -> Error err

test1 = TestCase (assertEqual "UN TEST" "((A+B)+C)" (show (unwrap $ parse "A+B+C")))
test2 = TestCase (assertEqual "UN TEST" "(A^(B|(C+D)))" (show (unwrap $ parse "A^B|C+D")))
test3 = TestCase (assertEqual "UN TEST" "(A^((B|C)|D))" (show (unwrap $ parse "A^B|C|D")))
test4 = TestCase (assertEqual "UN TEST" "((A^(B|C))^D)" (show (unwrap $ parse "A^B|C^D")))
test5 = TestCase (assertEqual "UN TEST" "((A^(B|(C+D)))^E)" (show (unwrap $ parse "A^B|C+D^E")))
test6 = TestCase (assertEqual "UN TEST" "A" (show (unwrap $ parse "     A        ")))

test7 = TestCase (assertBool "UN TEST D'ERREUR" (not $ checkResult $ parse "+"))
test8 = TestCase (assertBool "UN TEST D'ERREUR" (not $ checkResult $ parse "AB"))
test9 = TestCase (assertBool "UN TEST D'ERREUR" (not $ checkResult $ parse "A+|"))
test10 = TestCase (assertBool "UN TEST D'ERREUR" (not $ checkResult $ parse ""))
test11 = TestCase (assertBool "UN TEST D'ERREUR" (not $ checkResult $ parse "A|B+C^"))

test12 = TestCase (assertBool "UN TEST D'ERREUR" (not $ checkResult $ parse "&"))
test13 = TestCase (assertBool "UN TEST D'ERREUR" (not $ checkResult $ parse "1"))
test14 = TestCase (assertBool "UN TEST D'ERREUR" (not $ checkResult $ parse ""))

tests = TestList [TestLabel "test1" test1,
                  TestLabel "test2" test2,
                  TestLabel "test3" test3,
                  TestLabel "test4" test4,
                  TestLabel "test5" test5,
                  TestLabel "test6" test6,
                  TestLabel "test7" test7,
                  TestLabel "test8" test8,
                  TestLabel "test9" test9,
                  TestLabel "test10" test10,
                  TestLabel "test11" test11,
                  TestLabel "test12" test12,
                  TestLabel "test13" test13,
                  TestLabel "test14" test14
                 ]

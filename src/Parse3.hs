
import Text.ParserCombinators.ReadP
import Data.Char(isLetter)
import Control.Applicative

data Ope = Xor | Or | And | Eq | Imply
    deriving(Eq, Ord)
data Expr = Grp Ope Expr Expr | Fact String | Not Expr
    deriving(Eq)

instance Show Ope where
    show Or = "|"
    show And = "+"
    show Xor = "^"
    show Eq = "<=>"
    show Imply = "=>"

instance Show Expr where
    show (Grp o e1 e2) = "(" ++ show e1 ++ show o ++ show e2 ++ ")"
    show (Fact c) = c
    show (Not expr) = "!" ++ show expr
{-
expr        ::=     xorBlock EOF | xorBlock '\n'
xorBlock    ::=     xorBlock '^' orBlock | orBlock
orBlock     ::=     orBlock '^' andBlock | andBlock
andBlock    ::=     andBlock '+' factor | factor
factor      ::=     fact | '(' xorBlock ')' | '!' factor
fact        ::=     fact | letter
letter      ::=     'a' - 'z' | 'A' - 'Z'
-}

expr = do { x <- xorBlock; eof; return x} +++ do {x <- xorBlock; satisfy (== '\n'); return x}
xorBlock = orBlock `chainl1` xorOp
orBlock = andBlock `chainl1` orOp
andBlock = factor `chainl1` andOp
factor = fact +++ do { char '(' ; x <- xorBlock ; char ')'; return x} +++ do { char '!' ; x <- factor; return (Not x)}
fact = do {x <- many1 (satisfy (isLetter)); return (Fact x)}

xorOp = do { char '^'; return (Grp Xor) }
orOp = do { char '|'; return (Grp Or) }
andOp = do { char '+'; return (Grp And) }

parse s = case readP_to_S expr s of
  (x, _):_ -> Right x
  _ -> Left "Error"

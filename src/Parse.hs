module Parse
( parse,
  program,
  expr
    ) where

import Text.ParserCombinators.ReadP
import Data.Char(isLetter, isSpace)
import Control.Monad

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
program         ::=     expr EOF
relationList    ::=     {relationList newlineList} , relation [newlineList]
relation        ::=     expr '=>' expr | expr '<=>' expr
expr            ::=     expr '^' orBlock | orBlock
orBlock         ::=     orBlock '|' andBlock | andBlock
andBlock        ::=     andBlock '+' factorBlanks | factorBlanks
factorBlanks    ::=     whitespaces factor whitespaces
factor          ::=     fact | '(' expr ')' | '!' factor
fact            ::=     fact | letter
letter          ::=     'a' - 'z' | 'A' - 'Z'
newlineList     ::=     newlineList | '\n'
whitespaces     ::=     blanks |
blanks          ::=     blanks | ' ' | '\t'
-}

program = do { x <- relationList; eof; return x }
relationList = do { x <- relation `sepBy` newlineList; optional newlineList; return x}
relation = do {x <- expr; op <- relationOp; y <- expr; return (op x y)}
expr =  orBlock `chainl1` xorOp
orBlock = andBlock `chainl1` orOp
andBlock = factorBlanks `chainl1` andOp
factorBlanks = do { skipSpaces; x <- factor; skipSpaces; return x }
factor = fact +++ do { char '(' ; x <- expr ; char ')'; return x} +++ do { char '!' ; x <- factor; return (Not x)}
fact = do {x <- many1 (satisfy (isLetter)); return (Fact x)}
newlineList = skipMany1 (char '\n')

relationOp = do { string "=>"; return (Grp Imply) } +++ do { string "<=>"; return (Grp Eq) }
xorOp = do { char '^'; return (Grp Xor) }
orOp = do { char '|'; return (Grp Or) }
andOp = do { char '+'; return (Grp And) }

parse s = case readP_to_S expr s of
  (x, _):_ -> Right x
  _ -> Left "Error"

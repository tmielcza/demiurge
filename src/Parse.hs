module Parse
( parse,
  program,
  expr
    ) where

import Text.ParserCombinators.ReadP
import Data.Char(isLetter, isSpace)
import Control.Monad

data Expr = Xor Expr Expr |
            Or Expr Expr |
            And Expr Expr |
            Fact String |
            Not Expr

data Relation = Eq Expr Expr | Imply Expr Expr

newtype Init = Init [String]

newtype Query = Query [String]

instance Show Expr where
    show (Xor e1 e2) = "(" ++ show e1 ++ "^" ++ show e2 ++ ")"
    show (Or e1 e2) = "(" ++ show e1 ++ "|" ++ show e2 ++ ")"
    show (And e1 e2) = "(" ++ show e1 ++ "+" ++ show e2 ++ ")"
    show (Fact c) = c
    show (Not e) = "!" ++ show e

instance Show Relation where
    show (Imply e1 e2) = "(" ++ show e1 ++ "=>" ++ show e2 ++ ")"
    show (Eq e1 e2) = "(" ++ show e1 ++ "<=>" ++ show e2 ++ ")"

instance Show Init where
    show (Init facts) = "Init: "++ show facts

instance Show Query where
    show (Query facts) = "Query: "++ show facts

{-
program         =   whitespaces, [relations], newlines, [initFacts], newlines, [query], EOF
relations       =   relation, {newlines, relation}
relation        =   expr, ("=>" | "<=>"), expr
initFacts       =   '=', whitespaces, fact, {whitespaces, fact}
queries         =   '?', whitespaces, fact, {whitespaces, fact}
expr            =   {orBlock, '+'}, orBlock
orBlock         =   {andBlock, '+'}, andBlock
andBlock        =   {factor, '+'}, factor
factor          =   whitespaces, {'!'}, (fact | '(', expr, ')'), whitespaces
fact            =   upCaseLetter, {downcaseletter | '_'}
upCaseLetter    =   ('A' - 'Z')
downCaseLetter  =   ('a' - 'z')
newlines        =   {endOfLine, whitespaces}, endOfLine
endOfLine       =   [comment], '\n'
whitespaces     =   {' ' | '\t'}
comment         =   '#', {anyCharExceptNewline}
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

relationOp = do { string "=>"; return (Imply) } +++ do { string "<=>"; return (Eq) }
xorOp = do { char '^'; return (Xor) }
orOp = do { char '|'; return (Or) }
andOp = do { char '+'; return (And) }

parse s = case readP_to_S expr s of
  (x, _):_ -> Right x
  _ -> Left "Error"

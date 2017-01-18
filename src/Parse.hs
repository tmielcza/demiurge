module Parse
( parse,
  program,
  relation,
  initFacts,
  queryFacts,
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

newtype Init = Init [Expr] -- ce sont des expressions pas des strings qui seront renvoyés et un init peut etre a not

newtype Query = Query [Expr]

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
  program         =   whitespaces, newlines, [relations, newlines, initFacts, newlines, query, [newlines]] EOF
  relations       =   relation, {newlines, relation}
  relation        =   expr, ("=>" | "<=>"), expr
  initFacts       =   '=', {whitespaces, binaryFact}
  queries         =   '?', whitespaces, fact, {whitespaces, fact}
  expr            =   {orBlock, '+'}, orBlock
  orBlock         =   {andBlock, '+'}, andBlock
  andBlock        =   {factor, '+'}, factor
  factor          =   whitespaces, {'!'}, (fact | '(', expr, ')'), whitespaces
  binaryFact      =   whitespaces, {'!'}, fact, whitespaces
  fact            =   upCaseLetter, {downcaseletter | '_'}
  upCaseLetter    =   ('A' - 'Z')
  downCaseLetter  =   ('a' - 'z')
  newlines        =   {endOfLine, whitespaces}, endOfLine
  endOfLine       =   [comment], '\n'
  comment         =   whitespaces, '#', {anyCharExceptNewline}
  whitespaces     =   {' ' | '\t'}
-}


program = do { rules <- relationList; newlineList; facts <- initFacts; newlineList;  query <- queryFacts; newlineList; eof;  return (rules, facts, query) }
relationList = do { x <- relation `sepBy` newlineList; optional newlineList; return x}
relation = do {x <- expr; op <- relationOp; y <- expr; return (op x y)}
expr =  orBlock `chainl1` xorOp
orBlock = andBlock `chainl1` orOp
andBlock = factorBlanks `chainl1` andOp
factorBlanks = do { skipSpaces; x <- factor; skipSpaces; return x }
initFacts = do {char '=';skipSpaces; x <- (binaryFact `sepBy1` (char ' ')) +++ (return []); return (Init x)}
queryFacts = do {char '?';skipSpaces; x <- fact `sepBy1` (char ' ') ; return (Query x)}
binaryFact = fact +++ do { char '!' ; x <- fact; return (Not x)}
factor = fact +++ do { char '(' ; x <- expr ; char ')'; return x} +++ do { char '!' ; x <- factor; return (Not x)}
fact = do {x <- many1 (satisfy (isLetter)); return (Fact x)}
relationOp = do { string "=>"; return (Imply) } +++ do { string "<=>"; return (Eq) }
xorOp = do { char '^'; return (Xor) }
orOp = do { char '|'; return (Or) }
andOp = do { char '+'; return (And) }
newlineList = skipMany1(endOfLine)
endOfLine = do {optional comment; char '\n'; return ()}
comment = do {skipSpaces; char '#'; munch(/= '\n'); return ()}

parse s = case readP_to_S expr s of
  (x, _):_ -> Right x
  _ -> Left "Error"

module Parse
( parse,
  program,
  relation,
  relationList,
  initFacts,
  queryFacts,
  expr
    ) where

import Text.ParserCombinators.ReadP
import Data.Char(isLower, isUpper)
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

program = do {optional newlineList; rules <- relationList; newlineList; facts <- initFacts; newlineList;  query <- queryFacts; optional newlineList; eof;  return (rules, facts, query) }
relationList = do { x <- relation `sepBy` newlineList; optional comment; optional newlineList; return x}
relation = do {x <- expr; op <- relationOp; y <- expr; return (op x y)}
expr =  orBlock `chainl1` xorOp
orBlock = andBlock `chainl1` orOp
andBlock = factorBlanks `chainl1` andOp
factorBlanks = do { spaces; x <- factor; spaces; return x }
initFacts = do {char '='; spaces; x <- binaryFact `sepBy` spaces; return (Init x)}
queryFacts = do {char '?'; spaces; x <- fact `sepBy1` spaces ; return (Query x)}
binaryFact = fact +++ do { char '!' ; x <- fact; return (Not x)}
factor = fact +++ do { char '(' ; x <- expr ; char ')'; return x} +++ do { char '!' ; x <- factor; return (Not x)}
fact = do {y <- satisfy isUpper; x <- many (satisfy isLower +++ char '_'); return (Fact (y:x))}
relationOp = do { string "=>"; return (Imply) } +++ do { string "<=>"; return (Eq) }
xorOp = do { char '^'; return (Xor) }
orOp = do { char '|'; return (Or) }
andOp = do { char '+'; return (And) }
newlineList = skipMany1(endOfLine)
endOfLine = do {spaces; optional comment ; char '\n'; return ()}
comment = do {char '#'; munch(/= '\n'); return ()}
spaces = do {munch (\c -> c == ' ' || c == '\t'); return ()}

parse s = case readP_to_S program s of
  (x, _):_ -> Right x
  _ -> Left "Error"

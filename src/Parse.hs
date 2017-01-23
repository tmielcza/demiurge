module Parse
( parse,
  program,
  relation,
  relationList,
  initFacts,
  queryFacts,
  expr
    ) where

import Types
import Text.ParserCombinators.ReadP
import Data.Char(isLower, isUpper)
import Control.Monad

{-
  program         =   whitespaces, newlines, [relations, newlines, initFacts, newlines, query, [newlines]] EOF
  relations       =   relation, {newlines, relation}, [comment]
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

program = do {optional newlineList; rules <- relationList; newlineList; facts <- initFacts; newlineList;  query <- queryFacts; optional comment; optional newlineList; eof;  return (rules, facts, query) }
relationList = do { x <- relation `sepBy1` newlineList; return x}
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

-----------------------------------------------------------------------------
-- |
-- Module      :  Parse
-- author      : cdannapp & tmielcza
--
-- Function used for parsing. Use the ReadP Monad.
--
-- @
-- Here is our grammar in the Extended Backus-Naur Form (EBNF):
--  program         =   whitespaces, newlines, [relations, newlines, initFacts, newlines, query, [newlines]] EOF
--  relations       =   relation, {newlines, relation}, [comment]
--  relation        =   expr, ("=>" | "<=>"), expr
--  initFacts       =   '=', {whitespaces, binaryFact}
--  queries         =   '?', whitespaces, fact, {whitespaces, fact}
--  expr            =   {orBlock, '+'}, orBlock
--  orBlock         =   {andBlock, '+'}, andBlock
--  andBlock        =   {factor, '+'}, factor
--  factor          =   whitespaces, {'!'}, (fact | '(', expr, ')'), whitespaces
--  binaryFact      =   whitespaces, {'!'}, fact, whitespaces
--  fact            =   upCaseLetter, {downcaseletter | '_'}
--  upCaseLetter    =   ('A' - 'Z')
--  downCaseLetter  =   ('a' - 'z')
--  newlines        =   {endOfLine, whitespaces}, endOfLine
--  endOfLine       =   [comment], '\n'
--  comment         =   whitespaces, '#', {anyCharExceptNewline}
--  whitespaces     =   {' ' | '\t'}
-- @
-----------------------------------------------------------------------------
module Parse
( parse,
  program,
  relation,
  relationList,
  initFacts,
  queryFacts,
  expr,
  parseInit,
  parseQuery
    ) where


import Types
import Text.ParserCombinators.ReadP
import Data.Char(isLower, isUpper)
import Control.Monad

-- | Parser of the entire File, in case of success it forms a tuple containing
-- a list of rules, a list of init exprs, and a list of requested facts
program :: ReadP ([Relation], Init, Query)
program = do {optional newlineList; rules <- relationList; newlineList; facts <- initFacts; newlineList;  query <- queryFacts; optional comment; optional newlineList; eof;  return (rules, facts, query) }

-- | Parser for the group of rules
relationList :: ReadP [Relation]
relationList = relation `sepBy1` newlineList

-- | Parser for a rule
relation :: ReadP Relation
relation = do {x <- expr; op <- relationOp; y <- expr; return (op x y)}

-- | Parser of expression.First check the xor, then the or, then the and expression.
expr :: ReadP Expr
expr =  orBlock `chainl1` xorOp

-- | Parser of expression. If it contain a or it return an Expr Or
orBlock :: ReadP Expr
orBlock = andBlock `chainl1` orOp

-- | Parser of expression. If it contain a and it return an Expr Xor
andBlock :: ReadP Expr
andBlock = factorBlanks `chainl1` andOp

-- | Parser for Expression that form Init Fact
initFacts :: ReadP Init
initFacts = do {char '='; spaces; x <- binaryFact `sepBy` spaces; return (map exprToFactState x)}

-- | Parser for Expression that form Query
queryFacts :: ReadP Query
queryFacts = do {char '?'; spaces; fact `sepBy1` spaces}

-- | Parser for fact or not fact
binaryFact :: ReadP Expr
binaryFact = fact +++ do { char '!' ; x <- fact; return (Not x)}

-- | Parser for fact or parenthesis between spaces
factorBlanks :: ReadP Expr
factorBlanks = do { spaces; x <- factor; spaces; return x }

-- | Parser for fact or parenthesis content
factor :: ReadP Expr
factor = fact +++ do { char '(' ; x <- expr ; char ')'; return x} +++ do { char '!' ; x <- factor; return (Not x)}

-- | Parser for Fact
fact :: ReadP Expr
fact = do {y <- satisfy isUpper; x <- many (satisfy isLower +++ char '_'); return (Fact (y:x))}

-- | Parser of relation operator, define if it's an Implication or an Equivalence
relationOp :: ReadP (Expr -> Expr -> Relation)
relationOp = do { string "=>"; return Imply } +++ do { string "<=>"; return Eq }

-- | Consume the '^' to return the constructor Xor
xorOp :: ReadP (Expr -> Expr -> Expr)
xorOp = do { char '^'; return Xor }

-- | Consume the '|' to return the constructor Or
orOp :: ReadP (Expr -> Expr -> Expr)
orOp = do { char '|'; return Or }

-- | Consume the '+' to return the constructor And
andOp :: ReadP (Expr -> Expr -> Expr)
andOp = do { char '+'; return And }

-- | Consume several lines containing comments or nothing
newlineList :: ReadP ()
newlineList = skipMany1 endOfLine

-- | Consume the line until its end '\n' and an optional comments befor
endOfLine :: ReadP ()
endOfLine = do {spaces; optional comment ; char '\n'; return ()}

-- | Consume comment that means a '#' and every character until '\n'
comment :: ReadP ()
comment = do {char '#'; munch(/= '\n'); return ()}

-- | Consume tabulation or space
spaces :: ReadP ()
spaces = do {munch (\c -> c == ' ' || c == '\t'); return ()}

extractFromReadP :: ReadP a -> String -> Either String a
extractFromReadP readP s =
  let parsed = [x | (x, "") <- readP_to_S readP s]
  in case parsed of
  x:_ -> Right x
  _ -> Left "Parse Error"

-- | Send the string to the parser `program` and check if the return is an error
parse :: String -> Either String ([Relation], Init, Query)
parse = extractFromReadP program

-- | Parse the String and return a Init if initFacts returns Right. Used in the interactive mode.
parseInit :: String -> Either String Init
parseInit = extractFromReadP initFacts

-- | Parse the String and return a Init if queryFacts returns Right. Used in the interactive mode.
parseQuery :: String -> Either String Query
parseQuery = extractFromReadP queryFacts

module Parse
( parse,
  checkReturn,
  astXor
) where

import Debug.Trace
import Control.Monad
import Lexing


--    deriving(Show)
data Ope = Xor | Or | And | Eq | Imply
    deriving(Eq, Ord)
data Expr = Grp Ope Expr Expr | Fact Char | Not Expr
    deriving(Eq)
data Line = Rule Expr | Init [Expr] | Query [Expr]

instance Show Line where
    show (Rule exp) = show exp
    show (Init facts) = "Init" ++ show facts
    show (Query facts) = "Query" ++ show facts

instance Show Ope where
    show Or = "|"
    show And = "+"
    show Xor = "^"
    show Eq = "<=>"
    show Imply = "=>"

instance Show Expr where
    show (Grp o e1 e2) = "(" ++ show e1 ++ show o ++ show e2 ++ ")"
    show (Fact c) = [c]
    show (Not expr) = "!" ++ show expr



addExpr :: Ope -> Maybe Expr -> Expr -> Maybe Expr
addExpr _ Nothing r = Just r
addExpr op (Just l) r = Just (Grp op l r)

astEq :: [Token] -> (Maybe Expr, [Token])
astEq tokens =
  case astXor tokens of
    (Just expr_d, DArrow:rest_d) -> case astXor rest_d of
      (Just expr_d2, rest_d) -> (Just (Grp Eq expr_d expr_d2), rest_d)
      other -> other
    (Just expr_d, Arrow:rest_d) -> case astXor rest_d of
      (Just expr_d2, rest_d) -> (Just (Grp Imply expr_d expr_d2), rest_d)
      other -> other
    (Just _, []) -> (Nothing, [Arrow])
    other -> other

astXor :: [Token] -> (Maybe Expr, [Token])
astXor tokens =
  let loop expr rest = case astOr rest of
        (Just expr_d, Caret:rest_d) -> loop (addExpr Xor expr expr_d) rest_d
        (Just expr_d, rest_d) -> (addExpr Xor expr expr_d, rest_d)
        other -> other
  in loop Nothing tokens

astOr :: [Token] -> (Maybe Expr, [Token])
astOr tokens =
  let loop expr rest = case astAnd rest of
        (Just expr_d, Pipe:rest_d) -> loop (addExpr Or expr expr_d) rest_d
        (Just expr_d, rest_d) -> (addExpr Or expr expr_d, rest_d)
        other -> other
  in loop Nothing tokens

astAnd :: [Token] -> (Maybe Expr, [Token])
astAnd tokens =
  let loop expr rest = case astNot rest of
        (Just expr_d, Plus:rest_d) -> loop (addExpr And expr expr_d) rest_d
        (Just expr_d, rest_d) -> (addExpr And expr expr_d, rest_d)
        other -> other
  in loop Nothing tokens

astNot :: [Token] -> (Maybe Expr, [Token])
astNot (Bang:rest) =
  case astNot rest of
    (Just expr_d, rest_d) -> (Just (Not expr_d), rest_d)
    other -> other
astNot rest = astParen rest

astParen :: [Token] -> (Maybe Expr, [Token])
astParen tokens@(LParen:rest) =
  case astXor rest of
     (Just expr_d, RParen:rest_d) -> (Just expr_d, rest_d)
     _ -> (Nothing, tokens)
astParen x = astFact x

astFact :: [Token] -> (Maybe Expr, [Token])
astFact (Letter f:rest) = (Just (Fact f), rest)
astFact rest = (Nothing, rest)

-- Fait des tokens une expression pour les Rules
ast :: [Token] -> Either String Expr
ast = checkReturn . astEq

-- Fait des tokens un tableau de fact pour les Inits et Queries
extractFacts:: [Expr] -> Token -> Either String [Expr]
extractFacts acc (Letter c) = Right (Fact c:acc)
extractFacts _ tk = Left ("The token should be a letter and it's " ++ show tk)


-- Prends une liste de Tokens pour les transformer en Line dans l'ordre: Init, Query, Rule
tokensToLine :: [Token] -> Either String Line
tokensToLine (InitTk:tokens) = fmap (Init . reverse) (foldM extractFacts [] tokens)
tokensToLine (QueryTk:tokens) = fmap (Query . reverse) (foldM extractFacts [] tokens)
tokensToLine tokens = fmap Rule (ast tokens)


checkReturn :: (Maybe Expr, [Token]) -> Either String Expr
checkReturn (Just expr, [])         = Right expr
checkReturn (Nothing , [Arrow])     = Left ("Missing relation operator")
checkReturn (Nothing, [])           = Left "Unexpected end of line"
checkReturn (_, LParen:RParen:_)    = Left ("Empty parentheses")
checkReturn (_, LParen:_)           = Left ("Mismatched parenthesis")
checkReturn (_, RParen:_)           = Left ("Unexpected closing parentheses")
checkReturn (_, faulty:_)           = Left ("Unexpected token : " ++ show faulty)
-- les lignes vides ne doivent pas provoquer d'erreur



parse :: String -> Either String Line
parse str = tokenize str >>= tokensToLine



module Lexing
(
  tokenize,
  Token(Letter, Bang, LParen, RParen, InitTk, QueryTk, Caret, Pipe, Plus, Arrow, DArrow)
) where

import Control.Monad
import Data.Char


data Token = Letter Char
            | Bang
            | LParen
            | RParen
            | InitTk
            | QueryTk
            | Caret -- ^
            | Pipe -- |
            | Plus
            | Arrow -- =>
            | DArrow -- <=>

instance Show Token where
    show (Letter c) = [c]
    show Bang = "!"
    show LParen = "("
    show RParen = ")"
    show InitTk = "="
    show QueryTk = "?"
    show Caret = "^"
    show Pipe = "|"
    show Plus = "+"
    show Arrow = "=>"
    show DArrow = "<=>"

    showList xs = (++ "[" ++ showListTokens xs "" ++ "]")

showListTokens (x:xs) str = showListTokens xs (str ++ show x)
showListTokens [] str = str

tokenize :: String -> Either String [Token]
tokenize str = case foldM toToken  ("", []) (str) of
  Right ("", res) -> Right (reverse res)
  Right ("#", res) -> Right (reverse res)
  Right ("=", []) -> Right [InitTk]
  Right (_, res) -> Left "Lexical error"
  Left error -> Left error

toToken:: (String, [Token]) -> Char -> Either String (String, [Token])
toToken(_, acc) ' ' = Right("", acc)
toToken ("" , acc) '<' = Right ("<", acc)
toToken ("<" , acc) '=' = Right ("<=", acc)
toToken ("<=" , acc) '>' = Right ("", DArrow:acc)
toToken ("" , []) '?' = Right ("", [QueryTk])
toToken ("" , acc) '=' = Right ("=", acc)
toToken ("=" , acc) '>' = Right ("", Arrow:acc)
toToken ("=" , []) c = toToken ("", [InitTk]) c
toToken ("#" , acc) _ = Right ("#", acc)
toToken ("" , acc) c
  | c == '+' = Right ("", Plus:acc)
  | c == '|' = Right ("", Pipe:acc)
  | c == '^' = Right ("", Caret:acc)
  | c == '!' = Right ("", Bang:acc)
  | c == '(' = Right ("", LParen:acc)
  | c == ')' = Right ("", RParen:acc)
  | c == '#' = Right ("#", acc)
  | isAlpha c = Right ("", Letter c:acc)
  | otherwise = Left ("Lexical error near" ++ [c])
toToken _ c = Left ("Lexical error near " ++ [c])

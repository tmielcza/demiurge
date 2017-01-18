module Parse.Test where

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import Parse
import Lexing
import Data.Either.Unwrap
import Text.ParserCombinators.ReadP

exhaustivePattern f str = [x | (x, "") <- readP_to_S f str]


parseTest str expect =  (testCase str . assertEqual "" expect . show . head . fst . head . (readP_to_S program)) str
parseErrorTest str = (testCase str . assertBool "Must fail" . null . (readP_to_S program)) str

queryTest str expect = (testCase str . assertEqual "" expect . show . head . (exhaustivePattern queryFacts)) str
queryErrorTest str = (testCase str . assertBool "Must fail" . null . (exhaustivePattern queryFacts)) str

initTest str expect = (testCase str . assertEqual "" expect . show . head . (exhaustivePattern initFacts)) str
initErrorTest str = (testCase str . assertBool "Must fail" . null . (exhaustivePattern initFacts)) str

relationTest str expect = (testCase str . assertEqual "" expect . show . head . (exhaustivePattern relation)) str
relationErrorTest str = (testCase str . assertBool "Must fail" . null . (exhaustivePattern relation)) str

exprTest str expect = (testCase str . assertEqual "" expect . show . head . (exhaustivePattern expr)) str
exprErrorTest str = (testCase str . assertBool "Must fail" . null . (exhaustivePattern expr)) str

parseSuite = testGroup "Parsing tests"
             [
               testGroup "Expression Tests"
               [
                 exprTest "A+B+C" "((A+B)+C)",
                 exprTest "A^B|C+D" "(A^(B|(C+D)))",
                 exprTest "A^B|C|D" "(A^((B|C)|D))",
                 exprTest "A^B|C^D" "((A^(B|C))^D)",
                 exprTest "A^B|C+D^E" "((A^(B|(C+D)))^E)",
                 exprErrorTest "+",
                 exprErrorTest "A B",
                 exprErrorTest "A+|",
                 exprErrorTest "A|B+C^",
                 exprErrorTest "A|A||"
               ],
               testGroup "Not Tests"
               [
                 exprTest "!A" "!A",
                 exprTest "!A+!B" "(!A+!B)",
                 exprTest "!!!!!!A^!!!!!B" "(!!!!!!A^!!!!!B)",
                 exprErrorTest "!",
                 exprErrorTest "!!!!"
               ],
               testGroup "Parentheses Tests"
               [
                 exprTest "C | (D + Y)" "(C|(D+Y))" ,
                 exprTest "C ^ !(R | I) + P" "(C^(!(R|I)+P))" ,
                 exprTest "C + !(R | I) ^ P" "((C+!(R|I))^P)" ,
                 exprTest "C + !(R | (V ^ O)) ^ P" "((C+!(R|(V^O)))^P)" ,
                 exprTest "((((((U))))+(((((((F)))))))))" "(U+F)",
                 exprTest "(U)" "U",
                 exprErrorTest "(A + B",
                 exprErrorTest "(A + B +)",
                 exprErrorTest "(A + B ))",
                 exprErrorTest "()",
                 exprErrorTest "(((("
               ],
               testGroup "Relations Tests"
               [
                 relationTest "A => B" "(A=>B)",
                 relationTest "A <=> B" "(A<=>B)",
                 relationTest "A + C <=> B" "((A+C)<=>B)",
                 relationTest "A ^ C <=> B | D" "((A^C)<=>(B|D))",
                 relationTest "A + (C | P) <=> B" "((A+(C|P))<=>B)",
                 relationErrorTest "A => + B",
                 relationErrorTest "A + => B",
                 relationErrorTest "A => B => C",
                 relationErrorTest "(B => C)"
               ],
               testGroup "Queries Tests"
               [
                 initTest "=ABC D" "Init: [ABC,D]",
                 initTest "= A !B C" "Init: [A,!B,C]",
                 initTest "=" "Init: []",
                 initErrorTest "=+",
                 initErrorTest "=ABC+R",
                 initErrorTest "=A(BCR)"
               ],
               testGroup "Initial Facts Tests"
               [
                 queryTest "?P O N" "Query: [P,O,N]",
                 queryTest "? P O N" "Query: [P,O,N]",
                 queryErrorTest "?|",
                 queryErrorTest "?@",
                 queryErrorTest "? A B C + R",
                 queryErrorTest "? ABC+R",
                 queryErrorTest "? A B C !R"
               ]
             ]

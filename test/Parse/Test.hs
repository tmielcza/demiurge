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

exprTest str expect = (testCase str . assertEqual "" expect . show . head . (exhaustivePattern expr)) str
exprErrorTest str = (testCase str . assertBool "Must fail" . null . (exhaustivePattern expr)) str

-- tokenizeTest str expect = (testCase str . assertEqual "" expect . show . fromRight . tokenize) str
-- tokenizeErrorTest str = (testCase str . assertBool "Must fail" . isLeft . tokenize) str

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
                 parseTest "A => B" "(A=>B)",
                 parseTest "A <=> B" "(A<=>B)",
                 parseTest "A + C <=> B" "((A+C)<=>B)",
                 parseTest "A ^ C <=> B | D" "((A^C)<=>(B|D))",
                 parseTest "A + (C | P) <=> B" "((A+(C|P))<=>B)",
                 parseErrorTest "A => + B",
                 parseErrorTest "A + => B",
                 parseErrorTest "A => B => C",
                 parseErrorTest "(B => C)"
               ],
               testGroup "Queries Tests"
               [
                 parseTest "=ABC" "Init[A,B,C]",
                 parseTest "=" "Init[]",
                 parseErrorTest "=+",
                 parseErrorTest "=ABC+R",
                 parseErrorTest "=A(BCR)"
                 -- les Query peuvent-elles etre empty?
               ],
               testGroup "Initial Facts Tests"
               [
                 parseTest "?PON" "Query[P,O,N]",
                 parseErrorTest "?|",
                 parseErrorTest "?@",
                 parseErrorTest "?ABC+R"
               ]
             ]

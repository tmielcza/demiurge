module Parse.Test where

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import Parse
import Data.Either.Unwrap
import Text.ParserCombinators.ReadP

exhaustivePattern f str = [x | (x, "") <- readP_to_S f str]

parseFileTest file expect=
     let fileAssertion = do
          content <- readFile ("samples/" ++ file)
          assertEqual "" expect ((show . fromRight . parse) content)
     in testCase file fileAssertion
--          Right ([((A+C)=>B)],Init: [A,C],Query: [B])




parseTest str expect = (testCase str . assertEqual "" expect . show . fromRight . parse) str
parseTestError str = (testCase str . assertBool "Must fail" . isLeft . parse) str

queryTest str expect = (testCase str . assertEqual "" expect . show . head . (exhaustivePattern queryFacts)) str
queryErrorTest str = (testCase str . assertBool "Must fail" . null . (exhaustivePattern queryFacts)) str

initTest str expect = (testCase str . assertEqual "" expect . show . head . (exhaustivePattern initFacts)) str
initErrorTest str = (testCase str . assertBool "Must fail" . null . (exhaustivePattern initFacts)) str

relationListTest str expect = (testCase str . assertEqual "" expect . show . head . (exhaustivePattern relationList)) str
relationListErrorTest str = (testCase str . assertBool "Must fail" . null . (exhaustivePattern relationList)) str

relationTest str expect = (testCase str . assertEqual "" expect . show . head . (exhaustivePattern relation)) str
relationErrorTest str = (testCase str . assertBool "Must fail" . null . (exhaustivePattern relation)) str

exprTest str expect = (testCase str . assertEqual "" expect . show . head . (exhaustivePattern expr)) str
exprErrorTest str = (testCase str . assertBool "Must fail" . null . (exhaustivePattern expr)) str

parseSuite = testGroup "Parsing tests"
             [
               testGroup "Expression"
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
               testGroup "Not"
               [
                 exprTest "!A" "!A",
                 exprTest "!A+!B" "(!A+!B)",
                 exprTest "!!!!!!A^!!!!!B" "(!!!!!!A^!!!!!B)",
                 exprErrorTest "!",
                 exprErrorTest "!!!!"
               ],
               testGroup "Parentheses"
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
               testGroup "Relations"
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
               testGroup "Blanks / Comments"
               [
                 relationListTest "A => B#CDRR\n  C=>D"  "[(A=>B),(C=>D)]",
                 relationListTest "A => B#CDRR\n"  "[(A=>B)]",
                 relationListTest "A => B  #CDRR\n#Comments\n"  "[(A=>B)]",
                 relationListTest "A => B  \n  E <=> O"  "[(A=>B),(E<=>O)]",
                 relationListTest "A => B #CDRR" "[(A=>B)]",
                 relationListErrorTest ""
               ],
               testGroup "Queries"
               [
                 initTest "=ABCD" "Init: [A,B,C,D]",
                 initTest "= A    BC D" "Init: [A,B,C,D]",
                 initTest "= A!BC" "Init: [A,!B,C]",
                 initTest "=" "Init: []",
                 initTest "=It_s_a_factThis_too" "Init: [It_s_a_fact,This_too]",
                 initErrorTest "=+",
                 initErrorTest "=ABC+R",
                 initErrorTest "=A(BCR)",
                 initErrorTest ""
               ],
               testGroup "Initial Facts"
               [
                 queryTest "?PON" "Query: [P,O,N]",
                 queryTest "?   P  O N" "Query: [P,O,N]",
                 queryTest "?Today_s_the_dayNo_means_no" "Query: [Today_s_the_day,No_means_no]",
                 queryErrorTest "?|",
                 queryErrorTest "?",
                 queryErrorTest "?@",
                 queryErrorTest "? A B C + R",
                 queryErrorTest "? ABC+R",
                 queryErrorTest "? A B C !R",
                 queryErrorTest "",
                 queryErrorTest "allyourbasearebelongtous"
               ],
               testGroup "Parser"
               [
                 parseTest "A+B=>C\n=AB\n?C\n" "([((A+B)=>C)],Init: [A,B],Query: [C])",
                 parseTest "A+B=>C\n=AB\n?C" "([((A+B)=>C)],Init: [A,B],Query: [C])",
                 parseTest "Asteques+Boisson=>Crocodile\n=AstequesBoisson\n?Crocodile\n" "([((Asteques+Boisson)=>Crocodile)],Init: [Asteques,Boisson],Query: [Crocodile])",
                 parseTest "#tactatctatct\n\nA+B=>C#youplalala\n=AB#super\n?C#commentaire\n#derniere ligne\n" "([((A+B)=>C)],Init: [A,B],Query: [C])",
                 parseTest "A+B=>C\nE <=> Q\n=AB\n?C\n" "([((A+B)=>C),(E<=>Q)],Init: [A,B],Query: [C])"
               ]
             ]

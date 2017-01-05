module Parse.Test where

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import Parse
import Data.Either.Unwrap

parseTest str expect = (testCase str . assertEqual "" expect . show . fromRight . parse) str
parseErrorTest str = (testCase str . assertBool str . isLeft . parse) str

astXorTest str expect = (testCase str . assertEqual "" expect . show . fromRight .  (>>= (checkReturn . (astXor Nothing))) . tokenize) str
astXorErrorTest str = (testCase str . assertBool str . isLeft . (>>= (checkReturn . (astXor Nothing))) . tokenize) str


parseSuite = testGroup "Parsing tests"
             [
               -- fact tests
               astXorTest "     A        " "A",
               astXorErrorTest "&",
               astXorErrorTest "1",
               astXorErrorTest "",
               -- op tests
               astXorTest "A+B+C" "((A+B)+C)",
               astXorTest "A^B|C+D" "(A^(B|(C+D)))",
               astXorTest "A^B|C|D" "(A^((B|C)|D))",
               astXorTest "A^B|C^D" "((A^(B|C))^D)",
               astXorTest "A^B|C+D^E" "((A^(B|(C+D)))^E)",
               astXorErrorTest "+",
               astXorErrorTest "AB",
               astXorErrorTest "A+|",
               astXorErrorTest "A|B+C^",
               astXorErrorTest "A|A||",
               -- not tests
               astXorTest "!A" "!A",
               astXorTest "!A+!B" "(!A+!B)",
               astXorTest "!!!!!!A^!!!!!B" "(!!!!!!A^!!!!!B)",
               astXorErrorTest "!",
               astXorErrorTest "!!!!",
               -- parentheses tests
               astXorTest "C | (D + Y)" "(C|(D+Y))" ,
               astXorTest "C ^ !(R | I) + P" "(C^(!(R|I)+P))" ,
               astXorTest "C + !(R | I) ^ P" "((C+!(R|I))^P)" ,
               astXorTest "C + !(R | (V ^ O)) ^ P" "((C+!(R|(V^O)))^P)" ,
               astXorTest "(U)" "U",
               astXorErrorTest "(A + B",
               astXorErrorTest "(A + B +)",
               astXorErrorTest "(A + B ))",
               astXorErrorTest "()",
               --Implication tests
               parseTest "A => B" "(A=>B)",
               parseTest "A <=> B" "(A<=>B)",
               parseTest "A + C <=> B" "((A+C)<=>B)",
               parseTest "A ^ C <=> B | D" "((A^C)<=>(B|D))",
               parseTest "A + (C | P) <=> B" "((A+(C|P))<=>B)",
               parseErrorTest "A => + B",
               parseErrorTest "A + => B"
               parseErrorTest "A => B => C"
               parseErrorTest "(B => C)"

             ]

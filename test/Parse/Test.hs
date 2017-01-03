module Parse.Test where

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import Parse
import Data.Either.Unwrap

parseTest str expect = (testCase expect . assertEqual "" str . show . fromRight . parse) expect

parseErrorTest str = (testCase str . assertBool str . isLeft . parse) str

parseSuite = testGroup "Parsing tests"
             [
               -- fact parsing tests
               parseTest "A" "     A        ",
               parseErrorTest "&",
               parseErrorTest "1",
               parseErrorTest "",
               -- op parsing tests
               parseTest "((A+B)+C)" "A+B+C",
               parseTest "(A^(B|(C+D)))" "A^B|C+D",
               parseTest "(A^((B|C)|D))" "A^B|C|D",
               parseTest "((A^(B|C))^D)" "A^B|C^D",
               parseTest "((A^(B|(C+D)))^E)" "A^B|C+D^E",
               parseErrorTest "+",
               parseErrorTest "AB",
               parseErrorTest "A+|",
               parseErrorTest "A|B+C^",
               parseErrorTest "A|A||",
               -- not parsing tests
               parseTest "!A" "!A",
               parseTest "(!A+!B)" "!A+!B",
               parseTest "(!!!!!!A^!!!!!B)" "!!!!!!A^!!!!!B",
               parseErrorTest "!",
               parseErrorTest "!!!!"
             ]

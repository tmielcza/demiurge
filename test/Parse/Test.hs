module Parse.Test where

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import Parse

parseTest str expect = ((testCase expect) . (assertEqual "" str) . show . unwrap . parse) expect

parseErrorTest str = ((testCase str) . (assertBool str) . not . checkResult . parse) str

parseSuite = testGroup "Parsing tests"
             [
               parseTest "((A+B)+C)" "A+B+C",
               parseTest "(A^(B|(C+D)))" "A^B|C+D",
               parseTest "(A^((B|C)|D))" "A^B|C|D",
               parseTest "((A^(B|C))^D)" "A^B|C^D",
               parseTest "((A^(B|(C+D)))^E)" "A^B|C+D^E",
               parseTest "A" "     A        ",
               parseErrorTest "+",
               parseErrorTest "AB",
               parseErrorTest "A+|",
               parseErrorTest "",
               parseErrorTest "A|B+C^",
               parseErrorTest "&",
               parseErrorTest "1",
               parseErrorTest ""
             ]

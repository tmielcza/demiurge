module Parse.Test where

import Test.HUnit
import Parse

parseSuite = TestLabel "Parsing tests"
             (TestList [TestCase (assertEqual "UN TEST" "((A+B)+C)" (show (unwrap $ parse "A+B+CD")))
                       ]
             )

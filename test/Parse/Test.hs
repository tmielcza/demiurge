module Parse.Test where

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import Parse

parseSuite = testGroup "Parsing tests"
             [testCase "Un test" (assertEqual "UN TEST" "((A+B)+C)" (show (unwrap $ parse "A+B+CD")))
             ]

module ReadAndResolve.Test where
import ReadAndResolve (readAndResolve)
-- import Prelude hiding (True, False)
import Types
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)


resolveFileTest filepath expect=
     let fileAssertion = do
          content <- readFile (filepath)
          ret <- readAndResolve filepath Prelude.False
          assertEqual filepath expect ret
     in testCase filepath fileAssertion

answer fact state =
  "The fact " ++ fact ++ " is " ++ state ++"\n"

fileSuite = testGroup "Files tests"
            [
              resolveFileTest "samples/easy" (answer "B" "True"),
              resolveFileTest "samples/error1"  "The rules {(A + C) => B} and {(A + C) => !B} have different results for the goal B",
              resolveFileTest "samples/conjunction" (answer "C" "False"),
              resolveFileTest "samples/invalid" "The rules {B => E} and {C => !E} have different results for the goal E",
              resolveFileTest "samples/subject" "The rules {(A + B) => C} and {(A + B) => !C} have different results for the goal C",
              resolveFileTest "samples/test1" (
                answer "B" "Unprovable" ++
                answer "D" "Unprovable"
              ),
              resolveFileTest "samples/test2" (answer "Y" "True"),
              resolveFileTest "samples/equivalence" (answer "C" "True"),
              resolveFileTest "samples/contradiction" (
                              answer "Enfaitcestfaux" "False" ++
                              answer "F" "True"
              ),
              resolveFileTest "samples/tautomlogie" (
                              answer "Tropvrai" "True" ++
                              answer "P" "True"
              ),
              resolveFileTest "samples/transposition" (answer "R" "True"),
              resolveFileTest "samples/unso" (answer "Reponsefaux" "False"),
              resolveFileTest "samples/inferenceRule" (
                              answer "Q" "True" ++
                              answer "R" "False" ++
                              answer "V" "True" ++
                              answer "W" "True"
              ),
              resolveFileTest "samples/simpleTest" (answer "B" "True"),
              resolveFileTest "samples/untest" (
                answer "Maybe" "False" ++
                answer "True" "True"
              ),
              resolveFileTest "samples/untest2" (answer "E" "False"),
              resolveFileTest "samples/untest3"  (
                              answer "A" "False" ++
                              answer "B" "False" ++
                              answer "C" "False" ++
                              answer "D" "False"
              ),
              resolveFileTest "samples/untest4" (
                answer "A" "True" ++
                answer "B" "Unprovable" ++
                answer "C" "Unprovable"
              ),
              resolveFileTest "samples/untest6" (
                answer "A" "False" ++
                answer "B" "False"
              ),
              resolveFileTest "samples/untest7" (
                answer "A" "False" ++
                answer "B" "False" ++
                answer "C" "False" ++
                answer "E" "False"
              ),
              resolveFileTest "samples/untest8" (
                answer "A" "True" ++
                answer "B" "Unprovable" ++
                answer "C" "Unprovable" ++
                answer "D" "Unprovable"
              ),
              resolveFileTest "samples/untest9" (
                answer "A" "Unprovable" ++
                answer "B" "Unprovable"
              ),
              resolveFileTest "samples/untest10" (answer "B" "Unprovable")
            ]

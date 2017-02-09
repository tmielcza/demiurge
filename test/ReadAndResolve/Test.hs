module ReadAndResolve.Test where
import ReadAndResolve (readAndResolve)
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)


resolveFileTest filepath expect=
     let fileAssertion = do
          content <- readFile (filepath)
          ret <- readAndResolve filepath
          assertEqual filepath expect ret
     in testCase filepath fileAssertion

fileSuite = testGroup "Files tests"
            [
              resolveFileTest "samples/easy" "Right [(B,True)]",
              resolveFileTest "samples/error1" "Left \"Incoherent rules and/or initial facts\"",
              resolveFileTest "samples/conjunction" "Right [(C,False)]",
              resolveFileTest "samples/invalid" "Left \"Incoherent rules and/or initial facts\"",
              resolveFileTest "samples/subject" "Left \"Incoherent rules and/or initial facts\"",
              resolveFileTest "samples/test1" "Right [(B,Unprovable),(D,Unprovable)]",
              resolveFileTest "samples/test2" "Right [(Y,True)]",
              resolveFileTest "samples/testAnonA" "Right [(Enfaitcestfaux,False)]",
              resolveFileTest "samples/transposition" "Right [(R,True)]",
              resolveFileTest "samples/unso" "Right [(Reponsefaux,False)]",
              resolveFileTest "samples/inferenceRule" "Right [(Q,True),(R,False),(V,True),(W,True)]",
              resolveFileTest "samples/simpleTest" "Right [(B,True)]"
            ]

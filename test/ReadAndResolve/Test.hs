module ReadAndResolve.Test where
import ReadAndResolve (readAndResolve)
import Prelude hiding (True, False)
import Types
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)


resolveFileTest filepath expect=
     let fileAssertion = do
          content <- readFile (filepath)
          ret <- readAndResolve filepath
          assertEqual filepath expect ret
     in testCase filepath fileAssertion

--fact @# state = (Fact fact, state)

fileSuite = testGroup "Files tests"
            [
              resolveFileTest "samples/easy" (Right [(Fact "B", True)]),
              resolveFileTest "samples/error1"  (Left "Incoherent rules and/or initial facts"),
              resolveFileTest "samples/conjunction" (Right [(Fact "C", False)]),
              resolveFileTest "samples/invalid" (Left "Incoherent rules and/or initial facts"),
              resolveFileTest "samples/subject" (Left "Incoherent rules and/or initial facts"),

              resolveFileTest "samples/test1" (Right [
                (Fact "B", Unprovable (Not(Not(Fact "D")) `And` (Not(Fact "B")))),
                (Fact "D", Unprovable (Not(Not(Fact "D")) `And` (Not(Fact "B"))))
                 ]),

              resolveFileTest "samples/test2" (Right [(Fact "Y", True)]),
              resolveFileTest "samples/equivalence" (Right [(Fact "A", False), (Fact "C", False), (Fact "D", True)]),
              resolveFileTest "samples/testAnonA" (Right [(Fact "Enfaitcestfaux", False)]),
              resolveFileTest "samples/transposition" (Right [(Fact "R", True)]),
              resolveFileTest "samples/unso" (Right [(Fact "Reponsefaux", False)]),
              resolveFileTest "samples/inferenceRule" (Right [
                (Fact "Q", True),
                (Fact "R", False),
                (Fact "V", True),
                (Fact "W", True)
                 ]),
              resolveFileTest "samples/simpleTest" (Right [(Fact "B", True)]),
              resolveFileTest "samples/untest" (Right [(Fact "Maybe", False), (Fact "True", True)]),
              resolveFileTest "samples/untest2" (Right [(Fact "E", False)]),
              resolveFileTest "samples/untest3"  (Right [(Fact "A", False), (Fact "B", False), (Fact "C", False), (Fact "D", False)]),
              resolveFileTest "samples/untest4" (Right [
                (Fact "A", True),
                (Fact "B", Unprovable((Not(Fact "A")) `Xor` (Not(Fact "C")))),
                (Fact "C", Unprovable((Not(Fact "A")) `Xor` (Not(Fact "B"))))
                  ]),
              resolveFileTest "samples/untest6" (Right [(Fact "A", False), (Fact "B", False)]),
              resolveFileTest "samples/untest7" (Right [(Fact "A", False), (Fact "B", False), (Fact "C", False), (Fact "E", False)]),
              resolveFileTest "samples/untest8" (Right [
                (Fact "A", True),
                (Fact "B", Unprovable (Fact "Jesaispas")),
                (Fact "C", Unprovable (Fact "aremplir")),
                (Fact "E", Unprovable (Fact "avoir"))
                ]),
              resolveFileTest "samples/untest9" (Right [(Fact "A", Unprovable (Not $ Fact "B")), (Fact "B", Unprovable (Not $ Fact "A"))])

            ]

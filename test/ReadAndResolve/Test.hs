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


fileSuite = testGroup "Files tests"
            [
              resolveFileTest "samples/easy" (Right [("B", True)]),
              resolveFileTest "samples/error1"  (Left "Incoherent rules and/or initial facts"),
              resolveFileTest "samples/conjunction" (Right [("C", False)]),
              resolveFileTest "samples/invalid" (Left "Incoherent rules and/or initial facts"),
              resolveFileTest "samples/subject" (Left "Incoherent rules and/or initial facts"),

              resolveFileTest "samples/test1" (Right [
                ("B", Unprovable (Not(Not(Fact "D")) `And` (Not(Fact "B")))),
                ("D", Unprovable (Not(Not(Fact "D")) `And` (Not(Fact "B"))))
                 ]),

              resolveFileTest "samples/test2" (Right [("Y", True)]),
              resolveFileTest "samples/equivalence" (Right [("A", False), ("C", False), ("D", True)]),
              resolveFileTest "samples/testAnonA" (Right [("Enfaitcestfaux", False)]),
              resolveFileTest "samples/transposition" (Right [("R", True)]),
              resolveFileTest "samples/unso" (Right [("Reponsefaux", False)]),
              resolveFileTest "samples/inferenceRule" (Right [
                ("Q", True),
                ("R", False),
                ("V", True),
                ("W", True)
                 ]),
              resolveFileTest "samples/simpleTest" (Right [("B", True)]),
              resolveFileTest "samples/untest" (Right [("Maybe", False), ("True", True)]),
              resolveFileTest "samples/untest2" (Right [("E", False)]),
              resolveFileTest "samples/untest3"  (Right [("A", False), ("B", False), ("C", False), ("D", False)]),
              resolveFileTest "samples/untest4" (Right [
                ("A", True),
                ("B", Unprovable((Not(Fact "A")) `Xor` (Not(Fact "C")))),
                ("C", Unprovable((Not(Fact "A")) `Xor` (Not(Fact "B"))))
                  ]),
              resolveFileTest "samples/untest6" (Right [("A", False), ("B", False)]),
              resolveFileTest "samples/untest7" (Right [("A", False), ("B", False), ("C", False), ("E", False)]),
              resolveFileTest "samples/untest8" (Right [
                ("A", True),
                ("B", Unprovable (Fact "Jesaispas")),
                ("C", Unprovable (Fact "aremplir")),
                ("E", Unprovable (Fact "avoir"))
                ]),
              resolveFileTest "samples/untest9" (Right [("A", Unprovable (Not $ Fact "B")), ("B", Unprovable (Not $ Fact "A"))])

            ]

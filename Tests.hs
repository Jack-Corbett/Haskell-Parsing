-- comp2209 Functional Programming Challenges
-- (c) University of Southampton 2018
-- DO NOT RE-DISTRIBUTE OR RE-POST
-- sample test cases by Andy Gravell, Julian Rathke

-- Import standard library and parsing definitions from Hutton 2016, Chapter 13
import           Challenges
import           Data.Char
import           Parsing


-- Main program, testing constants and functions start here
--
-- Test Suites, one per exercise
tests :: [(String, [(String, Bool)])]
tests =
  [
  ("Challenge 1",
    [ ("Test 1: convertLet(let x1 = x1 in x2) equivLam LamApp (LamAbs 1 (LamVar 2)) (LamVar 1)",
        convertLet (Let [1] (Var 1) (Var 2)) `equivLam` LamApp (LamAbs 1 (LamVar 2)) (LamVar 1)
      ),
      ("Test 2: convertLet(let x1 x2 = x2 in x1) equivLam LamApp (LamAbs 1 (LamVar 1)) (LamAbs 2 (LamVar 2))",
        convertLet (Let [1,2] (Var 2) (Var 1)) `equivLam` LamApp (LamAbs 1 (LamVar 1)) (LamAbs 2 (LamVar 2))
      ),
      ("Test 3: convertLet(let x1 x2 x3 = x3 x2 in x1 x4) equivLam LamApp (LamAbs 1 (LamApp (LamVar 1) (LamVar 4))) (LamAbs 2 (LamAbs 3 (LamApp (LamVar 3) (LamVar 2))))",
        convertLet (Let [1,2,3] (App (Var 3) (Var 2)) (App (Var 1) (Var 4))) `equivLam` LamApp (LamAbs 1 (LamApp (LamVar 1) (LamVar 4))) (LamAbs 2 (LamAbs 3 (LamApp (LamVar 3) (LamVar 2))))
      ),
      ("Test 4: convertLet(let x1 = x2 in let x3 = x4 in x1 x3) equivLam LamApp (LamAbs 1 (LamApp (LamAbs 3 (LamApp (LamVar 1) (LamVar 3))) (LamVar 4))) (LamVar 2)",
        convertLet (Let [1] (Var 2) (Let [3] (Var 4) (App (Var 1) (Var 3)))) `equivLam` LamApp (LamAbs 1 (LamApp (LamAbs 3 (LamApp (LamVar 1) (LamVar 3))) (LamVar 4))) (LamVar 2)
      )
    ]
  ),
  ("Challenge 2",
    [ ("Test 1: prettyPrint (Let [1] (Var 2) (Var 1)) equivLetString let x1 = x2 in x1",
        prettyPrint (Let [1] (Var 2) (Var 1)) `equivLetString` "let x1 = x2 in x1"
      ),
      ("Test 2: prettyPrint (Let [1,2] (Var 2) (App (Var 3) (Var 1))) equivLetString let x1 x2 = x2 in x3 x1",
        prettyPrint (Let [1,2] (Var 2) (App (Var 3) (Var 1))) `equivLetString` "let x1 x2 = x2 in x3 x1"
      ),
      ("Test 3: prettyPrint (App (Let [1,2] (Var 2) (Var 3)) (Var 1)) equivLetString (let x1 x2 = x2 in x3) x1",
        prettyPrint (App (Let [1,2] (Var 2) (Var 3)) (Var 1)) `equivLetString` "(let x1 x2 = x2 in x3) x1"
      ),
      ("Test 4: prettyPrint (App (Var 1) (App (Var 2) (Var 3))) equivLetString x1 (x2 x3)",
        prettyPrint (App (Var 1) (App (Var 2) (Var 3))) `equivLetString` "x1 (x2 x3)"
      ),
      ("Test 5: prettyPrint (App (App (Var 1) (Var 2)) (Var 3)) equivLetString x1 x2 x3",
        prettyPrint (App (App (Var 1) (Var 2)) (Var 3)) `equivLetString` "x1 x2 x3"
      )
    ]
  ),
  ("Challenge 3",
    [ ("Test 1: parseLet (let x1 = x2) == Nothing",
        (parseLet "let x1 = x2") == Nothing
      ),
      ("Test 2: parseLet (let x1 = x2 in x1) equivLet (Let [1] (Var 2) (Var 1))",
        (parseLet "let x1 = x2 in x1") `equivLet` (Let [1] (Var 2) (Var 1))
      ),
      ("Test 3: parseLet (let x1 x2 = x2 in x1 x1) equivLet (Let [1,2] (Var 2) (App (Var 1) (Var 1)))",
        (parseLet "let x1 x2 = x2 in x1 x1") `equivLet` (Let [1,2] (Var 2) (App (Var 1) (Var 1)))
      ),
      ("Test 4: parseLet (x1 (x2 x3)) equivLet App (Var 1) (App (Var 2) (Var 3))",
        (parseLet "x1 (x2 x3)") `equivLet` (App (Var 1) (App (Var 2) (Var 3)))
      ),
      ("Test 5: parseLet (x1 x2 x3) equivLet (App (App (Var 1) (Var 2)) (Var 3))",
        (parseLet "x1 x2 x3") `equivLet` (App (App (Var 1) (Var 2)) (Var 3))
      )
    ]
  ),
  ("Challenge 4",
    [ ("Test 1: countReds \\x1 (\\x2 -> x2) 0 = (Just 0, Just 0)",
        countReds (LamAbs 1 (LamAbs 2 (LamVar 2))) 0 == (Just 0, Just 0)
      ),
      ("Test 2: countReds (\\x1 -> x1)(\\x2 -> \\x2) 1 = (Just 1, Just 1)",
        countReds (LamApp (LamAbs 1 (LamVar 1)) (LamAbs 2 (LamVar 2))) 1 == (Just 1, Just 1)
      ),
      ("Test 3: countReds lambdaExpr6 10 = (Just 2, Just 3)",
        countReds lambdaExpr6 10 == (Just 2, Just 3)
      ),
      ("Test 4: countReds lambdaExpr6 2 = (Just 2, Nothing)",
        countReds lambdaExpr6 2 == (Just 2, Nothing)
      ),
      ("Test 5: countReds lambdaExpr6 1 = (Nothing, Nothing)",
        countReds lambdaExpr6 1 == (Nothing, Nothing)
      )
    ]
  ),
  ("Challenge 5",
    [ ("Test 1: compileArith (0++) == Nothing",
        compileArith "0++" == Nothing
      ),
      ("Test 2: compileArith (0) equivLam2 \\x -> \\y -> y",
       (compileArith "0") `equivLam2` (LamAbs 1 (LamAbs 2 (LamVar 2)))
      ),
      ("Test 3: compileArith (1) equivLam2 \\x -> \\y -> x y",
       (compileArith "1") `equivLam2` (LamAbs 1 (LamAbs 2 (LamApp (LamVar 1) (LamVar 2))))
      ),
      ("Test 4: compileArith (2) equivLam2 \\x -> \\y -> x x y",
       (compileArith "2") `equivLam2` (LamAbs 1 (LamAbs 2 (LamApp (LamVar 1) (LamApp (LamVar 1) (LamVar 2)))))
      ),
      ("Test 5: compileArith \"(+1)\" equivLam2 (\\x -> \\y -> x y)(\\x -> \\y -> \\z -> y (x y z))",
       (compileArith "(+1)") `equivLam2`
           (LamApp (LamAbs 1 (LamAbs 2 (LamApp (LamVar 1) (LamVar 2)))) (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamApp (LamVar 2) (LamApp (LamApp (LamVar 1) (LamVar 2)) (LamVar 3)))))))
      )
    ]
  )
  ]

-- Main program checks the results of the tests and produces scores
main :: IO ()
main =
  do
    putStr ""
    testSuite tests

testSuite :: [(String, [(String,Bool)])] -> IO ()
testSuite [] = putStr ""
testSuite ((s,tc):ts) =
  do
    putStrLn (outPrefix (s ++ ": " ++ (message tc)))
    testCases tc
    testSuite ts

testCases :: [(String,Bool)] -> IO ()
testCases [] = putStr ""
testCases ((s,False):ts) =
  do
    putStr (outPrefix "Did not satisfy assertion: ")
    putStrLn s
    testCases ts
testCases ((s,True):ts) =
  do
    testCases ts

-- Auxiliary functions to support testing and scoring
outPrefix msg = "  " ++ msg

message :: [(String,Bool)] -> String
message ts =
  let failures = [(s,b) | (s,b) <- ts , b == False] in
  if failures == [] then "all test cases passed"
  else "failed " ++ (show (length failures)) ++ " out of " ++ (show (length ts)) ++ " test cases"


-- lambda calculus expressions test values
lambdaExpr1 = LamApp (LamAbs 1 (LamVar 1)) (LamAbs 1 (LamVar 1))
lambdaExpr2 = LamApp (LamAbs 1 (LamAbs 2 (LamVar 1))) (LamApp (LamAbs 3 (LamVar 3)) (LamAbs 4 (LamVar 4)))
lambdaExpr3 = LamApp lambdaExpr2 lambdaExpr1
lambdaExpr4 = LamApp lambdaExpr1 lambdaExpr2
lambdaExpr5 = (LamApp (LamAbs 1 (LamAbs 2 (LamVar 1))) (LamVar 3))
lambdaExpr6 = LamApp lambdaExpr5 (LamApp (LamAbs 4 (LamVar 4)) (LamVar 5))
-- Smullyan's mockingbird(s)
lambdaOmega = LamAbs 1 (LamApp (LamVar 1) (LamVar 1))
lambdaOmega1 = LamApp lambdaOmega lambdaOmega
-- lambda calculus propositional logic constants and functions
lambdaTrue = LamAbs 1 (LamAbs 2 (LamVar 1))
lambdaFalse = LamAbs 1 (LamAbs 2 (LamVar 2))
lambdaAnd = LamAbs 1 (LamAbs 2 (LamApp (LamApp (LamVar 2) (LamVar 1)) (LamVar 2)))
lambdaAnd1 = LamApp (LamApp lambdaAnd lambdaFalse) lambdaFalse
lambdaAnd2 = LamApp (LamApp lambdaAnd lambdaTrue) lambdaTrue
-- test cases for the church numerals
lambdaZero = LamAbs 1 (LamAbs 2 (LamVar 2))
lambdaSucc = LamAbs 1 (LamAbs 2 (LamAbs 3 (LamApp (LamVar 2) (LamApp (LamApp (LamVar 1) (LamVar 2)) (LamVar 3)))))
lambdaOne = LamAbs 1 (LamAbs 2 (LamApp (LamVar 1) (LamVar 2)))
lambdaSuccZero = LamApp lambdaSucc lambdaZero

-- test for equivalent lambda expressions
equivLam :: LamExpr -> LamExpr -> Bool
-- may need to be replaced by some more sophisticated test
-- such as checking for alpha equivalence
equivLam m n = m == n

-- test for equivalent lambda expressions where the first may be Nothing
equivLam2 :: Maybe LamExpr -> LamExpr -> Bool
-- may need to be replaced by some more sophisticated test
-- such as checking for beta equivalence
equivLam2 Nothing n  = False
equivLam2 (Just m) n = m == n

-- test for two let strings being equivalent
equivLetString :: String -> String -> Bool
-- at present just check string equality modulo extra spaces
-- may need to be replaced by some more sophisticated test
equivLetString s t = remSpaces(s) == remSpaces(t)

-- test for two let expressions being equivalent, where the first may be Nothing
-- may need to be replaced by some more sophisticated test
equivLet :: Maybe Expr -> Expr -> Bool
equivLet Nothing e  = False
equivLet (Just d) e = d == e

-- removed duplicated spaces
remSpaces :: String -> String
remSpaces ""          = ""
remSpaces (' ':' ':s) = remSpaces (' ':s)
remSpaces (c:s)       = c:(remSpaces s)

-- COMP2209 Coursework 2, University of Southampton 2018
-- DUMMY FILE FOR YOU TO EDIT AND ADD YOUR OWN IMPLEMENTATIONS
-- NOTE THAT NO THIRD PARTY MODULES MAY BE USED IN SOLVING THESE EXERCISES AND
-- THAT YOU MAY NOT CHANGE THE FUNCTION TYPE SIGNATURES NOR TYPE DEFINITIONS
-- This module statement makes public only the specified functions and types
-- DO NOT CHANGE THIS LIST OF EXPORTED FUNCTIONS AND TYPES
module Challenges (convertLet, prettyPrint, parseLet, countReds, compileArith,
    Expr(App, Let, Var), LamExpr(LamApp, LamAbs, LamVar)) where

import           Data.Char
import           Parsing

-- Challenge 1
data Expr = App Expr Expr | Let [Int] Expr Expr | Var Int deriving (Show,Eq)
data LamExpr = LamApp LamExpr LamExpr | LamAbs Int LamExpr | LamVar Int deriving (Show,Eq)

-- Convert a let expression to lambda expression
convertLet :: Expr -> LamExpr
convertLet (Var x)         = LamVar x
convertLet (App e1 e2)     = LamApp (convertLet e1) (convertLet e2)
convertLet (Let x e1 e2) | length x == 1 = LamApp (LamAbs (head x) (convertLet e2)) (convertLet e1)
                         | otherwise = LamApp (LamAbs (head x) (convertLet e2)) (absSequence (tail x) e1)
                         where
                           -- Generate a sequence of lambda abstractions
                           absSequence :: [Int] -> Expr -> LamExpr
                           absSequence x' e | length x' == 1 = LamAbs (head x') (convertLet e)
                                            | otherwise = LamAbs (head x') (absSequence (tail x') e)



-- Challenge 2
-- Pretty print a let expression by converting it to a string
prettyPrint :: Expr -> String
prettyPrint (Var x) = "x" ++ show x
prettyPrint (Let x e1 e2) = "let " ++ printVars x ++ "= " ++ prettyPrint e1 ++ " in " ++ prettyPrint e2
prettyPrint (App e1 e2)  = printApp e1 e2

-- Ensures correct bracketing when dealing with multiple let expressions
prettyPrint' :: Expr -> Expr -> String
prettyPrint' e1 e2 = prettyPrint e1 ++ "(" ++ prettyPrint e2 ++ ")"

-- Print application statements with the correct bracketing
printApp :: Expr -> Expr -> String
printApp (App a1 a2@Let{}) e2 = "(" ++ prettyPrint' a1 a2 ++ ") " ++ prettyPrint e2
printApp e1@Let{} e2 = "(" ++ prettyPrint e1 ++ ") " ++ prettyPrint e2
printApp e1 e2@Let{} = prettyPrint e1 ++ "(" ++ prettyPrint e2 ++ ")"
printApp e1 e2@App{} = prettyPrint e1 ++ " (" ++ prettyPrint e2 ++ ")"
printApp e1 e2       = prettyPrint e1 ++ " " ++ prettyPrint e2

-- Print a list of variables
printVars :: [Int] -> String
printVars []     = ""
printVars (x:xs) = "x" ++ show x ++ " " ++ printVars xs



-- Challenge 3
-- Parse a let expression
parseLet :: String -> Maybe Expr
parseLet s = case parse expr s of
  [(a, "")] -> Just a
  _         -> Nothing

-- Base parser for an expression
expr :: Parser Expr
expr = bExprExpr' <|> bExpr <|> varExpr' <|> var <|> letPExpr' <|> letP

-- Parser to handle multiple expressions
expr' :: Expr -> Parser Expr
expr' e = exprExpr' e <|> expr'' e

-- Deepest parser which takes an expression, matches a terminal expressions and
-- returns an application which enforces left association
expr'' :: Expr -> Parser Expr
expr'' e = do e1 <- termExpr
              return (App e e1)

-- Parser for terminal expression (no applications)
termExpr :: Parser Expr
termExpr = bExpr <|> var <|> letP

-- PARSER CASES --
-- Matches a terminal expression followed by another expression
exprExpr' :: Expr -> Parser Expr
exprExpr' e = do e1 <- termExpr
                 symbol ""
                 expr' (App e e1)

-- Matches a bracketed expression followed by another expression
bExprExpr' :: Parser Expr
bExprExpr' = do e1 <- bExpr
                symbol ""
                expr' e1

-- Matches a variable followed by another expression
varExpr' :: Parser Expr
varExpr' = do e1 <- var
              symbol ""
              expr' e1

-- Matches a let expression followed by another expression
letPExpr' :: Parser Expr
letPExpr' = do l <- letP
               symbol ""
               expr' l

-- Matches a bracketed expression
bExpr :: Parser Expr
bExpr = do symbol "("
           e <- expr
           symbol ")"
           return e

-- Matches a variable
var :: Parser Expr
var = do symbol "x"
         x <- natural
         return (Var x)

-- Matches a let expression
letP :: Parser Expr
letP = do symbol "let"
          xl <- varListExpr
          symbol "="
          e1 <- expr
          symbol "in"
          e2 <- expr
          return (Let xl e1 e2)

-- Parses the list of vars at the start of a let expression
varListExpr :: Parser [Int]
varListExpr = many varInt

-- Extract the integer value of a variable
varInt :: Parser Int
varInt = do symbol "x"
            natural




-- Challenge 4
-- Count reductions using two different strategies
countReds :: LamExpr -> Int -> (Maybe Int, Maybe Int)
countReds e limit = (reduce left e limit 0, reduce right e limit 0)

{-| Keeps applying the passed reduction function until either the limit has been
  reached and Nothing is returned or the result of reducing is the same as the
  expression passed, in which case we have fully reduced the expression, and a
  count of the reductions is returned.
-}
reduce :: (LamExpr -> LamExpr) -> LamExpr -> Int -> Int -> Maybe Int
reduce f initExpr limit count
  | count == limit && reducedExpr /= initExpr = Nothing
  | reducedExpr == initExpr = Just count
  | reducedExpr /= initExpr = reduce f reducedExpr limit (count + 1)
   where
     reducedExpr = f initExpr

-- Performs leftmost reduction before reducing the right redexes
left :: LamExpr -> LamExpr
left e | not (null (genRedexes 'l' e)) = evall e
       | not (null (genRedexes 'r' e)) = evalr e
       | otherwise = e

-- Performs rightmost reduction before reduction the left redexes
right :: LamExpr -> LamExpr
right e | not (null (genRedexes 'r' e)) = evalr e
        | not (null (genRedexes 'l' e)) = evall e
        | otherwise = e

-- Generates a list of redexes (reducible expressions)
-- It takes a Char representing whether to generate left 'l' or right 'r' redexes
genRedexes :: Char -> LamExpr -> [LamExpr]
genRedexes _ LamVar{}                    = []
genRedexes d (LamAbs _ t)                = genRedexes d t
genRedexes d e@(LamApp (LamAbs _ e1) e2) = genRedexes d e1 ++ genRedexes d e2 ++ [e]
genRedexes d (LamApp t1 t2) | d == 'l' = genRedexes d t1
                            | otherwise = genRedexes d t2

-- Perform left evaluation
evall :: LamExpr -> LamExpr
evall v@LamVar{}                = v
evall (LamAbs x e1)             = LamAbs x (evall e1)
evall (LamApp (LamAbs x e1) e2) = subst e1 x e2
evall (LamApp e1 e2)            = LamApp (evall e1) e2

-- Perform right evaluation
evalr :: LamExpr -> LamExpr
evalr v@LamVar{}                = v
evalr (LamAbs x e1)             = LamAbs x (evalr e1)
evalr (LamApp (LamAbs x e1) e2) = subst e1 x e2
evalr (LamApp e1 e2@LamVar{})   = LamApp (evalr e1) e2
evalr (LamApp e1 e2)            = LamApp e1 (evalr e2)

-- Verify if an expression is free
free :: Int -> LamExpr -> Bool
free x (LamVar y)     =  x == y
free x (LamAbs y e)   | x == y = False
                      | otherwise = free x e
free x (LamApp e1 e2) = free x e1 || free x e2

-- Perform substition to reduce the expression
subst :: LamExpr -> Int -> LamExpr -> LamExpr
subst (LamVar x) y e | x == y = e
subst (LamVar x) y _ | x /= y = LamVar x
subst (LamAbs x e1) y e  | x /= y && not (free x e)  = LamAbs x (subst e1 y e)
subst (LamAbs x e1) y e  | x /= y &&      free x e   = let x' = rename x in subst (LamAbs x' (subst e1 x (LamVar x'))) y e
subst (LamAbs x e1) y _  | x == y                    = LamAbs x e1
subst (LamApp e1 e2) y e = LamApp (subst e1 y e) (subst e2 y e)

-- Rename a variable by adding 1000 to it's value
rename :: Int -> Int
rename x = x + 1000


-- Used as an intermediate data type when converting a string to a lambda expression.
-- It follows Church's formatting to make conversion simpler
data ArithExpr = N Int | Suc ArithExpr | Plus ArithExpr ArithExpr deriving (Show,Eq)
-- Challenge 5
-- Compile an arithmetic expression into a lambda calculus equivalent
-- If the parser fails to parse the string it returns nothing, otherwise it converts the expression to lambda calculus
compileArith :: String -> Maybe LamExpr
compileArith s = case parseArith s of
  Just l  -> Just (arithToLambda l)
  Nothing -> Nothing

-- Converts an arithmetic expression to lambda calculus, recursively calling itself to build up nested expressions
arithToLambda :: ArithExpr -> LamExpr
arithToLambda (N x)        = LamAbs 1 (LamAbs 2 (convertN x))
arithToLambda (Suc a1)     = LamApp (arithToLambda a1) (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamApp (LamVar 2) (LamApp (LamApp (LamVar 1) (LamVar 2)) (LamVar 3))))))
arithToLambda (Plus a1 a2) = LamApp (arithToLambda (Suc a1)) (arithToLambda a2) -- Reuse the logic for Succ as (+1)1 = 1+1

-- Generates a sequence of applications and variables that represent the passed integer
convertN :: Int -> LamExpr
convertN x | x == 0 = LamVar 2
           | x > 0  = appSequence x
           where
             appSequence n
                | n == 1 = LamApp (LamVar 1) (LamVar 2)
                | otherwise = LamApp (LamVar 1) (appSequence (n - 1))

-- Parses a string into an arithemtic expression
parseArith :: String -> Maybe ArithExpr
parseArith s = case parse arithExpr s of
  [(a, "")] -> Just a
  _         -> Nothing

-- Base parser for an arithmetic expression - suc is required as the successor function alone is valid
arithExpr :: Parser ArithExpr
arithExpr = val <|> suc

-- Parser that covers expression cases
val :: Parser ArithExpr
val = sucValVal' <|> sucVal <|> numVal' <|> num <|> bValVal' <|> bVal

-- Parser to handle addition chains
val' :: ArithExpr -> Parser ArithExpr
val' e = plusValVal' e <|> plusVal e

-- Parser that matches terminal statements - without a trailing plus
termVal :: Parser ArithExpr
termVal = sucVal <|> num <|> bVal

-- PARSER CASES
-- Match a successor expression
suc :: Parser ArithExpr
suc = do symbol "(+"
         a <- val
         symbol ")"
         return (Suc a)

-- Match a successor expression followed by another expression
sucValVal' :: Parser ArithExpr
sucValVal' = do e <- sucVal
                symbol ""
                val' e

-- Match a number followed by another expression
numVal' :: Parser ArithExpr
numVal' = do n <- num
             symbol ""
             val' n

-- Match a bracketed expression followed by another expression
bValVal' :: Parser ArithExpr
bValVal' = do e <- bVal
              symbol ""
              val' e

-- Match a plus followed by a terminal value followed by another expression
plusValVal' :: ArithExpr -> Parser ArithExpr
plusValVal' e = do symbol "+"
                   e1 <- termVal
                   symbol ""
                   val' (Plus e e1)

-- Match a plus followed by a terminal value
plusVal :: ArithExpr -> Parser ArithExpr
plusVal e = do symbol "+"
               e1 <- termVal
               symbol ""
               return (Plus e e1)

-- Match a successor expression followed by another expression
sucVal :: Parser ArithExpr
sucVal = do e1 <- sucN
            symbol ""
            e2 <- val
            return (Plus e1 e2)

-- Get the value from inside the successor
sucN :: Parser ArithExpr
sucN = do symbol "(+"
          a <- val
          symbol ")"
          return a

-- Match a number
num :: Parser ArithExpr
num = do x <- natural
         return (N x)

-- Match a bracketed expression
bVal :: Parser ArithExpr
bVal = do symbol "("
          e <- val
          symbol ")"
          return e



-- TESTS
-- Additional tests for Challenge 5
additionalTests :: [(String, [(String, Bool)])]
additionalTests =
  [
  ("Challenge 5 - Parsing Addition",
    [ ("Test 1: parseArith \"1+1\" equal to Just (Plus (N 1) (N 1))",
        parseArith "1+1" == Just (Plus (N 1) (N 1))
      ),
      ("Test 2: parseArith \"1+2+3\" equal to Just (Plus (Plus (N 1) (N 2)) (N 3))",
        parseArith "1+2+3" == Just (Plus (Plus (N 1) (N 2)) (N 3))
      ),
      ("Test 3: parseArith \"(+1)1\" equal to Plus (N 1) (N 1)",
        parseArith "(+1)1" == Just (Plus (N 1) (N 1))
      ),
      ("Test 4: parseArith \"((+1)1)+1\" equal to Just (Plus (Plus (N 1) (N 1)) (N 1))",
        parseArith "((+1)1)+1" == Just (Plus (Plus (N 1) (N 1)) (N 1))
      )
    ]
  ),
  ("Challenge 5 - Parsing Invalid Inputs",
    [ ("Test 1: parseArith \"(+1\" equal to Nothing",
        parseArith "(+1" == Nothing
      ),
      ("Test 2: parseArith \"+1)\" equal to Nothing",
        parseArith "+1)" == Nothing
      ),
      ("Test 3: parseArith \"(1+1\" equal to Nothing",
        parseArith "(1+1" == Nothing
      ),
      ("Test 4: parseArith \"1+\" equal to Nothing",
        parseArith "1+" == Nothing
      )
    ]
  )
  ]

-- Helper function to convert a lambda expression to a string
lamToString :: LamExpr -> String
lamToString (LamVar n) = [chr (n + 96)]
lamToString (LamApp e1 e2) = "(" ++ lamToString e1 ++ ") (" ++ lamToString e2 ++ ") "
lamToString (LamAbs n e) = "\\" ++ lamToString (LamVar n) ++ ". " ++ lamToString e

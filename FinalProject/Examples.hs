module Examples where

import Syntax

-- Simple arithmetic expression: 2 + 3
e1 :: Expr
e1 = BinExpr (Value (ValI 2)) Add (Value (ValI 3))

-- Arithmetic with precedence: 2 + 3 * 5
e2 :: Expr
e2 = BinExpr (Value (ValI 2)) Add (BinExpr (Value (ValI 3)) Mul (Value (ValI 5)))

-- Not (!)
eNotTrue :: Expr
eNotTrue = UnaryExpr Not (Value (ValB True)) -- not True

-- Not (!)
eNotFalse :: Expr
eNotFalse = UnaryExpr Not (Value (ValB False)) -- not False

-- Boolean AND operation: true && false
eAnd :: Expr
eAnd = BinExpr (Value (ValB True)) And (Value (ValB False))

-- Boolean OR operation: true || false
eOr :: Expr
eOr = BinExpr (Value (ValB True)) Or (Value (ValB False))

-- Not Equal
eNotEqual :: Expr
eNotEqual = BinExpr (Value (ValI 5)) NotEq (Value (ValI 10))  -- Should evaluate to True

-- Equal
eEqual :: Expr
eEqual = BinExpr (Value (ValI 5)) Eq (Value (ValI 5))  -- Should evaluate to True

-- Greater Than
eGreaterThan :: Expr
eGreaterThan = BinExpr (Value (ValI 5)) Syntax.GT (Value (ValI 3)) -- 5 > 3

-- Less Than
eLessThan :: Expr
eLessThan = BinExpr (Value (ValI 2)) Syntax.LT (Value (ValI 10)) -- 2 < 10

-- String concatenation: "Hello " ++ "World"
eConcat :: Expr
eConcat = BinExpr (Value (ValS "Hello ")) Concat (Value (ValS "World"))

-- If-Else expression: if (5 >= 3) then "Yes" else "No"
eIfElse :: Expr
eIfElse = IfElse (BinExpr (Value (ValI 5)) GEq (Value (ValI 3))) (Value (ValS "Yes")) (Value (ValS "No"))

-- Variable assignment: A = 10
stmtAssign :: Stmt
stmtAssign = Assign "A" TypeI (Value (ValI 10))

-- Print statement: print A
stmtPrint :: Stmt
stmtPrint = Print (Ref "A")

-- While loop: while (Counter < 3) { print Counter; Counter = Counter + 1 }
stmtWhile :: Stmt
stmtWhile = While
    (BinExpr (Ref "Counter") Syntax.LT (Value (ValI 3)))
    (Seq
        (Print (Ref "Counter"))
        (End (Assign "Counter" TypeI (BinExpr (Ref "Counter") Add (Value (ValI 1))))))

-- Example of a For Loop
stmtForLoopExample :: Stmt
stmtForLoopExample = For
    (Assign "Counter" TypeI (Value (ValI 0)))  -- Initialization: Counter = 0
    (BinExpr (Ref "Counter") Syntax.LT (Value (ValI 3)))  -- Condition: Counter < 3
    (Assign "Counter" TypeI (BinExpr (Ref "Counter") Add (Value (ValI 1))))  -- Increment: Counter = Counter + 1
    (End (Print (Ref "Counter")))  -- Body: print Counter

-- Do-While loop: do { print Counter; Counter = Counter + 1 } while (Counter < 3)
stmtDoWhile :: Stmt
stmtDoWhile = DoWhile
    (Seq
        (Print (Ref "Counter"))
        (End (Assign "Counter" TypeI (BinExpr (Ref "Counter") Add (Value (ValI 1))))))
    (BinExpr (Ref "Counter") Syntax.LT (Value (ValI 3)))

-- Array Indexing Example
stmtArrayIndexExample :: Expr
stmtArrayIndexExample = Index (Value (ValArray [ValI 10, ValI 20, ValI 30])) (Value (ValI 1))

-- Array Length Example
stmtArrayLengthExample :: Expr
stmtArrayLengthExample = Length (Value (ValArray [ValI 1, ValI 2, ValI 3]))  -- Should evaluate to 3

-- String Length Example
stmtStringLengthExample :: Expr
stmtStringLengthExample = Length (Value (ValS "Hello"))

-- Switch-Case Example
stmtSwitchCaseExample :: Stmt
stmtSwitchCaseExample = Switch (Value (ValI 2))
    [ Case (ValI 1) (End (Print (Value (ValS "Case 1")))),
      Case (ValI 2) (End (Print (Value (ValS "Case 2"))))
    ] (Just (End (Print (Value (ValS "Default Case")))))

-- Function declaration: func sum(a, b) { return a + b; }
stmtFuncDecl :: Stmt
stmtFuncDecl = FuncDecl "sum" ["a", "b"] (End (Return (BinExpr (Ref "a") Add (Ref "b"))))

-- Program: A = 5; B = 3; print A + B;
progExample :: Program
progExample = BeginEnd (Seq
    (Assign "A" TypeI (Value (ValI 5)))
    (Seq
        (Assign "B" TypeI (Value (ValI 3)))
        (End (Print (BinExpr (Ref "A") Add (Ref "B"))))))

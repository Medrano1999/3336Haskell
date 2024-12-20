module Examples where

import Syntax

-- Simple arithmetic expression: 2 + 3
e1 :: Expr
e1 = BinExpr (Value (ValI 2)) Add (Value (ValI 3))

-- Arithmetic with precedence: 2 + 3 * 5
e2 :: Expr
e2 = BinExpr (Value (ValI 2)) Add (BinExpr (Value (ValI 3)) Mul (Value (ValI 5)))

-- MIDSEMESTER: Not (!)
eNotTrue :: Expr
eNotTrue = UnaryExpr Not (Value (ValB True)) -- not True

-- MIDSEMESTER: Not (!)
eNotFalse :: Expr
eNotFalse = UnaryExpr Not (Value (ValB False)) -- not False

-- MIDSEMESTER: Boolean AND operation: true && false
eAnd :: Expr
eAnd = BinExpr (Value (ValB True)) And (Value (ValB False))

-- MIDSEMESTER: Boolean OR operation: true || false
eOr :: Expr
eOr = BinExpr (Value (ValB True)) Or (Value (ValB False))

-- NEW: Not Equal
eNotEqual :: Expr
eNotEqual = BinExpr (Value (ValI 5)) NotEq (Value (ValI 10))  -- Should evaluate to True

-- NEW: Equal
eEqual :: Expr
eEqual = BinExpr (Value (ValI 5)) Eq (Value (ValI 5))  -- Should evaluate to True

-- NEW: Greater Than
eGreaterThan :: Expr
eGreaterThan = BinExpr (Value (ValI 5)) Syntax.GT (Value (ValI 3)) -- 5 > 3

-- NEW: Less Than
eLessThan :: Expr
eLessThan = BinExpr (Value (ValI 2)) Syntax.LT (Value (ValI 10)) -- 2 < 10

-- MIDSEMESTER: String concatenation: "Hello " ++ "World"
eConcat :: Expr
eConcat = BinExpr (Value (ValS "Hello ")) Concat (Value (ValS "World"))

-- If-Else expression: if (5 >= 3) then "Yes" else "No"
eIfElse :: Expr
eIfElse = IfElse (BinExpr (Value (ValI 5)) GEq (Value (ValI 3))) (Value (ValS "Yes")) (Value (ValS "No"))

-- NEW (use of char): Character Assignment Example: Assign 'c' to variable A
stmtCharAssign :: Stmt
stmtCharAssign = Assign "A" TypeC (Value (ValC 'c'))  -- A:char = 'c'

-- Print Character Example: Print variable A
stmtCharPrint :: Stmt
stmtCharPrint = Print (Ref "A")  -- print 

-- NEW: Program for assigning and printing character
progCharSequence :: Program
progCharSequence = BeginEnd (Seq stmtCharAssign (End stmtCharPrint))

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

-- NEW: Do-While loop: do { print Counter; Counter = Counter + 1 } while (Counter < 3)
stmtDoWhile :: Stmt
stmtDoWhile = DoWhile
    (Seq
        (Print (Ref "Counter"))
        (End (Assign "Counter" TypeI (BinExpr (Ref "Counter") Add (Value (ValI 1))))))
    (BinExpr (Ref "Counter") Syntax.LT (Value (ValI 3)))

-- NEW: Array Indexing Example
stmtArrayIndexExample :: Expr
stmtArrayIndexExample = Index (Value (ValArray [ValI 10, ValI 20, ValI 30])) (Value (ValI 1))

-- NEW: Array Length Example
stmtArrayLengthExample :: Expr
stmtArrayLengthExample = Length (Value (ValArray [ValI 1, ValI 2, ValI 3]))  -- Should evaluate to 3

-- NEW: String Length Example
stmtStringLengthExample :: Expr
stmtStringLengthExample = Length (Value (ValS "Hello"))

-- NEW: Switch-Case Example
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

import System.IO.Unsafe

-- single line comment

{-
    multi line comment
-}
-- grammar of the language
{-
<program> -> begin <stmts> end
<stmts> -> <stmt> | <stmt>;<stmts>
<stmt> -> <var> = <expr> 
        | while <expr> <stmts> 
        | for <stmt> <expr> <stmt> <stmts> 
        | print <expr>
        | input <var> <type>
        | do <stmts> while <expr>
        | func <var> (<params>) <stmts>
        | call <var> (<args>)
<var> -> string
<op> -> + | - | * | / | >= | && | || | ++ | index
<expr> -> <val> | <expr> <op> <expr> | if <expr> then <expr> else <expr> | <var>
<val> -> integers | booleans | strings | arrays | error
<env> -> [(<var>, <val>)]
<result> -> <env> | error
-}

-- abstract data types

-- The "Program" type represents a whole program, which consists of multiple statments.
newtype Program = BeginEnd Statements
    deriving Show   -- The "deriving Show" automatically creates a way to display the Program type as a string.

-- "Statements" represents either a single statement or a sequence of statements
data Statements = End Stmt | Seq Stmt Statements
    deriving Show

-- "Stmt" (statment) represents different types of statements in the language.
-- It can be assignment, loop (while or for), print, input, function definition, or function call.
data Stmt = Assign Var Type Expr
          | While Expr Statements
          | DoWhile Statements Expr
          | For Stmt Expr Stmt Statements
          | Print Expr
          | Input Var Type
          | Define Var [Var] Statements -- Define a function with parameters and a body.
          | Call Var [Expr] -- Call a function by its name with arguments.
    deriving Show

-- "Var" is a synonym for "String". This is used to represent variable names.
type Var = String

-- The "Type" represents the type of values that can be used, such as Integer, Boolean, String, or Array.
data Type = TypeI | TypeB | TypeS | TypeA
    deriving Show

-- "Op" represents the different operations available in the language, like addition, subtraction, etc.
data Op = Add | Sub | Mul | Div | GEq | And | Or | Concat | Index
    deriving Show

-- "Expr" (expression) represents different types of expressions in the language.
data Expr = Value Val   -- A literal value
          | BinExpr Expr Op Expr -- A binary operation between two expressions
          | IfElse Expr Expr Expr -- An if-else expression (if condition then value1 else value2)
          | Ref Var -- Reference to a variable
    deriving Show

-- "Val" represents the different types of literal values available in the language
data Val = ValI Int -- Integer value
         | ValB Bool -- Boolean value
         | ValS String -- String value
         | ValA [Val] -- Array of values
         | ValE String -- Error message
    deriving Show

-- "Env" is an environment that maps variable names to their values.
type Env = [(Var, Val)]

-- "FuncEnv" is an environment that stores funciton definitions, mapping function names to parameters and the function body.
type FuncEnv = [(Var, ([Var], Statements))]

-- "Result" represents the result of evaluating a statement, which can be either a valid environment or an error message.
data Result = Valid Env | Error String
    deriving Show

-- Helper function to add a statement to a sequence of statments.
sequenceSS :: Statements -> Stmt -> Statements
sequenceSS (End s) su = Seq s (End su)
sequenceSS (Seq s1 ss) su = Seq s1 (sequenceSS ss su)

-- Evaluate a whole program.
evaluateP :: Program -> Result
evaluateP (BeginEnd ss) = evaluateSS ss [] []

-- Evaluate a sequence of statements.
evaluateSS :: Statements -> Env -> FuncEnv -> Result
evaluateSS (End s) env fenv = evaluateS s env fenv
evaluateSS (Seq s ss) env fenv = case evaluateS s env fenv of
    Valid env' -> evaluateSS ss env' fenv
    Error msg -> Error msg

-- Evaluate a single statement.
evaluateS :: Stmt -> Env -> FuncEnv -> Result
-- Assignemnt statment: Assign value of an expression to a variable.
evaluateS (Assign x t e) env fenv = case (evaluate e env fenv, t) of
    (ValI vi, TypeI) -> Valid ((x, ValI vi) : env)
    (ValB vb, TypeB) -> Valid ((x, ValB vb) : env)
    (ValS vs, TypeS) -> Valid ((x, ValS vs) : env)
    (ValA va, TypeA) -> Valid ((x, ValA va) : env)
    (ValI _, TypeB) -> Error $ "Type mismatch: expected Boolean but got Integer for variable " ++ x
    (ValB _, TypeI) -> Error $ "Type mismatch: expected Integer but got Boolean for variable " ++ x
    (ValS _, TypeA) -> Error $ "Type mismatch: expected Array but got String for variable " ++ x
    (ValE s, _) -> Error s

-- While loop: Repeatedly evaluate the body while the condition is true.
evaluateS (While e ss) env fenv = case evaluate e env fenv of
    ValB False -> Valid env -- Exit loop if condition is false.
    ValI 0 -> Valid env -- Exit loop if condition is 0 (considered false).
    ValB True -> case evaluateSS ss env fenv of
                    Error msg -> Error msg --  If there is an error, return it.
                    Valid env' -> evaluateS (While e ss) env' fenv  -- Recur if contidion is still true.
    ValE msg -> Error msg -- If condition evaluation results in an error, return the error.
    _ -> Error "Condition in while loop must evaluate to a boolean"

-- Do-While loop: Evaluate the body at least once and continue while the conition is true.
evaluateS (DoWhile ss e) env fenv = case evaluateSS ss env fenv of
    Error msg -> Error msg -- If there is an error in the body, return it.
    Valid env' -> case evaluate e env' fenv of
        ValB True -> evaluateS (DoWhile ss e) env' fenv
        ValB False -> Valid env'
        _ -> Error "Condition in do-while loop must evaluate to a boolean"

-- For loop: Initialization, condition, and step to control the iteration.
evaluateS (For init e step body) env fenv = case evaluateS init env fenv of
    Error msg -> Error msg
    Valid env' -> evaluateS (While e (sequenceSS body step)) env' fenv

-- Print statement: Print the value of an expression.
evaluateS (Print e) env fenv = case evaluate e env fenv of
    ValE msg -> unsafePerformIO (print msg >> return (Error msg))
    v -> unsafePerformIO (print v >> return (Valid env))

-- Input statement: Get input from the user and assign it to a variable.
evaluateS (Input x t) env fenv = unsafePerformIO $ do
    putStrLn $ "Enter value for " ++ x ++ ":"
    input <- getLine
    let result = case t of
            TypeI -> case reads input of
                        [(val, "")] -> Valid ((x, ValI val) : env)
                        _ -> Error $ "Invalid input for integer variable " ++ x
            TypeB -> case input of
                        "true" -> Valid ((x, ValB True) : env)
                        "false" -> Valid ((x, ValB False) : env)
                        _ -> Error $ "Invalid input for boolean variable " ++ x
            TypeS -> Valid ((x, ValS input) : env)
            _ -> Error $ "Unsupported input type for variable " ++ x
    return result

-- Evaluate an expression to produce a value.
evaluate :: Expr -> Env -> FuncEnv -> Val
evaluate (Value v) _ _ = v
evaluate (IfElse c e1 e2) env fenv = case evaluate c env fenv of
    ValB True -> evaluate e1 env fenv
    ValB False -> evaluate e2 env fenv
    ValI 0 -> evaluate e2 env fenv
    ValI _ -> evaluate e1 env fenv
    ValE msg -> ValE msg
    _ -> ValE "Condition in if-else must evaluate to a boolean"
evaluate (BinExpr e1 op e2) env fenv = case (evaluate e1 env fenv, evaluate e2 env fenv) of
    (ValI i1, ValI i2) -> evaluateOp (ValI i1) op (ValI i2)
    (ValB b1, ValB b2) -> evaluateOp (ValB b1) op (ValB b2)
    (ValS s1, ValS s2) -> evaluateOp (ValS s1) op (ValS s2)
    (ValA a, ValI i) -> evaluateOp (ValA a) Index (ValI i)
    (ValE msg, _) -> ValE msg
    (_, ValE msg) -> ValE msg
    _ -> ValE "Operands must be of compatible types"
evaluate (Ref x) env _ = case lookup x env of
    Nothing -> ValE "Variable not in scope"
    Just v -> v

evaluateOp :: Val -> Op -> Val -> Val
-- Integer operations
evaluateOp (ValI i1) Add (ValI i2) = ValI (i1 + i2)
evaluateOp (ValI i1) Sub (ValI i2) = ValI (i1 - i2)
evaluateOp (ValI i1) Mul (ValI i2) = ValI (i1 * i2)
evaluateOp (ValI i1) Div (ValI i2) = if i2 == 0 then ValE "Division by zero" else ValI (i1 `div` i2)
evaluateOp (ValI i1) GEq (ValI i2) = ValB (i1 >= i2)

-- Boolean operations
evaluateOp (ValB b1) And (ValB b2) = ValB (b1 && b2)
evaluateOp (ValB b1) Or (ValB b2) = ValB (b1 || b2)

-- String operations
evaluateOp (ValS s1) Concat (ValS s2) = ValS (s1 ++ s2)

-- Array indexing
evaluateOp (ValA arr) Index (ValI idx) =
    if idx < length arr && idx >= 0 then arr !! idx else ValE "Array index out of bounds"

-- Example program
sW :: Stmt -- while A >= 3 { print A; A = A - 1 }
sW = While
    (BinExpr (Ref "A") GEq (Value (ValI 3)))
    (Seq
        (Print (Ref "A"))
        (End (Assign "A" TypeI (BinExpr (Ref "A") Sub (Value (ValI 1))))
        )
    )

progW :: Program
progW = BeginEnd (Seq (Assign "A" TypeI (Value (ValI 5))) (End sW))

-- main function for running the program
main :: IO ()
main = do
    putStrLn "Running example programs:"
    let result1 = evaluateP progW
    putStrLn $ "Result of progW: " ++ show result1

module Evaluator where

import Examples
import Syntax
import System.IO.Unsafe  -- For printing side effects within evaluation

-- Sequences statements; appends a statement to a list of statements
sequenceSS :: Statements -> Stmt -> Statements
sequenceSS (End s) su = Seq s (End su)
sequenceSS (Seq s1 ss) su = Seq s1 (sequenceSS ss su)

-- Evaluates an entire program, starting with an empty environment
evaluateP :: Program -> Result
evaluateP (BeginEnd ss) = evaluateSS ss []

-- Evaluates a sequence of statements
evaluateSS :: Statements -> Env -> Result
evaluateSS (End s) env = evaluateS s env
evaluateSS (Seq s ss) env = case evaluateS s env of
    Valid env' -> evaluateSS ss env'
    Error err -> Error err

-- Evaluates a single statement
evaluateS :: Stmt -> Env -> Result
-- Assignment
evaluateS (Assign x t e) env = case (evaluate e env, t) of
    (ValI i, TypeI) -> Valid ((x, ValI i) : env)      -- Assign integer
    (ValF f, TypeF) -> Valid ((x, ValF f) : env)      -- Assign float
    (ValB b, TypeB) -> Valid ((x, ValB b) : env)      -- Assign boolean
    (ValC c, TypeC) -> Valid ((x, ValC c) : env)      -- Assign character
    (ValS s, TypeS) -> Valid ((x, ValS s) : env)      -- Assign string
    (ValArray arr, TypeArray) -> Valid ((x, ValArray arr) : env)  -- Assign array
    (_, _) -> Error $ "Type mismatch for variable: " ++ x

evaluateS (Switch expr cases def) env =
    case evaluate expr env of
        ValE err -> Error err
        val -> evaluateCases cases def val env
  where
    evaluateCases :: [Case] -> Maybe Statements -> Val -> Env -> Result
    evaluateCases [] Nothing _ env = Valid env
    evaluateCases [] (Just defStmt) _ env = evaluateSS defStmt env
    evaluateCases (Case caseVal stmts : rest) def val env =
        if caseVal == val then evaluateSS stmts env else evaluateCases rest def val env

-- While loop
evaluateS (While e ss) env = case evaluate e env of
    ValB False -> Valid env  -- Terminate if condition is false
    ValB True -> case evaluateSS ss env of
        Valid env' -> evaluateS (While e ss) env'  -- Continue loop
        Error err -> Error err  -- Error in loop body
    _ -> Error "Condition in while loop must be boolean"

-- Do-While loop
evaluateS (DoWhile ss cond) env = case evaluateSS ss env of
    Error err -> Error err
    Valid env' -> case evaluate cond env' of
        ValB True -> evaluateS (DoWhile ss cond) env'  -- Continue loop
        ValB False -> Valid env'  -- Terminate loop
        _ -> Error "Condition in do-while loop must be boolean"

-- Print statement
evaluateS (Print e) env = case evaluate e env of
    ValE err -> unsafePerformIO (print err >> return (Error err))  -- Print error
    val -> unsafePerformIO (print val >> return (Valid env))       -- Print value

-- Function declaration
evaluateS (FuncDecl name params body) env = Valid ((name, ValE "Function declared") : env)

-- Return statement
evaluateS (Return e) env = case evaluate e env of
    ValE err -> Error err
    val -> Valid (("return", val) : env)

-- For loop
evaluateS (For init cond step body) env = case evaluateS init env of
    Error err -> Error err
    Valid env' -> evaluateForLoop cond step body env'
  where
    evaluateForLoop c s b env' = case evaluate c env' of
        ValB False -> Valid env'  -- Exit loop if condition is false
        ValB True -> case evaluateSS b env' of
            Error err -> Error err
            Valid env'' -> case evaluateS s env'' of
                Error err -> Error err
                Valid env''' -> evaluateForLoop c s b env'''  -- Continue loop
        _ -> Error "Condition in for loop must be boolean"

-- Evaluates an expression
evaluate :: Expr -> Env -> Val
-- Value
evaluate (Value v) _ = v

-- Unary operation
evaluate (UnaryExpr Not e) env = case evaluate e env of
    ValB b -> ValB (not b)
    _      -> ValE "Unary NOT requires a boolean operand"

-- If-Else expression
evaluate (IfElse cond e1 e2) env = case evaluate cond env of
    ValB True -> evaluate e1 env
    ValB False -> evaluate e2 env
    _ -> ValE "Condition in if-else must be boolean"

-- String length and array length
evaluate (Length expr) env = case evaluate expr env of
    ValS str -> ValI (length str)
    ValArray arr -> ValI (length arr)
    _ -> ValE "Invalid argument for length"

-- Array indexing
evaluate (Index arrayExpr indexExpr) env = case (evaluate arrayExpr env, evaluate indexExpr env) of
    (ValArray arr, ValI idx) | idx >= 0 && idx < length arr -> arr !! idx
    (ValS str, ValI idx) | idx >= 0 && idx < length str -> ValC (str !! idx)
    _ -> ValE "Invalid indexing"

-- Binary operations
evaluate (BinExpr e1 op e2) env = case (evaluate e1 env, evaluate e2 env) of
    (ValI i1, ValI i2) -> evaluateIntOp op i1 i2  -- Integer operations
    (ValF f1, ValF f2) -> evaluateFloatOp op f1 f2  -- Float operations
    (ValS s1, ValS s2) -> evaluateStringOp op s1 s2  -- String operations
    (ValB b1, ValB b2) -> evaluateBoolOp op b1 b2  -- Boolean operations
    _ -> ValE "Operands are of incompatible types"

-- Variable reference
evaluate (Ref x) env = case lookup x env of
    Nothing -> ValE ("Variable " ++ x ++ " not found")
    Just v -> v

-- Function application
evaluate (App (Func param _ body) args) env =
    let newEnv = foldl (\acc (p, a) -> case evaluate a env of
                    ValE err -> error err
                    val -> (p, val) : acc) env (zip [param] args)
    in evaluate body newEnv
evaluate _ _ = ValE "Unsupported expression"

-- Helper functions for evaluating operations
evaluateIntOp :: Op -> Int -> Int -> Val
evaluateIntOp Add i1 i2 = ValI (i1 + i2)
evaluateIntOp Sub i1 i2 = ValI (i1 - i2)
evaluateIntOp Mul i1 i2 = ValI (i1 * i2)
evaluateIntOp Div i1 i2
    | i2 == 0  = ValE "Division by zero"
    | otherwise = ValI (i1 `div` i2)
evaluateIntOp Syntax.GEq i1 i2 = ValB (i1 >= i2)
evaluateIntOp Syntax.GT i1 i2 = ValB (i1 >= i2)
evaluateIntOp Syntax.LT i1 i2  = ValB (i1 < i2)    -- Explicitly use Syntax.LT
evaluateIntOp Syntax.Eq i1 i2  = ValB (i1 == i2)   -- Explicitly use Syntax.Eq
evaluateIntOp Syntax.NotEq i1 i2 = ValB (i1 /= i2) -- Explicitly use Syntax.NotEq
evaluateIntOp _ _ _ = ValE "Unsupported operation"

evaluateFloatOp :: Op -> Float -> Float -> Val
evaluateFloatOp Add f1 f2 = ValF (f1 + f2)
evaluateFloatOp Sub f1 f2 = ValF (f1 - f2)
evaluateFloatOp Mul f1 f2 = ValF (f1 * f2)
evaluateFloatOp Div f1 f2
    | f2 == 0.0 = ValE "Division by zero"
    | otherwise = ValF (f1 / f2)

evaluateStringOp :: Op -> String -> String -> Val
evaluateStringOp Concat s1 s2 = ValS (s1 ++ s2)

evaluateBoolOp :: Op -> Bool -> Bool -> Val
evaluateBoolOp And b1 b2 = ValB (b1 && b2)
evaluateBoolOp Or b1 b2 = ValB (b1 || b2)
evaluateBoolOp Eq b1 b2 = ValB (b1 == b2)
evaluateBoolOp NotEq b1 b2 = ValB (b1 /= b2)

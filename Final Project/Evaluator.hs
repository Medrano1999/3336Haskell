module Evaluator (evaluateP) where

import Syntax
import System.IO.Unsafe

-- Function to evaluate a program
evaluateP :: Program -> Result
evaluateP (BeginEnd ss) = evaluateSS ss [] []

-- Function to evaluate a sequence of statements
evaluateSS :: Statements -> Env -> FuncEnv -> Result
evaluateSS (End s) env fenv = evaluateS s env fenv
evaluateSS (Seq s ss) env fenv = case evaluateS s env fenv of
    Valid env' -> evaluateSS ss env' fenv
    Error msg -> Error msg

-- Function to evaluate a statement
evaluateS :: Stmt -> Env -> FuncEnv -> Result
evaluateS (Assign x t e) env fenv = case (evaluate e env fenv, t) of
    (ValI vi, TypeI) -> Valid ((x, ValI vi) : env)
    (ValB vb, TypeB) -> Valid ((x, ValB vb) : env)
    (ValS vs, TypeS) -> Valid ((x, ValS vs) : env)
    (ValA va, TypeA) -> Valid ((x, ValA va) : env)
    (_, _) -> Error $ "Type mismatch for variable " ++ x

evaluateS (While e ss) env fenv = case evaluate e env fenv of
    ValB False -> Valid env
    ValB True -> case evaluateSS ss env fenv of
                    Error msg -> Error msg
                    Valid env' -> evaluateS (While e ss) env' fenv
    _ -> Error "While condition must be boolean"

evaluateS (DoWhile ss e) env fenv = case evaluateSS ss env fenv of
    Error msg -> Error msg
    Valid env' -> case evaluate e env' fenv of
        ValB True -> evaluateS (DoWhile ss e) env' fenv
        ValB False -> Valid env'
        _ -> Error "Do-while condition must be boolean"

evaluateS (For init e step body) env fenv = case evaluateS init env fenv of
    Error msg -> Error msg
    Valid env' -> evaluateS (While e (sequenceSS body step)) env' fenv

evaluateS (Print e) env fenv = case evaluate e env fenv of
    ValE msg -> unsafePerformIO (print msg >> return (Error msg))
    v -> unsafePerformIO (print v >> return (Valid env))

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

-- Function to evaluate an expression
evaluate :: Expr -> Env -> FuncEnv -> Val
evaluate (Value v) _ _ = v
evaluate (IfElse c e1 e2) env fenv = case evaluate c env fenv of
    ValB True -> evaluate e1 env fenv
    ValB False -> evaluate e2 env fenv
    _ -> ValE "Condition in if-else must be boolean"

evaluate (BinExpr e1 op e2) env fenv = case (evaluate e1 env fenv, evaluate e2 env fenv) of
    (ValI i1, ValI i2) -> evaluateOp (ValI i1) op (ValI i2)
    (ValB b1, ValB b2) -> evaluateOp (ValB b1) op (ValB b2)
    (ValS s1, ValS s2) -> evaluateOp (ValS s1) op (ValS s2)
    _ -> ValE "Operands must be of compatible types"

evaluate (Ref x) env _ = case lookup x env of
    Nothing -> ValE "Variable not in scope"
    Just v -> v

-- Function to evaluate binary operations
evaluateOp :: Val -> Op -> Val -> Val
evaluateOp (ValI i1) Add (ValI i2) = ValI (i1 + i2)
evaluateOp (ValI i1) Sub (ValI i2) = ValI (i1 - i2)
evaluateOp (ValI i1) Mul (ValI i2) = ValI (i1 * i2)
evaluateOp (ValI i1) Div (ValI i2) = if i2 == 0 then ValE "Division by zero" else ValI (i1 `div` i2)
evaluateOp (ValI i1) GEq (ValI i2) = ValB (i1 >= i2)
evaluateOp (ValB b1) And (ValB b2) = ValB (b1 && b2)
evaluateOp (ValB b1) Or (ValB b2) = ValB (b1 || b2)
evaluateOp (ValS s1) Concat (ValS s2) = ValS (s1 ++ s2)
evaluateOp _ _ _ = ValE "Invalid operation"

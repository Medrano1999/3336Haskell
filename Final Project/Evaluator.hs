module Evaluator where

import Syntax
import System.IO.Unsafe  -- Needed for printing side effects within evaluation

-- Evaluates an entire program, starting from an empty environment
evaluateP :: Program -> Result
evaluateP (BeginEnd ss) = evaluateSS ss []

-- Evaluates a sequence of statements
evaluateSS :: Statements -> Env -> Result
evaluateSS (End s) env = evaluateS s env  -- If there's only one statement, evaluate it
evaluateSS (Seq s ss) env = case evaluateS s env of
    Valid env' -> evaluateSS ss env'  -- If the statement is valid, continue with the rest
    Error s -> Error s  -- If there's an error, return it

-- Evaluates a single statement
evaluateS :: Stmt -> Env -> Result
-- Assignment: Evaluate the expression and assign it to the variable in the environment
evaluateS (Assign x t e) env = case (evaluate e env, t) of
    (ValI vi, TypeI) -> Valid ((x, ValI vi) : env)  -- Assign integer
    (ValF vf, TypeF) -> Valid ((x, ValF vf) : env)  -- Assign float
    (ValB vb, TypeB) -> Valid ((x, ValB vb) : env)  -- Assign boolean
    (ValC vc, TypeC) -> Valid ((x, ValC vc) : env)  -- Assign character
    (ValS vs, TypeS) -> Valid ((x, ValS vs) : env)  -- Assign string
    (ValArray va, TypeArray) -> Valid ((x, ValArray va) : env)  -- Assign array
    (_, _) -> Error $ "Type mismatch for " ++ x  -- Type mismatch error

-- While loop: Continuously evaluate the body while the condition is true
evaluateS (While e ss) env = case evaluate e env of
    ValB False -> Valid env  -- If condition is false, end loop
    ValB True -> case evaluateSS ss env of
        Valid env' -> evaluateS (While e ss) env'  -- If body is valid, loop again
        Error em -> Error em  -- Error in body execution
    _ -> Error "Condition in while loop must be boolean"  -- Invalid condition type

-- Function Declaration
evaluateS (FuncDecl name params body) env = Valid ((name, ValE "Function declared") : env)

-- Return Statement
evaluateS (Return e) env = case evaluate e env of
    ValE err -> Error err  -- Return an error if evaluation fails
    val -> Valid (("return", val) : env)

-- Do-While loop: Execute body at least once, then check condition to continue
evaluateS (DoWhile body cond) env = case evaluateSS body env of
    Error errMsg -> Error errMsg  -- Error in the body
    Valid env' -> case evaluate cond env' of
        ValB True -> evaluateS (DoWhile body cond) env'  -- Loop again if condition is true
        ValB False -> Valid env'  -- Terminate if condition is false
        _ -> Error "Condition in do-while loop must be boolean"  -- Invalid condition type

-- Print statement: Prints the value of an expression
evaluateS (Print e) env = case evaluate e env of
    ValE em -> unsafePerformIO (print em >> return (Error em))  -- Print an error message
    v -> unsafePerformIO (print v >> return (Valid env))  -- Print value and return current environment

-- Evaluates an expression and returns a value
evaluate :: Expr -> Env -> Val
evaluate (Value v) _ = v  -- If it's a value, just return it
evaluate (IfElse cond e1 e2) env = case evaluate cond env of
    ValB True -> evaluate e1 env  -- If condition is true, evaluate the first expression
    ValB False -> evaluate e2 env  -- If false, evaluate the second expression
    _ -> ValE "Condition should be boolean"  -- Error if condition is not boolean

-- Binary expression evaluation
evaluate (BinExpr e1 op e2) env = case (evaluate e1 env, evaluate e2 env) of
    (ValI i1, ValI i2) -> evaluateIntOp op i1 i2  -- Integer operations
    (ValF f1, ValF f2) -> evaluateFloatOp op f1 f2  -- Float operations
    (ValS s1, ValS s2) -> evaluateStringOp op s1 s2  -- String operations
    (ValB b1, ValB b2) -> evaluateBoolOp op b1 b2  -- Boolean operations
    _ -> ValE "Operands are of incompatible types"  -- Error if types do not match

-- Variable reference
evaluate (Ref x) env = case lookup x env of
    Nothing -> ValE ("Variable " ++ x ++ " not in scope")  -- Error if variable not found
    Just v -> v  -- Return the value of the variable

-- Function application: Evaluate the body of the function with the arguments provided
evaluate (App (Func x _ body) args) env =
    let newEnv = foldl (\acc (var, arg) -> case evaluate arg env of
                    ValE err -> error err
                    val -> (var, val) : acc) env (zip [x] args)  -- Update environment with arguments
    in evaluate body newEnv  -- Evaluate the body of the function in the new environment

-- If an expression doesn't match any of the above, return undefined
evaluate _ _ = ValE "Undefined expression"

-- Helper functions to evaluate different types of operations
evaluateIntOp :: Op -> Int -> Int -> Val
evaluateIntOp Add i1 i2 = ValI (i1 + i2)
evaluateIntOp Sub i1 i2 = ValI (i1 - i2)
evaluateIntOp Mul i1 i2 = ValI (i1 * i2)
evaluateIntOp Div i1 i2
    | i2 == 0 = ValE "Division by zero"
    | otherwise = ValI (i1 `div` i2)
evaluateIntOp Eq i1 i2 = ValB (i1 == i2)
evaluateIntOp NotEq i1 i2 = ValB (i1 /= i2)
evaluateIntOp Syntax.LT i1 i2 = ValB (i1 < i2)  -- Explicitly refer to Syntax.LT

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
evaluateBoolOp Eq b1 b2 = ValB (b1 == b2)
evaluateBoolOp NotEq b1 b2 = ValB (b1 /= b2)

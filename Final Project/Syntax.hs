module Syntax where

-- Context-free grammar for the language
{-
<program>       -> begin <stmts> end
<stmts>         -> <stmt> | <stmt> ; <stmts>
<stmt>          -> <var> = <expr>                      -- Assignment statement
                | while <expr> <stmts>                 -- While loop
                | do <stmts> while <expr>              -- Do-while loop
                | for <stmt> <expr> <stmt> <stmts>     -- For loop
                | print <expr>                         -- Print statement
                | input <var> <type>                   -- Input statement (user input)
                | func <var> (<params>) <stmts>        -- Function definition with parameters
                | call <var> (<args>)                  -- Function call with arguments
<var>           -> string
<type>          -> int | bool | string | array         -- Variable types
<op>            -> + | - | * | / | >= | && | || | ++ | index -- Operators
<expr>          -> <val>                               -- A value (e.g., integer, boolean, string)
                | <expr> <op> <expr>                   -- A binary expression
                | if <expr> then <expr> else <expr>    -- If-then-else expression
                | <var>                                -- Variable reference
<val>           -> integers | booleans | strings | arrays | error
<env>           -> [(<var>, <val>)]                    -- Environment storing variable bindings
<result>        -> <env> | error
-}

-- Abstract data types representing components of the language

-- A `Program` is represented as a collection of statements encapsulated within `BeginEnd`.
newtype Program = BeginEnd Statements
    deriving Show

-- A sequence of statements, which can be a single statement (`End`) or a sequence (`Seq`).
data Statements = End Stmt | Seq Stmt Statements
    deriving Show

-- A `Stmt` represents the different types of statements supported in the language.
data Stmt = Assign Var Type Expr         -- Assignment statement (`x:int = expr`)
          | While Expr Statements        -- While loop (`while expr { stmts }`)
          | DoWhile Statements Expr      -- Do-while loop (`do { stmts } while (expr)`)
          | For Stmt Expr Stmt Statements -- For loop (`for (init; cond; step) { stmts }`)
          | Print Expr                   -- Print statement (`print expr`)
          | Input Var Type               -- Input statement (`input var:type`)
          | Define Var [Var] Statements  -- Function definition with parameters (`func f(x, y) { stmts }`)
          | Call Var [Expr]              -- Function call with arguments (`call f(x, y)`)
    deriving Show

-- `Var` represents a variable, which is essentially a string name.
type Var = String

-- Types of variables: integer, boolean, string, or array.
data Type = TypeI   -- Integer type
          | TypeB   -- Boolean type
          | TypeS   -- String type
          | TypeA   -- Array type
    deriving Show

-- Operators that can be used in expressions.
data Op = Add    -- Addition
        | Sub    -- Subtraction
        | Mul    -- Multiplication
        | Div    -- Division
        | GEq    -- Greater or equal to
        | And    -- Logical AND
        | Or     -- Logical OR
        | Concat -- String concatenation
        | Index  -- Array index
    deriving Show

-- Expressions represent calculations or values.
data Expr = Value Val               -- A value (like integer, boolean, etc.)
          | BinExpr Expr Op Expr    -- A binary operation (`expr op expr`)
          | IfElse Expr Expr Expr   -- If-then-else expression (`if c then e1 else e2`)
          | Ref Var                 -- A reference to a variable (`x`)
    deriving Show

-- Values that can be used in the language.
data Val = ValI Int      -- Integer value
         | ValB Bool     -- Boolean value
         | ValS String   -- String value
         | ValA [Val]    -- Array value
         | ValE String   -- Error value (e.g., an error message)
    deriving Show

-- The `Env` type represents the environment, which is essentially a mapping between variables and their values.
type Env = [(Var, Val)]

-- A `FuncEnv` is used to store function definitions with their parameters and body.
type FuncEnv = [(Var, ([Var], Statements))]

-- `Result` is the outcome of evaluating a program or statement.
data Result = Valid Env    -- A valid environment after evaluation
            | Error String -- Error message if the evaluation fails
    deriving Show

sequenceSS :: Statements -> Stmt -> Statements
sequenceSS (End s) su = Seq s (End su)
sequenceSS (Seq s1 ss) su = Seq s1 (sequenceSS ss su)

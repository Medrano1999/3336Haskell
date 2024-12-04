module Syntax where

-- Grammar of the language
{-
<program> -> begin <stmts> end
<stmts> -> <stmt> | <stmt>;<stmts>
<stmt> -> <var> = <expr> | while <expr> <stmts> | for <stmt> <expr> <stmt> <stmts> | do <stmts> while <expr> | print <expr> | func <var> (<params>) <stmts> | return <expr>
<var> -> string
<op> -> + | - | * | / | >= | != | == | concat
<expr> -> <val> | <expr> <op> <expr> | if <expr> then <expr> else <expr> | func <var> <expr> | <expr> <expr> | <var>
<val> -> integers | floats | booleans | characters | strings | arrays
<env> -> [(<var>, <val>)]
<result> -> <env> | error
-}

-- Data type to represent a program
newtype Program = BeginEnd Statements
    deriving Show

-- Data type to represent a series of statements
data Statements = End Stmt | Seq Stmt Statements
    deriving Show

-- Data type to represent different types of statements
data Stmt
    = Assign Var Type Expr           -- Assignment statement: <var> = <expr>
    | While Expr Statements          -- While loop: while <expr> <stmts>
    | For Stmt Expr Stmt Statements  -- For loop: for <stmt>; <expr>; <stmt> <stmts>
    | DoWhile Statements Expr        -- Do-While loop: do <stmts> while <expr>
    | Print Expr                     -- Print statement: print <expr>
    | FuncDecl Var [Var] Statements  -- Function declaration: func <var> (<params>) <stmts>
    | Return Expr                    -- Return statement: return <expr>
    deriving Show

-- Type alias to represent variables
type Var = String

-- Data type to represent types of variables
data Type = TypeI | TypeF | TypeB | TypeC | TypeS | TypeArray
    deriving Show

-- Data type to represent operators
data Op
    = Add | Sub | Mul | Div | GEq | NotEq | Eq | Concat | LT
    deriving Show

-- Data type to represent different types of expressions
data Expr
    = Value Val              -- A value (like an integer or boolean)
    | BinExpr Expr Op Expr   -- A binary operation: <expr> <op> <expr>
    | IfElse Expr Expr Expr  -- An if-else expression: if <expr> then <expr> else <expr>
    | Func Var Type Expr     -- A function with a variable and its body
    | App Expr [Expr]        -- Function application with arguments
    | Ref Var                -- A reference to a variable
    deriving Show

-- Data type to represent different types of values
data Val
    = ValI Int               -- Integer value
    | ValF Float             -- Float value
    | ValB Bool              -- Boolean value
    | ValC Char              -- Character value
    | ValS String            -- String value
    | ValArray [Val]         -- Array of values
    | ValE String            -- Error value (used for error messages)
    deriving Show

-- Type alias to represent the environment, which stores variable-value bindings
type Env = [(Var, Val)]

-- Data type to represent the result of evaluating a program or statement
data Result = Valid Env | Error String
    deriving Show

-- Function to sequence statements; appends a statement to an existing list of statements
sequenceSS :: Statements -> Stmt -> Statements
sequenceSS (End s) su = Seq s (End su)
sequenceSS (Seq s1 ss) su = Seq s1 (sequenceSS ss su)

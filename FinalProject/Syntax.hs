module Syntax where

-- Grammar of the language
{-
<program> -> begin <stmts> end
<stmts> -> <stmt> | <stmt>;<stmts>
<stmt> -> <var> = <expr> 
        | while <expr> <stmts> 
        | for <stmt> <expr> <stmt> <stmts> 
        | do <stmts> while <expr> 
        | print <expr> 
        | func <var> (<params>) <stmts> 
        | return <expr>
        | switch (<expr>) <cases>
<cases> -> case <val>: <stmts>; <cases> | default: <stmts>
<var> -> string
<expr> -> <val> 
         | <expr> <op> <expr> 
         | <unary_op> <expr> 
         | if <expr> then <expr> else <expr>
         | <expr>[<expr>] 
         | length(<expr>) 
         | func <var> <expr> 
         | <expr> <expr> 
         | <var>
<val> -> integers | floats | booleans | characters | strings | arrays
<op> -> + | - | * | / | >= | != | == | concat | && | || | > | < 
<unary_op> -> not
<env> -> [(<var>, <val>)]
<result> -> <env> | error
-}

-- Abstract data types

-- Program structure
newtype Program = BeginEnd Statements

-- Series of statements
data Statements = End Stmt | Seq Stmt Statements

-- Individual statement types
data Stmt
    = Assign Var Type Expr           -- Assignment
    | While Expr Statements          -- While loop
    | For Stmt Expr Stmt Statements  -- For loop
    | DoWhile Statements Expr        -- Do-While loop
    | Print Expr                     -- Print statement
    | FuncDecl Var [Var] Statements  -- Function declaration
    | Return Expr                    -- Return statement
    | Switch Expr [Case] (Maybe Statements) -- Switch-case statement: switch (<expr>) <cases>

data Case = Case Val Statements

-- Variable names
type Var = String

-- Data types for variables
data Type = TypeI | TypeF | TypeB | TypeC | TypeS | TypeArray

-- Operators
data Op
    = Add | Sub | Mul | Div | GEq | NotEq | Eq | Concat | And | Or | LT | GT | Not

-- Expressions
data Expr
    = Value Val              -- A value
    | BinExpr Expr Op Expr   -- Binary operation
    | UnaryExpr Op Expr      -- A unary operation: not <expr>
    | IfElse Expr Expr Expr  -- If-Else expression
    | Index Expr Expr        -- Array indexing: <expr>[<expr>]
    | Length Expr            -- String/array length: length(<expr>)
    | Func Var Type Expr     -- Function
    | App Expr [Expr]        -- Function application
    | Ref Var                -- Variable reference

-- Values
data Val
    = ValI Int               -- Integer
    | ValF Float             -- Float
    | ValB Bool              -- Boolean
    | ValC Char              -- Character
    | ValS String            -- String
    | ValArray [Val]         -- Array
    | ValE String            -- Error

-- Environment
type Env = [(Var, Val)]

-- Evaluation result
data Result = Valid Env | Error String

-- Precedence rules for operators
precedence :: Op -> Int
precedence Add = 0
precedence Sub = 0
precedence Mul = 1
precedence Div = 1
precedence And = -1
precedence Or = -1
precedence GEq = -2
precedence NotEq = -2
precedence Eq = -2
precedence Concat = 0
precedence Syntax.LT = -2
precedence Syntax.GT = -2
precedence Not = -3

-- Helper function for parentheses in binary expressions
requiresParenth :: Op -> Expr -> Bool
requiresParenth op (BinExpr _ o2 _) = precedence op < precedence o2
requiresParenth _ _ = False

-- Show instance for Expr
instance Show Expr where
    show (Value v) = show v
    show (UnaryExpr Not e) = "not " ++ show e
    show (IfElse c e1 e2) = "if (" ++ show c ++ ") then " ++ show e1 ++ " else " ++ show e2
    show (Ref x) = x
    show (App e1 args) = show e1 ++ "(" ++ unwords (map show args) ++ ")"
    show (Func x t e) = "\\" ++ x ++ ":" ++ show t ++ " -> " ++ show e
    show (BinExpr el op er) =
        if requiresParenth op er
        then show el ++ " " ++ show op ++ " (" ++ show er ++ ")"
        else show el ++ " " ++ show op ++ " " ++ show er
    show (Index arr idx) = show arr ++ "[" ++ show idx ++ "]"  -- Show for array indexing
    show (Length e) = "length(" ++ show e ++ ")"              -- Show for length function

-- Show instance for Program
instance Show Program where
    show (BeginEnd ss) = "begin\n" ++ show ss ++ "\nend"

-- Show instance for Val
instance Show Val where
    show (ValI vi) = show vi
    show (ValF vf) = show vf
    show (ValB vb) = show vb
    show (ValC vc) = show [vc]
    show (ValS vs) = show vs
    show (ValArray va) = "[" ++ unwords (map show va) ++ "]"  -- Show for arrays
    show (ValE em) = "ERR: " ++ em

-- Show instance for Statements
instance Show Statements where
    show (End s) = "\t" ++ show s ++ ";"
    show (Seq s ss) = "\t" ++ show s ++ ";\n" ++ show ss

-- Show instance for Op
instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"
    show GEq = ">="
    show NotEq = "!="
    show Eq = "=="
    show Concat = "++"
    show And = "&&"
    show Or = "||"
    show Syntax.LT = "<"
    show Syntax.GT = ">"

-- Show instance for Type
instance Show Type where
    show TypeI = "int"
    show TypeF = "float"
    show TypeB = "bool"
    show TypeC = "char"
    show TypeS = "string"
    show TypeArray = "array"

-- Show instance for Stmt
instance Show Stmt where
    show (Assign x t e) = x ++ ":" ++ show t ++ " = " ++ show e
    show (While e ss) = "while (" ++ show e ++ ")\n" ++ show ss
    show (DoWhile ss e) = "do\n" ++ show ss ++ "while (" ++ show e ++ ")"
    show (For si e su ss) = "for (" ++ show si ++ "; " ++ show e ++ "; " ++ show su ++ ")\n" ++ show ss
    show (Print e) = "print " ++ show e
    show (FuncDecl x ps ss) = "func " ++ x ++ "(" ++ unwords ps ++ ")\n" ++ show ss
    show (Return e) = "return " ++ show e
    show (Switch expr cases def) =
        "switch (" ++ show expr ++ ")\n" ++ unlines (map show cases) ++
        maybe "" (\d -> "default:\n" ++ show d) def

instance Show Case where
    show (Case v ss) = "case " ++ show v ++ ":\n" ++ show ss
    
instance Show Result where
    show (Valid env) = "Valid Environment: " ++ show env
    show (Error msg) = "Error: " ++ msg

-- Custom Eq instance for Val
instance Eq Val where
    (ValI i1) == (ValI i2) = i1 == i2
    (ValF f1) == (ValF f2) = f1 == f2
    (ValB b1) == (ValB b2) = b1 == b2
    (ValC c1) == (ValC c2) = c1 == c2
    (ValS s1) == (ValS s2) = s1 == s2
    (ValArray a1) == (ValArray a2) = a1 == a2
    (ValE e1) == (ValE e2) = e1 == e2
    _ == _ = False


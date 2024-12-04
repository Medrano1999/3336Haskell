import System.IO.Unsafe
-- single line comment

{-
    multi line comment
-}
-- grammar of the language
{-
<program> -> begin <stmts> end
<stmts> -> <stmt> | <stmt>;<stmts>
<stmt> -> <var> = <expr> | while <expr> <stmts> | for <stmt> <expr> <stmt> <stmts> | print <expr>
<var> -> string
<op> -> + | - | * | / | >=
<expr> -> <val> | <expr> <op> <expr> | if <expr> then <expr> else <expr>
    | func <var> <expr> | <expr> <expr> | <var>
<val> -> integers | booleans | error
<env> -> [(<var>, <val>)]
<result> -> <env> | error
-}

-- abstract data types

--data Program = BeginEnd Statements
newtype Program = BeginEnd Statements
    deriving Show

data Statements = End Stmt | Seq Stmt Statements
    deriving Show

data Stmt = Assign Var Type Expr
        | While Expr Statements
        | For Stmt Expr Stmt Statements
        | Print Expr
    deriving Show
-- | IfElS Expr Statements Statements
type Var = String

data Type = TypeI | TypeB
    deriving Show

data Op = Add | Sub | Mul | Div | GEq | And | Or | Concat
    deriving Show

data Expr = Value Val | BinExpr Expr Op Expr | IfElse Expr Expr Expr | Func Var Type Expr -- expr is the body of the fun
        | App Expr Expr | Ref Var
    deriving Show
data Val = ValI Int | ValB Bool | ValE String | ValS String
    deriving Show

type Env = [(Var, Val)]

data Result = Valid Env | Error String
    deriving Show

sequenceSS :: Statements -> Stmt -> Statements
sequenceSS (End s) su = Seq s (End su)
sequenceSS (Seq s1 ss) su = Seq s1 (sequenceSS ss su)

evaluateP :: Program -> Result
evaluateP (BeginEnd ss) = evaluateSS ss []

evaluateSS :: Statements -> Env -> Result
evaluateSS (End s) env = evaluateS s env
evaluateSS (Seq s ss) env = case evaluateS s env of
                                Valid env' -> evaluateSS ss env'
                                Error s -> Error s
    --evaluateSS ss (evaluateS s env)

evaluateS :: Stmt -> Env -> Result
evaluateS (Assign x t e) env = case (evaluate e env, t) of
                                (ValI vi, TypeI) -> Valid ((x, ValI vi) : env)
                                (ValB vb, TypeB) -> Valid $ (x, ValB vb) : env
                                (ValI _, TypeB) -> Error $ "Type mismatch for " ++ x
                                (ValB _, TypeI) -> Error $ "Type mismatch for " ++ x
                                (ValE s, _) -> Error s
evaluateS (While e ss) env = case evaluate e env of
                                ValE em -> Error em
                                ValB False -> Valid env
                                ValI 0 -> Valid env
                                _ -> case evaluateSS ss env of
                                        Error em -> Error em
                                        Valid env' -> evaluateS (While e ss) env'
evaluateS (For si e su ss) env = case evaluateS si env of
                                    Error em -> Error em
                                    Valid env' -> evaluateS (While e (sequenceSS ss su)) env'
evaluateS (Print e) env = case evaluate e env of
                                ValE em -> unsafePerformIO(print em >> return (Error em))
                                v -> unsafePerformIO(print v >> return (Valid env))


evaluateOp :: Val -> Op -> Val -> Val
-- Integer operations
evaluateOp (ValI i1) Add (ValI i2) = ValI (i1 + i2)
evaluateOp (ValI i1) Sub (ValI i2) = ValI (i1 - i2)
evaluateOp (ValI i1) Mul (ValI i2) = ValI (i1 * i2)
evaluateOp (ValI i1) Div (ValI i2) = ValI (i1 `div` i2)
evaluateOp (ValI i1) GEq (ValI i2) = ValB (i1 >= i2)

-- Boolean operations
evaluateOp (ValB b1) And (ValB b2) = ValB (b1 && b2)
evaluateOp (ValB b1) Or (ValB b2) = ValB (b1 || b2)
--valuateOp (ValB b) Not _ = ValB (not b)

-- String operations
evaluateOp (ValS s1) Concat (ValS s2) = ValS (s1 ++ s2)

evaluate :: Expr -> Env -> Val
evaluate (Value v) _ = v
evaluate (UnExpr Not e) env = case evaluate e env of
    ValB b -> ValB (not b)
    _ -> ValE "Operand for 'not' must be a boolean"
evaluate (IfElse c e1 e2) env = case evaluate c env of
                                ValB True -> evaluate e1 env
                                ValB False -> evaluate e2 env
                                ValI 0 -> evaluate e2 env
                                ValI _ -> evaluate e1 env
                                ValE s -> ValE s
                                --ValI i -> error "condition should be a boolean expression"
evaluate (BinExpr e1 op e2) env = case (evaluate e1 env, evaluate e2 env) of
                                (ValI i1, ValI i2) -> evaluateOp (ValI i1) op (ValI i2)
                                (ValB b1, ValB b2) -> evaluateOp (ValB b1) op (ValB b2)
                                (ValS s1, ValS s2) -> evaluateOp (ValS s1) op (ValS s2)
                                (ValE em1, _) -> ValE em1
                                (_, ValE em2) -> ValE em2
                                _ -> ValE "operands should be integer"
evaluate (Ref x) env = case lookup x env of
                        Nothing -> ValE "Variable not in scope"
                        Just v -> v
evaluate (App (Func x t e) e2) env = case (evaluate e2 env, t) of
                                        (ValI vi, TypeI) -> evaluate e ((x, ValI vi) : env)
                                        (ValB vb, TypeB) -> evaluate e ((x, ValB vb) : env)
                                        (ValI _, TypeB) -> ValE $ "Type mismatch for " ++ x
                                        (ValB _, TypeI) -> ValE $ "Type mismatch for " ++ x
                                        (ValE s, _) -> ValE s
evaluate _ env = ValE "undefined" -- Func or App e1 e2 where e1 is not a function

precedence :: Op -> Int
precedence And = 0
precedence Or = 0
precedence Not = 0
precedence Add = 1
precedence Sub = 1
precedence Mul = 2
precedence Div = 2
precedence Concat = 2

e1 :: Expr -- 2 + 3 * 5
e1 = BinExpr (Value (ValI 2)) Add (BinExpr (Value (ValI 3)) Mul (Value (ValI 5))) 

e2 :: Expr -- if (2>=3) then 5 else false
e2 = IfElse (BinExpr (Value (ValI 2)) GEq (Value (ValI 3))) (Value (ValI 5)) (Value (ValB False))

e3:: Expr -- 2
e3 = Value (ValI 2)

e4 :: Expr -- 5 + 3 - 2
e4 = BinExpr (BinExpr (Value (ValI 5)) Add (Value (ValI 3))) Sub (Value (ValI 2))

e5 :: Expr
e5 = BinExpr (BinExpr (Ref "A") Add (Value (ValI 3))) Sub (Value (ValI 2))

e6 :: Expr
e6 = BinExpr (Ref "A") Mul (Ref "A")

e8 :: Expr
e8 = Func "A" TypeI e6

e7 :: Expr
e7 = App e8 (Value (ValB False))

e9 :: Expr -- String concatenation "Hello" ++ " World"
e9 = BinExpr (Value (ValS "Hello")) Concat (Value (ValS " World"))

e10 :: Expr -- Logical AND True && False
e10 = BinExpr (Value (ValB True)) And (Value (ValB False))

s :: Stmt
s = Assign "X" TypeI e7

p1 = BeginEnd (End s)

s1 :: Stmt
s1 = Assign "A" TypeI (Value (ValI 5))

s2 :: Stmt
s2 = Assign "B" TypeI (BinExpr (Ref "A") Add (Value (ValI 5)))

s3 :: Stmt
s3 = Assign "C" TypeI (BinExpr (Ref "A") Add (Ref "B"))

ss :: Statements
ss = Seq s1 (Seq s2 (End (Print e3)))

p :: Program
p = BeginEnd ss

sW :: Stmt -- while A>=3 {print A; A=A-1}
sW = While 
        (BinExpr (Ref "A") GEq (Value (ValI 3))) 
            (Seq 
                (Print (Ref "A")) 
                (End (Assign "A" TypeI (BinExpr (Ref "A") Sub (Value (ValI 1)))))
            )

progW :: Program
progW = BeginEnd (Seq (Assign "A" TypeI (Value (ValI 5))) (End sW))

sF :: Stmt -- for (int B = 5; B >= 3; B = B-1) {Print B}
sF = For 
        (Assign "C" TypeI (Value (ValI 5))) 
        (BinExpr (Ref "B") GEq (Value (ValI 3))) 
        (Assign "B" TypeI (BinExpr (Ref "B") Sub (Value (ValI 1)))) 
            (End (Print (Ref "B")))

progF :: Program
progF = BeginEnd (End sF)
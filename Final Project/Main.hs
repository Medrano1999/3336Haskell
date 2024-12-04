module Main where

import Syntax
import Evaluator

-- Main function to run all examples
main :: IO ()
main = do
    putStrLn "Running example programs:\n"

    -- Float Example
    let floatExample = Assign "X" TypeF (Value (ValF 3.14))
    print (evaluateS floatExample [])

    -- Character Example
    let charExample = Assign "Y" TypeC (Value (ValC 'a'))
    print (evaluateS charExample [])

    -- String Manipulation Example
    let stringManipulationExample = Assign "Z" TypeS (BinExpr (Value (ValS "Hello ")) Concat (Value (ValS "World")))
    print (evaluateS stringManipulationExample [])

    -- Equality Comparison Example
    let equalityComparisonExample = Assign "A" TypeB (BinExpr (Value (ValI 5)) Eq (Value (ValI 5)))
    print (evaluateS equalityComparisonExample [])

    -- Inequality Comparison Example
    let inequalityComparisonExample = Assign "B" TypeB (BinExpr (Value (ValI 5)) Syntax.LT (Value (ValI 10)))
    print (evaluateS inequalityComparisonExample [])

    -- Array/List Example
    let arrayExample = Assign "L" TypeArray (Value (ValArray [ValI 1, ValI 2, ValI 3]))
    print (evaluateS arrayExample [])

    -- Function Definition Example
    let functionDefinitionExample = FuncDecl "sum" ["a", "b"] (End (Return (BinExpr (Ref "a") Add (Ref "b"))))
    print (evaluateS functionDefinitionExample [])

    -- While Loop Example
    let whileLoopExample = While
            (BinExpr (Ref "Counter") Syntax.LT (Value (ValI 3)))  -- Condition is Counter < 3
            (Seq
                (Print (Ref "Counter"))  -- Print Counter
                (End (Assign "Counter" TypeI (BinExpr (Ref "Counter") Add (Value (ValI 1))))))  -- Increment Counter
    print (evaluateS whileLoopExample [("Counter", ValI 0)])

    -- DoWhile Loop Example
    let doWhileExample = DoWhile
            (Seq
                (Print (Ref "Counter"))  -- Print Counter
                (End (Assign "Counter" TypeI (BinExpr (Ref "Counter") Add (Value (ValI 1))))))  -- Increment Counter
            (BinExpr (Ref "Counter") Syntax.LT (Value (ValI 3)))  -- Condition is Counter < 3
    print (evaluateS doWhileExample [("Counter", ValI 0)])

module Main where

import Syntax
import Evaluator

-- Example program to test
sW :: Stmt -- while A >= 3 { print A; A = A - 1 }
sW = While
    (BinExpr (Ref "A") GEq (Value (ValI 3)))
    (Seq
        (Print (Ref "A"))
        (End (Assign "A" TypeI (BinExpr (Ref "A") Sub (Value (ValI 1)))))
    )

progW :: Program
progW = BeginEnd (Seq (Assign "A" TypeI (Value (ValI 5))) (End sW))

-- Main function for running the program
main :: IO ()
main = do
    putStrLn "Running example programs:"
    let result1 = evaluateP progW
    putStrLn $ "Result of progW: " ++ show result1

module MutationTests (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Language
import Mutation

tests = testGroup "Mutation calculations"
         [
         testCase "Constant expression has no variables" $ (countVariables (Operand TRUE)) @?= 0,
         testCase "Single variable has one variable" $ (countVariables (Operand (Var "a"))) @?= 1,
         testCase "Unary operator has as many variables as underlying operand" $
           (countVariables (UnaryOperator Yes (Operand (Var "a")))) @?= 1,
         testCase "Binary operator has as many variables as sum of underlying operands" $
                    (countVariables (BinaryOperator And
                        (UnaryOperator Yes (Operand (Var "a")))
                        (BinaryOperator Xor (UnaryOperator Not (Operand (Var "b")))
                            (Operand (Var "c"))))) @?= 3,
         testCase "Variables are only counted once" $
                    (countVariables (BinaryOperator And
                        (UnaryOperator Yes (Operand (Var "a")))
                        (BinaryOperator Xor (UnaryOperator Not (Operand (Var "a")))
                            (Operand (Var "c"))))) @?= 2,
         testCase "Constant operand has two forms" $ (countForms (Operand TRUE)) @?= 2
         -- ,
         -- testCase "Constant operand has two forms" $ (countForms (Operand (Var "a"))) @?= 3
         ]
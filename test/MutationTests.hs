module MutationTests (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Helpers.ExpressionCreators

import Language
import Mutation
import VariableHelpers
import MutationConfiguration

tests = testGroup "Mutation calculations"
         [
         testCase "Constant expression has no variables" $ (countVariables (Operand Truth)) @?= 0,
         testCase "Single variable has one variable" $ (countVariables (Operand (var "a"))) @?= 1,
         testCase "Unary operator has as many variables as underlying operand" $
           (countVariables (UnaryOperator Yes (Operand (var "a")))) @?= 1,
         testCase "Binary operator has as many variables as sum of underlying operands" $
           (countVariables (NAryOperator And [
             (UnaryOperator Yes (Operand (var "a"))),
             (NAryOperator Xor
               [UnaryOperator Not (Operand (var "b")),
               Operand (var "c")]
             )]
           ))@?= 3,
         testCase "Variables are only counted once" $
           (countVariables (NAryOperator And [
             UnaryOperator Yes (Operand (var "a")),
             NAryOperator Xor [
               UnaryOperator Not (Operand (var "a")),
               Operand (var "c")
               ]
             ]
           )) @?= 2,
         testCase "Constant operand has two forms" $ (countForms maxMC (Operand Truth)) @?= 2,
         testCase "Single variable has four forms" $ (countForms maxMC (Operand (var "a"))) @?= 4,
         testCase "Single unary operator with variable has four forms" $
           (countForms maxMC (UnaryOperator Not (Operand (var "a")))) @?= 4,
         testCase "Binary operator with one variable has fourty-eight forms" $
           (countForms maxMC (NAryOperator Xor [Operand (var "a"), Operand Truth])) @?= 96
         ]
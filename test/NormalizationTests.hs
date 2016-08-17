module NormalizationTests (tests) where

import Language
import Normalization

import Test.Tasty

import Test.Tasty.HUnit

tests = testGroup "Grammar normalization"
         [
         testCase "Normalization adds unary operators to operands" $
             (normalize (Operand TRUE)) @?= (UnaryOperator Yes (Operand TRUE)),
         testCase "Normalization does not add unary operator to operands if it already exists" $
             (normalize (UnaryOperator Not (Operand TRUE))) @?= (UnaryOperator Not (Operand TRUE)),
         testCase "Normalization does not remove additional unary operators to operands if it already exists" $
             (normalize (UnaryOperator Not (UnaryOperator Yes (Operand FALSE)))) @?=
                 UnaryOperator Not (UnaryOperator Yes (Operand FALSE)),
         testCase "Normalization adds unary operator before binary operator" $
             (normalize (BinaryOperator Xor (Operand (Var "a")) (Operand TRUE))) @?=
                 (UnaryOperator Yes (BinaryOperator Xor
                     (UnaryOperator Yes(Operand (Var "a")))
                     (UnaryOperator Yes (Operand TRUE))))
         ]
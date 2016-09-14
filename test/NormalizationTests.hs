module NormalizationTests (tests) where

import Language
import Normalization

import Helpers.ExpressionCreators

import Test.Tasty
import Test.Tasty.HUnit

tests = testGroup "Grammar normalization"
         [
         testCase "Normalization adds unary operators to operands" $
             (normalize (Operand Truth)) @?= (UnaryOperator Yes (Operand Truth)),
         testCase "Normalization does not add unary operator to operands if it already exists" $
             (normalize (UnaryOperator Not (Operand Truth))) @?= (UnaryOperator Not (Operand Truth)),
         testCase "Normalization does not add unary operator to nary operator if it already exists" $
             (normalize (UnaryOperator Not (NAryOperator Xor [Operand (var "a")]))) @?=
               (UnaryOperator Not (NAryOperator Xor [UnaryOperator Yes (Operand (var "a"))])),
         testCase "Normalization does not remove additional unary operators to operands if it already exists" $
             (normalize (UnaryOperator Not (UnaryOperator Yes (Operand Truth)))) @?=
                 UnaryOperator Not (UnaryOperator Yes (Operand Truth)),
         testCase "Normalization adds unary operator before nary operator" $
             (normalize (NAryOperator Xor [Operand (var "a"), Operand Truth])) @?=
                 (UnaryOperator Yes (NAryOperator Xor [
                     UnaryOperator Yes(Operand (var "a")),
                     UnaryOperator Yes (Operand Truth)
                     ])
                 )
         ]
module LanguageTests (tests) where

import Language
import Normalization

import Test.Tasty

import Test.Tasty.HUnit

tests = testGroup "basic grammar"
         [
         testCase "Normalization adds unary operators to operands" $
             (normalize (Operand TRUE)) @=? (UnaryOperator Yes (Operand TRUE)),
          testCase "Normalization does not add unary operators to operands if it already exists" $
             (normalize (UnaryOperator Not (Operand TRUE))) @=? (UnaryOperator Not (Operand TRUE)),
          testCase "Normalization does not remove additional unary operators to operands if it already exists" $
             (normalize (UnaryOperator Not (UnaryOperator Yes (Operand FALSE)))) @=?
                 (normalize (UnaryOperator Not (UnaryOperator Yes (Operand FALSE))))
         ]
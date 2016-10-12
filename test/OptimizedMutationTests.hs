module OptimizedMutationTests (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Helpers.ExpressionCreators

import Language
import OptimizedMutation
import VariableHelpers
import MutationConfiguration

tests = testGroup "Mutation calculations"
         [
         testCase "Input space for single variable has two elements" $
             (generateInputSpace (Operand $ var "a")) @?= [
                 [(Variable "a", Operand Truth)],
                 [(Variable "a", UnaryOperator Not (Operand Truth))]
             ],
         testCase "Input space for two variables has four elements" $
             (generateInputSpace (NAryOperator And [Operand (var "a"), Operand (var "b")])) @?= [
                 [(Variable "a", Operand Truth), (Variable "b", Operand Truth)],
                 [(Variable "a", Operand Truth), (Variable "b", UnaryOperator Not (Operand Truth))],
                 [(Variable "a", UnaryOperator Not (Operand Truth)), (Variable "b", Operand Truth)],
                 [(Variable "a", UnaryOperator Not (Operand Truth)), (Variable "b", UnaryOperator Not (Operand Truth))]
             ]
         ]
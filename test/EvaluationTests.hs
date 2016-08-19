module EvaluationTests (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Helpers.ExpressionCreators

import Language
import Evaluation

tests = testGroup "Expression evaluations" $ [operandTests, unaryTests, andTests, orTests]

operandTests = testGroup "operand tests"
  [
     testCase "Constant expression cannot be simplified" $ (evaluate (Operand Truth) []) @?= (Operand Truth),
     testCase "Variable cannot be simplified" $ (evaluate (Operand (var "a")) []) @?= (Operand (var "a")),
     testCase "Variable substituted with constant cannot be simplified" $
       (evaluate (Operand (var "a")) [(Variable "a", Operand Truth)]) @?= (Operand Truth)
  ]

unaryTests = testGroup "unary operator tests"
  [
     testCase "Operator Yes is removed by simplification" $
       (evaluate (UnaryOperator Yes (Operand Truth)) []) @?= (Operand Truth),
     testCase "Operator No is left by simplification" $
       (evaluate (UnaryOperator Not (Operand Truth)) []) @?= (UnaryOperator Not (Operand Truth))
  ]

andTests = testGroup "and operator tests"
  [
     testCase "And of two truths is simplified to truth" $
       (evaluate (BinaryOperator And (Operand Truth) (Operand Truth)) []) @?= (Operand Truth),
     testCase "And of truth and not truth is not truth" $
       (evaluate (BinaryOperator And (Operand Truth) (UnaryOperator Not (Operand Truth))) []) @?=
         (UnaryOperator Not (Operand Truth)),
     testCase "And of not truth and truth is not truth" $
       (evaluate (BinaryOperator And (UnaryOperator Not (Operand Truth)) (Operand Truth)) []) @?=
         (UnaryOperator Not (Operand Truth)),
     testCase "And of not truth and not truth is not truth" $
       (evaluate (BinaryOperator And (UnaryOperator Not (Operand Truth)) (UnaryOperator Not (Operand Truth))) []) @?=
         (UnaryOperator Not (Operand Truth)),
     testCase "And of var and not truth is not truth" $
       (evaluate (BinaryOperator And (UnaryOperator Not (Operand Truth)) (Operand (var "a"))) []) @?=
         (UnaryOperator Not (Operand Truth)),
     testCase "And of truth and var is var" $
       (evaluate (BinaryOperator And (Operand Truth) (Operand (var "a"))) []) @?= (Operand (var "a")),
     testCase "And of var and truth is var" $
       (evaluate (BinaryOperator And (Operand (var "a")) (Operand Truth)) []) @?= (Operand (var "a")),
     testCase "And of two same vars is this var" $
       (evaluate (BinaryOperator And (Operand (var "a")) (Operand (var "a"))) []) @?= (Operand (var "a")),
     testCase "And of two vars remains and of two vars" $
       (evaluate (BinaryOperator And (Operand (var "a")) (Operand (var "b"))) []) @?=
         (BinaryOperator And (Operand (var "a")) (Operand (var "b"))),
     testCase "And of two vars can be sometimes computed if only one is known" $
       (evaluate (BinaryOperator And (Operand (var "a")) (Operand (var "b")))
         [(Variable "a", (UnaryOperator Not (Operand Truth)))]) @?= (UnaryOperator Not (Operand Truth))

  ]


orTests = testGroup "or operator tests"
  [
     testCase "Or of two truths is simplified to truth" $
       (evaluate (BinaryOperator Or (Operand Truth) (Operand Truth)) []) @?= (Operand Truth),
     testCase "Or of truth and not truth is truth" $
       (evaluate (BinaryOperator Or (Operand Truth) (UnaryOperator Not (Operand Truth))) []) @?= (Operand Truth),
     testCase "Or of not truth and truth is truth" $
       (evaluate (BinaryOperator Or (UnaryOperator Not (Operand Truth)) (Operand Truth)) []) @?= (Operand Truth),
     testCase "Or of not truth and not truth is not truth" $
       (evaluate (BinaryOperator Or (UnaryOperator Not (Operand Truth)) (UnaryOperator Not (Operand Truth))) []) @?=
         (UnaryOperator Not (Operand Truth)),
     testCase "Or of var and not truth is var" $
       (evaluate (BinaryOperator Or (UnaryOperator Not (Operand Truth)) (Operand (var "a"))) []) @?= (Operand (var "a")),
     testCase "Or of truth and var is truth" $
       (evaluate (BinaryOperator Or (Operand Truth) (Operand (var "a"))) []) @?= (Operand Truth),
     testCase "Or of var and truth is truth" $
       (evaluate (BinaryOperator Or (Operand (var "a")) (Operand Truth)) []) @?= (Operand Truth),
     testCase "Or of two same vars is this var" $
       (evaluate (BinaryOperator Or (Operand (var "a")) (Operand (var "a"))) []) @?= (Operand (var "a")),
     testCase "Or of two vars remains or of two vars" $
       (evaluate (BinaryOperator Or (Operand (var "a")) (Operand (var "b"))) []) @?=
         (BinaryOperator Or (Operand (var "a")) (Operand (var "b"))),
     testCase "Or of two vars can be sometimes computed if only one is known" $
       (evaluate (BinaryOperator Or (Operand (var "a")) (Operand (var "b")))
         [(Variable "a", Operand Truth)]) @?= (Operand Truth),
     testCase "Or of two vars can simplified if only one is known" $
       (evaluate (BinaryOperator Or (Operand (var "a")) (Operand (var "b")))
         [(Variable "a", (UnaryOperator Not (Operand Truth)))]) @?= (Operand (var "b"))
  ]
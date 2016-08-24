module EvaluationTests (tests) where

import Data.List (sort)

import Test.Tasty
import Test.Tasty.HUnit

import Helpers.ExpressionCreators

import Language
import Evaluation

tests = testGroup "Expression evaluations" $ [operandTests, unaryTests, binaryTests, complexTests]

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

binaryTests = testGroup "binary operators" $ [andTests, orTests, xorTests]

andTests = testGroup "and operator tests"
  [
     testCase "And of two truths is simplified to truth" $
       (evaluate (NAryOperator And [Operand Truth, Operand Truth]) []) @?= (Operand Truth),
     testCase "And of truth and not truth is not truth" $
       (evaluate (NAryOperator And [Operand Truth, UnaryOperator Not (Operand Truth)]) []) @?=
         (UnaryOperator Not (Operand Truth)),
     testCase "And of not truth and truth is not truth" $
       (evaluate (NAryOperator And [UnaryOperator Not (Operand Truth), (Operand Truth)]) []) @?=
         (UnaryOperator Not (Operand Truth)),
     testCase "And of not truth and not truth is not truth" $
       (evaluate (NAryOperator And [UnaryOperator Not (Operand Truth), UnaryOperator Not (Operand Truth)]) []) @?=
         (UnaryOperator Not (Operand Truth)),
     testCase "And of var and not truth is not truth" $
       (evaluate (NAryOperator And [UnaryOperator Not (Operand Truth), (Operand (var "a"))]) []) @?=
         (UnaryOperator Not (Operand Truth)),
     testCase "And of truth and var is var" $
       (evaluate (NAryOperator And [Operand Truth, Operand (var "a")]) []) @?= (Operand (var "a")),
     testCase "And of var and truth is var" $
       (evaluate (NAryOperator And [Operand (var "a"), Operand Truth]) []) @?= (Operand (var "a")),
     testCase "And of two same vars is this var" $
       (evaluate (NAryOperator And [Operand (var "a"), Operand (var "a")]) []) @?= (Operand (var "a")),
     testCase "And of two vars remains and of two vars" $
       (evaluate (NAryOperator And [Operand (var "a"), Operand (var "b")]) []) @?=
         (NAryOperator And [Operand (var "a"), Operand (var "b")]),
     testCase "And of two vars can be sometimes computed if only one is known" $
       (evaluate (NAryOperator And [Operand (var "a"), Operand (var "b")])
         [(Variable "a", UnaryOperator Not (Operand Truth))]) @?= (UnaryOperator Not (Operand Truth))
  ]

orTests = testGroup "or operator tests"
  [
     testCase "Or of two truths is simplified to truth" $
       (evaluate (NAryOperator Or [Operand Truth, Operand Truth]) []) @?= (Operand Truth),
     testCase "Or of truth and not truth is truth" $
       (evaluate (NAryOperator Or [Operand Truth, UnaryOperator Not (Operand Truth)]) []) @?= (Operand Truth),
     testCase "Or of not truth and truth is truth" $
       (evaluate (NAryOperator Or [UnaryOperator Not (Operand Truth), Operand Truth]) []) @?= (Operand Truth),
     testCase "Or of not truth and not truth is not truth" $
       (evaluate (NAryOperator Or [UnaryOperator Not (Operand Truth), (UnaryOperator Not (Operand Truth))]) []) @?=
         (UnaryOperator Not (Operand Truth)),
     testCase "Or of var and not truth is var" $
       (evaluate (NAryOperator Or [UnaryOperator Not (Operand Truth), Operand (var "a")]) []) @?= (Operand (var "a")),
     testCase "Or of truth and var is truth" $
       (evaluate (NAryOperator Or [Operand Truth, Operand (var "a")]) []) @?= (Operand Truth),
     testCase "Or of var and truth is truth" $
       (evaluate (NAryOperator Or [Operand (var "a"), Operand Truth]) []) @?= (Operand Truth),
     testCase "Or of two same vars is this var" $
       (evaluate (NAryOperator Or [Operand (var "a"), Operand (var "a")]) []) @?= (Operand (var "a")),
     testCase "Or of two vars remains or of two vars" $
       (evaluate (NAryOperator Or [Operand (var "a"), Operand (var "b")]) []) @?=
         (NAryOperator Or [Operand (var "a"), Operand (var "b")]),
     testCase "Or of two vars can be sometimes computed if only one is known" $
       (evaluate (NAryOperator Or [Operand (var "a"), Operand (var "b")])
         [(Variable "a", Operand Truth)]) @?= (Operand Truth),
     testCase "Or of two vars can simplified if only one is known" $
       (evaluate (NAryOperator Or [Operand (var "a"), Operand (var "b")])
         [(Variable "a", (UnaryOperator Not (Operand Truth)))]) @?= (Operand (var "b"))
  ]

xorTests = testGroup "or operator tests"
  [
     testCase "Xor of two truths is simplified to not truth" $
       (evaluate (NAryOperator Xor [Operand Truth, Operand Truth]) []) @?= (UnaryOperator Not (Operand Truth)),
     testCase "Xor of truth and not truth is truth" $
       (evaluate (NAryOperator Xor [Operand Truth, UnaryOperator Not (Operand Truth)]) []) @?= (Operand Truth),
     testCase "Xor of not truth and truth is truth" $
       (evaluate (NAryOperator Xor [UnaryOperator Not (Operand Truth), Operand Truth]) []) @?= (Operand Truth),
     testCase "Xor of not truth and not truth is not truth" $
       (evaluate (NAryOperator Xor [UnaryOperator Not (Operand Truth), UnaryOperator Not (Operand Truth)]) []) @?=
         (UnaryOperator Not (Operand Truth)),
     testCase "Xor of var and truth cannot be simplified" $
       (evaluate (NAryOperator Xor [Operand (var "a"), Operand Truth]) []) @?=
         (NAryOperator Xor $ sort [Operand (var "a"), Operand Truth]),
     testCase "Xor of two same vars is not truth" $
       (evaluate (NAryOperator Xor [Operand (var "a"), Operand (var "a")]) []) @?= (UnaryOperator Not (Operand Truth)),
     testCase "Xor of two vars remains or of two vars" $
       (evaluate (NAryOperator Xor [Operand (var "a"), Operand (var "b")]) []) @?=
         (NAryOperator Xor [Operand (var "a"), Operand (var "b")]),
     testCase "Xor of two vars cannot be computed if only one is known" $
       (evaluate (NAryOperator Xor [Operand (var "a"), Operand (var "b")])
         [(Variable "a", Operand Truth)]) @?= (NAryOperator Xor $ sort [Operand Truth, Operand (var "b")])
  ]

complexTests = testGroup "complex tests"
  [
     testCase "And of two vars one evaluating to and other to or" $
       (evaluate (NAryOperator And [Operand (var "a"), Operand (var "b")])
         [(Variable "a", NAryOperator And [Operand (var "b"), Operand Truth]),
           (Variable "b", NAryOperator Or [Operand Truth, Operand (var "c")])]) @?=
         (Operand Truth)
  ]
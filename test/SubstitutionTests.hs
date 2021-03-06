module SubstitutionTests (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Helpers.ExpressionCreators

import Language
import Substitution

tests = testGroup "Variable substitutions"
         [
         testCase "Constant expression has no variables" $
           (substitute (Operand Truth) []) @?= (Operand Truth),
         testCase "Constant expression yields same even if variables are in substitution list" $
           (substitute (Operand Truth) [(Variable "b", Operand Truth)]) @?= (Operand Truth),
         testCase "No variables yield no substitutions" $
           (substitute (Operand (var "a")) []) @?= (Operand (var "a")),
         testCase "No existing variables yield no substitutions" $
           (substitute (Operand (var "a")) [(Variable "b", Operand Truth)]) @?= (Operand (var "a")),
         testCase "Single variable is properly substituted" $
           (substitute (Operand (var "a")) [(Variable "a", Operand Truth)]) @?= (Operand Truth),
         testCase "Single variable is properly substituted" $
           (substitute (UnaryOperator Not (Operand (var "a"))) [(Variable "a", Operand Truth)]) @?=
            (UnaryOperator Not (Operand Truth)),
         testCase "Single variable is properly substituted in two places" $
           (substitute (NAryOperator And [Operand (var "a"), Operand (var "a")]) [(Variable "a", Operand Truth)]) @?=
            (NAryOperator And [Operand Truth, Operand Truth]),
         testCase "Multiple variables are properly substituted even with more complex expressions" $
           (substitute (NAryOperator And [Operand (var "a"), Operand (var "b")])
             [(Variable "a", Operand Truth), (Variable "b", UnaryOperator Not (Operand Truth))]) @?=
               (NAryOperator And [Operand Truth, UnaryOperator Not (Operand Truth)]),
         testCase "Substitution works one-by-one from the beginning of list" $
            (substitute (NAryOperator And [Operand (var "a"), Operand (var "b")])
              [(Variable "a", Operand (var "b")), (Variable "b", UnaryOperator Not (Operand Truth))]) @?=
                (NAryOperator And [UnaryOperator Not (Operand Truth), UnaryOperator Not (Operand Truth)]),
         testCase "From the beginning, really. Totally not possible to form recurrence" $
            (substitute (NAryOperator And [Operand (var "a"), Operand (var "b")])
              [(Variable "a", Operand Truth), (Variable "b", Operand (var "a"))]) @?=
                (NAryOperator And [Operand Truth, Operand (var "a")])
         ]
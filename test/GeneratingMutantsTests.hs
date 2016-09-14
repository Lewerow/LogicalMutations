module GeneratingMutantsTests (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Set as S (fromList)

import Helpers.ExpressionCreators

import Language
import Mutation

tests = testGroup "Mutant generation"
     [
         testCase "Single operand can take two forms" $ (generateAllMutants maxMC (Operand Truth)) @?= [
             UnaryOperator Yes (Operand Truth), UnaryOperator Not (Operand Truth)
         ],
         testCase "Single variable can take four forms" $ (S.fromList $ generateAllMutants maxMC (Operand (var "a"))) @?=
             S.fromList [
                 UnaryOperator Yes (Operand Truth),
                 UnaryOperator Not (Operand Truth),
                 UnaryOperator Yes (Operand (var "a")),
                 UnaryOperator Not (Operand (var "a"))
             ],
         testCase "Single NAry operator is translated to three" $ (S.fromList $ generateAllMutants maxMC
             (NAryOperator And [Operand (var "a")])) @?= S.fromList [
                UnaryOperator Yes $ NAryOperator And [UnaryOperator Yes (Operand (var "a"))],
                UnaryOperator Yes $ NAryOperator And [UnaryOperator Not (Operand (var "a"))],
                UnaryOperator Yes $ NAryOperator And [UnaryOperator Yes (Operand Truth)],
                UnaryOperator Yes $ NAryOperator And [UnaryOperator Not (Operand Truth)],
                UnaryOperator Yes $ NAryOperator Or [UnaryOperator Yes (Operand (var "a"))],
                UnaryOperator Yes $ NAryOperator Or [UnaryOperator Not (Operand (var "a"))],
                UnaryOperator Yes $ NAryOperator Or [UnaryOperator Yes (Operand Truth)],
                UnaryOperator Yes $ NAryOperator Or [UnaryOperator Not (Operand Truth)],
                UnaryOperator Yes $ NAryOperator Xor [UnaryOperator Yes (Operand (var "a"))],
                UnaryOperator Yes $ NAryOperator Xor [UnaryOperator Not (Operand (var "a"))],
                UnaryOperator Yes $ NAryOperator Xor [UnaryOperator Yes (Operand Truth)],
                UnaryOperator Yes $ NAryOperator Xor [UnaryOperator Not (Operand Truth)],
                UnaryOperator Not $ NAryOperator And [UnaryOperator Yes (Operand (var "a"))],
                UnaryOperator Not $ NAryOperator And [UnaryOperator Not (Operand (var "a"))],
                UnaryOperator Not $ NAryOperator And [UnaryOperator Yes (Operand Truth)],
                UnaryOperator Not $ NAryOperator And [UnaryOperator Not (Operand Truth)],
                UnaryOperator Not $ NAryOperator Or [UnaryOperator Yes (Operand (var "a"))],
                UnaryOperator Not $ NAryOperator Or [UnaryOperator Not (Operand (var "a"))],
                UnaryOperator Not $ NAryOperator Or [UnaryOperator Yes (Operand Truth)],
                UnaryOperator Not $ NAryOperator Or [UnaryOperator Not (Operand Truth)],
                UnaryOperator Not $ NAryOperator Xor [UnaryOperator Yes (Operand (var "a"))],
                UnaryOperator Not $ NAryOperator Xor [UnaryOperator Not (Operand (var "a"))],
                UnaryOperator Not $ NAryOperator Xor [UnaryOperator Yes (Operand Truth)],
                UnaryOperator Not $ NAryOperator Xor [UnaryOperator Not (Operand Truth)]
             ],
         testCase "Single NAry operator can be translated to two" $ (S.fromList $ generateAllMutants noXorMC
             (NAryOperator And [Operand (var "a")])) @?= S.fromList [
                UnaryOperator Yes $ NAryOperator And [UnaryOperator Yes (Operand (var "a"))],
                UnaryOperator Yes $ NAryOperator And [UnaryOperator Not (Operand (var "a"))],
                UnaryOperator Yes $ NAryOperator And [UnaryOperator Yes (Operand Truth)],
                UnaryOperator Yes $ NAryOperator And [UnaryOperator Not (Operand Truth)],
                UnaryOperator Yes $ NAryOperator Or [UnaryOperator Yes (Operand (var "a"))],
                UnaryOperator Yes $ NAryOperator Or [UnaryOperator Not (Operand (var "a"))],
                UnaryOperator Yes $ NAryOperator Or [UnaryOperator Yes (Operand Truth)],
                UnaryOperator Yes $ NAryOperator Or [UnaryOperator Not (Operand Truth)],
                UnaryOperator Not $ NAryOperator And [UnaryOperator Yes (Operand (var "a"))],
                UnaryOperator Not $ NAryOperator And [UnaryOperator Not (Operand (var "a"))],
                UnaryOperator Not $ NAryOperator And [UnaryOperator Yes (Operand Truth)],
                UnaryOperator Not $ NAryOperator And [UnaryOperator Not (Operand Truth)],
                UnaryOperator Not $ NAryOperator Or [UnaryOperator Yes (Operand (var "a"))],
                UnaryOperator Not $ NAryOperator Or [UnaryOperator Not (Operand (var "a"))],
                UnaryOperator Not $ NAryOperator Or [UnaryOperator Yes (Operand Truth)],
                UnaryOperator Not $ NAryOperator Or [UnaryOperator Not (Operand Truth)]
             ],
         testCase "Unary operator doesn't change a thing" $ (S.fromList $ generateAllMutants noXorMC
             (UnaryOperator Not (NAryOperator And [Operand (var "a")]))) @?= S.fromList [
                UnaryOperator Yes $ NAryOperator And [UnaryOperator Yes (Operand (var "a"))],
                UnaryOperator Yes $ NAryOperator And [UnaryOperator Not (Operand (var "a"))],
                UnaryOperator Yes $ NAryOperator And [UnaryOperator Yes (Operand Truth)],
                UnaryOperator Yes $ NAryOperator And [UnaryOperator Not (Operand Truth)],
                UnaryOperator Yes $ NAryOperator Or [UnaryOperator Yes (Operand (var "a"))],
                UnaryOperator Yes $ NAryOperator Or [UnaryOperator Not (Operand (var "a"))],
                UnaryOperator Yes $ NAryOperator Or [UnaryOperator Yes (Operand Truth)],
                UnaryOperator Yes $ NAryOperator Or [UnaryOperator Not (Operand Truth)],
                UnaryOperator Not $ NAryOperator And [UnaryOperator Yes (Operand (var "a"))],
                UnaryOperator Not $ NAryOperator And [UnaryOperator Not (Operand (var "a"))],
                UnaryOperator Not $ NAryOperator And [UnaryOperator Yes (Operand Truth)],
                UnaryOperator Not $ NAryOperator And [UnaryOperator Not (Operand Truth)],
                UnaryOperator Not $ NAryOperator Or [UnaryOperator Yes (Operand (var "a"))],
                UnaryOperator Not $ NAryOperator Or [UnaryOperator Not (Operand (var "a"))],
                UnaryOperator Not $ NAryOperator Or [UnaryOperator Yes (Operand Truth)],
                UnaryOperator Not $ NAryOperator Or [UnaryOperator Not (Operand Truth)]
             ]
     ]
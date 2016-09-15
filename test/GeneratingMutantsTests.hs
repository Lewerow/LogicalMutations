module GeneratingMutantsTests (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Set as S (fromList)

import Helpers.ExpressionCreators

import Language
import Mutation
import MutationConfiguration

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
             (NAryOperator And [Operand (var "a")])) @?= S.fromList (allNAry And ["a"] ++ allNAry Or ["a"] ++ allNAry Xor ["a"]),

         testCase "Single NAry operator can be translated to two" $ (S.fromList $ generateAllMutants noXorMC
             (NAryOperator And [Operand (var "a")])) @?= S.fromList (allNAry And ["a"] ++ allNAry Or ["a"]),

         testCase "Unary operator doesn't change a thing" $ (S.fromList $ generateAllMutants noXorMC
             (UnaryOperator Not (NAryOperator And [Operand (var "a")]))) @?=
                 S.fromList (allNAry And ["a"] ++ allNAry Or ["a"]),

         testCase "allNAry returns allNAry" $ (S.fromList $ allNAry And ["a"]) @?= S.fromList [
                 UnaryOperator Yes $ NAryOperator And [UnaryOperator Yes (Operand (var "a"))],
                 UnaryOperator Yes $ NAryOperator And [UnaryOperator Not (Operand (var "a"))],
                 UnaryOperator Yes $ NAryOperator And [UnaryOperator Yes (Operand Truth)],
                 UnaryOperator Yes $ NAryOperator And [UnaryOperator Not (Operand Truth)],
                 UnaryOperator Not $ NAryOperator And [UnaryOperator Yes (Operand (var "a"))],
                 UnaryOperator Not $ NAryOperator And [UnaryOperator Not (Operand (var "a"))],
                 UnaryOperator Not $ NAryOperator And [UnaryOperator Yes (Operand Truth)],
                 UnaryOperator Not $ NAryOperator And [UnaryOperator Not (Operand Truth)]
             ],

         testCase "Mutation doesn't yield duplicates" $ (S.fromList $ generateAllMutants (onlyMC And)
             (NAryOperator And [Operand (var "a"), Operand (var "b")])) @?=
                 S.fromList [
                     UnaryOperator Yes $ NAryOperator And [UnaryOperator Yes (Operand (var "a")), UnaryOperator Yes (Operand (var "a"))],
                     UnaryOperator Yes $ NAryOperator And [UnaryOperator Yes (Operand (var "a")), UnaryOperator Yes (Operand (var "b"))],
                     UnaryOperator Yes $ NAryOperator And [UnaryOperator Yes (Operand (var "b")), UnaryOperator Yes (Operand (var "a"))],
                     UnaryOperator Yes $ NAryOperator And [UnaryOperator Yes (Operand (var "b")), UnaryOperator Yes (Operand (var "b"))],
                     UnaryOperator Yes $ NAryOperator And [UnaryOperator Yes (Operand (var "a")), UnaryOperator Yes (Operand Truth)],
                     UnaryOperator Yes $ NAryOperator And [UnaryOperator Yes (Operand Truth), UnaryOperator Yes (Operand Truth)],
                     UnaryOperator Yes $ NAryOperator And [UnaryOperator Yes (Operand Truth), UnaryOperator Yes (Operand (var "a"))],
                     UnaryOperator Yes $ NAryOperator And [UnaryOperator Yes (Operand Truth), UnaryOperator Yes (Operand (var "b"))],
                     UnaryOperator Yes $ NAryOperator And [UnaryOperator Yes (Operand (var "b")), UnaryOperator Yes (Operand Truth)]
                 ],

         testCase "Mutation doesn't yield duplicates" $ (S.fromList $ generateAllMutants noVarChangeNoXor
             (NAryOperator And [Operand (var "a"), Operand (var "b")])) @?=
                 S.fromList [
                     UnaryOperator Yes $ NAryOperator And [UnaryOperator Yes (Operand (var "a")), UnaryOperator Yes (Operand (var "b"))],
                     UnaryOperator Not $ NAryOperator And [UnaryOperator Yes (Operand (var "a")), UnaryOperator Yes (Operand (var "b"))],
                     UnaryOperator Yes $ NAryOperator And [UnaryOperator Not (Operand (var "a")), UnaryOperator Yes (Operand (var "b"))],
                     UnaryOperator Not $ NAryOperator And [UnaryOperator Not (Operand (var "a")), UnaryOperator Yes (Operand (var "b"))],
                     UnaryOperator Yes $ NAryOperator And [UnaryOperator Yes (Operand (var "a")), UnaryOperator Not (Operand (var "b"))],
                     UnaryOperator Not $ NAryOperator And [UnaryOperator Yes (Operand (var "a")), UnaryOperator Not (Operand (var "b"))],
                     UnaryOperator Yes $ NAryOperator And [UnaryOperator Not (Operand (var "a")), UnaryOperator Not (Operand (var "b"))],
                     UnaryOperator Not $ NAryOperator And [UnaryOperator Not (Operand (var "a")), UnaryOperator Not (Operand (var "b"))],
                     UnaryOperator Yes $ NAryOperator Or [UnaryOperator Yes (Operand (var "a")), UnaryOperator Yes (Operand (var "b"))],
                     UnaryOperator Not $ NAryOperator Or [UnaryOperator Yes (Operand (var "a")), UnaryOperator Yes (Operand (var "b"))],
                     UnaryOperator Yes $ NAryOperator Or [UnaryOperator Not (Operand (var "a")), UnaryOperator Yes (Operand (var "b"))],
                     UnaryOperator Not $ NAryOperator Or [UnaryOperator Not (Operand (var "a")), UnaryOperator Yes (Operand (var "b"))],
                     UnaryOperator Yes $ NAryOperator Or [UnaryOperator Yes (Operand (var "a")), UnaryOperator Not (Operand (var "b"))],
                     UnaryOperator Not $ NAryOperator Or [UnaryOperator Yes (Operand (var "a")), UnaryOperator Not (Operand (var "b"))],
                     UnaryOperator Yes $ NAryOperator Or [UnaryOperator Not (Operand (var "a")), UnaryOperator Not (Operand (var "b"))],
                     UnaryOperator Not $ NAryOperator Or [UnaryOperator Not (Operand (var "a")), UnaryOperator Not (Operand (var "b"))]
                 ]
     ]
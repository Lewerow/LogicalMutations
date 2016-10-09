module TreeficationTests (tests) where

import Language
import Normalization
import Treefication

import Helpers.ExpressionCreators

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Map.Strict as Map


complexExpression = NAryOperator And [
        NAryOperator Xor [
          NAryOperator Or [Operand Truth, Operand (var "a")],
          UnaryOperator Not (Operand (var "b")),
          UnaryOperator Yes (Operand (var "c"))
        ],
        NAryOperator And [
          Operand (var "a"),
          Operand (var "c")
        ]
      ]

tests = testGroup "Treefication"
         [
         testCase "Single operand is treefied to one node" $
             treeficate (Operand Truth) @?= (Tree (NodeId 0) [], Map.fromList [(NodeId 0, NodeInfo (TerminalNode Truth) KE)]),
         testCase "Unary operator is treefied to two nodes" $
             treeficate (UnaryOperator Not (Operand Truth)) @?=
               (Tree (NodeId 0) [Tree (NodeId 1) []], Map.fromList [
                 (NodeId 0, NodeInfo (UnaryNode Not) KE),
                 (NodeId 1, NodeInfo (TerminalNode Truth) KE)
               ]),
         testCase "NAry operator with two arguments (one negated) is treefied to four nodes" $
             treeficate (NAryOperator And [UnaryOperator Not (Operand Truth), Operand (var "a")]) @?= (Tree (NodeId 0) [
               Tree (NodeId 1) [Tree (NodeId 2) []],
               Tree (NodeId 3) []
             ], Map.fromList [
               (NodeId 0, NodeInfo (NAryNode And) KE),
               (NodeId 1, NodeInfo (UnaryNode Not) KE),
               (NodeId 2, NodeInfo (TerminalNode Truth) KE),
               (NodeId 3, NodeInfo (TerminalNode (var "a")) KE)
             ]),
         testCase "Complicated NAry operators yield proper results" $
             treeficate (NAryOperator And [
               NAryOperator Xor [
                 NAryOperator Or [Operand Truth, Operand (var "a")],
                 UnaryOperator Not (Operand (var "b")),
                 UnaryOperator Yes (Operand (var "c"))
               ],
               NAryOperator And [
                 Operand (var "a"),
                 Operand (var "c")
               ]
             ]) @?= (Tree (NodeId 0) [
               Tree (NodeId 1) [
                 Tree (NodeId 2) [Tree (NodeId 3) [], Tree (NodeId 4) []],
                 Tree (NodeId 5) [Tree (NodeId 6) []],
                 Tree (NodeId 7) [Tree (NodeId 8) []]
               ],
               Tree (NodeId 9) [
                 Tree (NodeId 10) [],
                 Tree (NodeId 11) []
               ]
             ], Map.fromList [
               (NodeId 0, NodeInfo (NAryNode And) KE),
               (NodeId 1, NodeInfo (NAryNode Xor) KE),
               (NodeId 2, NodeInfo (NAryNode Or) KE),
               (NodeId 3, NodeInfo (TerminalNode Truth) KE),
               (NodeId 4, NodeInfo (TerminalNode (var "a")) KE),
               (NodeId 5, NodeInfo (UnaryNode Not) KE),
               (NodeId 6, NodeInfo (TerminalNode (var "b")) KE),
               (NodeId 7, NodeInfo (UnaryNode Yes) KE),
               (NodeId 8, NodeInfo (TerminalNode (var "c")) KE),
               (NodeId 9, NodeInfo (NAryNode And) KE),
               (NodeId 10, NodeInfo (TerminalNode (var "a")) KE),
               (NodeId 11, NodeInfo (TerminalNode (var "c")) KE)
             ]),
         testCase "Untreefication reverses treefication" $
             (untreeficate $ treeficate complexExpression) @?= complexExpression
         ]
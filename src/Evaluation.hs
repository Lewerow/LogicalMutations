module Evaluation where

import Language
import Substitution

data CallType = First | Next deriving (Eq, Show)

evaluate :: Expression -> [(Variable, Expression)] -> Expression
evaluate expr vars = simplify $ substitute expr vars

simplify :: Expression -> Expression
simplify expr = case expr of
  UnaryOperator Yes a -> a
  BinaryOperator And a b -> solveAnd a b
  BinaryOperator Or a b -> solveOr a b
  BinaryOperator Xor a b -> solveXor a b
  otherwise -> expr

solveAnd :: Expression -> Expression -> Expression
solveAnd a b = solveAndHelper First a b
  where
  solveAndHelper _ (Operand Truth) a = simplify a
  solveAndHelper _ a (Operand Truth) = simplify a
  solveAndHelper _ (UnaryOperator Not (Operand Truth)) _ = UnaryOperator Not (Operand Truth)
  solveAndHelper _ _ (UnaryOperator Not (Operand Truth)) = UnaryOperator Not (Operand Truth)
  solveAndHelper n a b
   | a == b = a
   | n == First = solveAndHelper Next (simplify a) (simplify b)
   | otherwise = BinaryOperator And a b

solveOr :: Expression -> Expression -> Expression
solveOr a b = solveOrHelper First a b
  where
  solveOrHelper _ (Operand Truth) _ = Operand Truth
  solveOrHelper _ _ (Operand Truth) = Operand Truth
  solveOrHelper _ (UnaryOperator Not (Operand Truth)) a = simplify a
  solveOrHelper _ a (UnaryOperator Not (Operand Truth)) = simplify a
  solveOrHelper n a b
   | a == b = a
   | n == First = solveOrHelper Next (simplify a) (simplify b)
   | otherwise = BinaryOperator Or a b

solveXor :: Expression -> Expression -> Expression
solveXor a b = solveXorHelper First a b
  where
  solveXorHelper n a b
   | a == b = (UnaryOperator Not (Operand Truth))
   | a == (UnaryOperator Not (Operand Truth)) && b == (Operand Truth) = Operand Truth
   | b == (UnaryOperator Not (Operand Truth)) && a == (Operand Truth) = Operand Truth
   | n == First = solveXorHelper Next (simplify a) (simplify b)
   | otherwise = BinaryOperator Xor a b
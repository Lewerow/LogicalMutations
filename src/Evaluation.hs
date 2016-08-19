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
solveAnd = solveBinary (\a -> simplify a, \_ -> UnaryOperator Not (Operand Truth)) And

solveOr :: Expression -> Expression -> Expression
solveOr = solveBinary (\_ -> Operand Truth, \a -> simplify a) Or

type ConstantSolution = Expression -> Expression
solveBinary :: (ConstantSolution, ConstantSolution) -> BinaryOperatorType -> Expression -> Expression -> Expression
solveBinary f o a b = solveBinaryHelper First a b
  where
  solveBinaryHelper _ (Operand Truth) a = fst f $ a
  solveBinaryHelper _ a (Operand Truth) = fst f $ a
  solveBinaryHelper _ (UnaryOperator Not (Operand Truth)) a = snd f $ a
  solveBinaryHelper _ a (UnaryOperator Not (Operand Truth)) = snd f $ a
  solveBinaryHelper n a b
    | a == b = a
    | n == First = solveBinaryHelper Next (simplify a) (simplify b)
    | otherwise = BinaryOperator o a b

solveXor :: Expression -> Expression -> Expression
solveXor a b = solveXorHelper First a b
  where
  solveXorHelper n a b
   | a == b = (UnaryOperator Not (Operand Truth))
   | a == (UnaryOperator Not (Operand Truth)) && b == (Operand Truth) = Operand Truth
   | b == (UnaryOperator Not (Operand Truth)) && a == (Operand Truth) = Operand Truth
   | n == First = solveXorHelper Next (simplify a) (simplify b)
   | otherwise = BinaryOperator Xor a b
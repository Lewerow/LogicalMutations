module Language where

-- data Entity = BinaryOperator Text Entity Entity | UnaryOperator Text Entity | Operand Text

data BinaryOperatorType = And | Or | Xor
data UnaryOperatorType = Not | Yes
data LogicalType = TRUE | FALSE | Var Text

data Expression = BinaryOperator BinaryOperatorType Expression Expression |
  UnaryOperator UnaryOperatorType Expression |
  Operand LogicalType

data Logical = TRUE | FALSE | Var Text


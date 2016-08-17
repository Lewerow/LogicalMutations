module Language where

-- data Entity = BinaryOperator Text Entity Entity | UnaryOperator Text Entity | Operand Text

data BinaryOperatorType = And | Or | Xor deriving (Show, Eq)
data UnaryOperatorType = Not | Yes deriving (Show, Eq)
data LogicalType = TRUE | FALSE | Var String deriving (Show, Eq, Ord)

data Expression = BinaryOperator BinaryOperatorType Expression Expression |
  UnaryOperator UnaryOperatorType Expression |
  Operand LogicalType deriving (Show, Eq)


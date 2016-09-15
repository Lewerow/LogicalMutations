module Language where

newtype Variable = Variable String deriving (Show, Eq, Ord)

data NAryOperatorType = And | Or | Xor deriving (Show, Eq, Ord)
data UnaryOperatorType = Not | Yes deriving (Show, Eq, Ord)
data LogicalType = Truth | Var Variable deriving (Show, Eq, Ord) -- no false - is achieved by Not Truth

data Expression = NAryOperator NAryOperatorType [Expression] |
  UnaryOperator UnaryOperatorType Expression |
  Operand LogicalType deriving (Show, Eq, Ord)
module Language where

-- data Entity = BinaryOperator Text Entity Entity | UnaryOperator Text Entity | Operand Text

newtype Variable = Variable String deriving (Show, Eq, Ord)

data BinaryOperatorType = And | Or | Xor deriving (Show, Eq)
data UnaryOperatorType = Not | Yes deriving (Show, Eq)
data LogicalType = Truth | Var Variable deriving (Show, Eq, Ord) -- no false - is achieved by Not Truth

data Expression = BinaryOperator BinaryOperatorType Expression Expression |
  UnaryOperator UnaryOperatorType Expression |
  Operand LogicalType deriving (Show, Eq)

binaryOperatorsCount :: Int
binaryOperatorsCount = 3

unaryOperatorsCount :: Int
unaryOperatorsCount = 2
module Mutation where

import Data.Set

import Language

countVariables :: Expression -> Int
countVariables a = size $ countVariablesHelper a where
  countVariablesHelper :: Expression -> Set LogicalType
  countVariablesHelper (Operand (Var a)) = singleton (Var a)
  countVariablesHelper (Operand a) = empty
  countVariablesHelper (UnaryOperator a b) = countVariablesHelper b
  countVariablesHelper (BinaryOperator a b c) = (countVariablesHelper b) `union` (countVariablesHelper c)

countForms :: Expression -> Int
countForms a = 2 + countVariables a

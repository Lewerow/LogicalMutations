module Mutation where

import Data.Set

import Language
import Normalization

countVariables :: Expression -> Int
countVariables a = size $ countVariablesHelper a where
  countVariablesHelper :: Expression -> Set LogicalType
  countVariablesHelper (Operand (Var a)) = singleton (Var a)
  countVariablesHelper (Operand a) = empty
  countVariablesHelper (UnaryOperator a b) = countVariablesHelper b
  countVariablesHelper (BinaryOperator a b c) = (countVariablesHelper b) `union` (countVariablesHelper c)

countForms :: Expression -> Int
countForms a = countFormsHelper $ normalize a

countFormsHelper :: Expression -> Int
countFormsHelper a = case a of
  (Operand b) -> operandOptionsCount
  (UnaryOperator b c) -> unaryOperatorsCount * countFormsHelper c
  (BinaryOperator b c d) -> binaryOperatorsCount * (countFormsHelper c) * (countFormsHelper d)
  where
    operandOptionsCount = 1 + countVariables a

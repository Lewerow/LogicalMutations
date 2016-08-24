module Mutation where

import qualified Data.Set as S (singleton, empty, union, size, Set)

import Language
import Normalization

countVariables :: Expression -> Int
countVariables a = S.size $ countVariablesHelper a where
  countVariablesHelper :: Expression -> S.Set LogicalType
  countVariablesHelper (Operand (Var a)) = S.singleton (Var a)
  countVariablesHelper (Operand a) = S.empty
  countVariablesHelper (UnaryOperator a b) = countVariablesHelper b
  countVariablesHelper (NAryOperator a b) = foldl S.union S.empty $ map countVariablesHelper b

countForms :: Expression -> Int
countForms a = countFormsHelper $ normalize a

countFormsHelper :: Expression -> Int
countFormsHelper a = case a of
  (Operand b) -> operandOptionsCount
  (UnaryOperator b c) -> unaryOperatorsCount * countFormsHelper c
  (NAryOperator b c) -> binaryOperatorsCount * (foldl (*) 1 $ map countFormsHelper c)
  where
    operandOptionsCount = 1 + countVariables a

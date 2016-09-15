module VariableHelpers where

import qualified Data.Set as S (singleton, empty, union, size, Set)

import Language

countVariables :: Expression -> Int
countVariables a = S.size $ getVariables a

getVariables :: Expression -> S.Set LogicalType
getVariables (Operand (Var a)) = S.singleton (Var a)
getVariables (Operand a) = S.empty
getVariables (UnaryOperator a b) = getVariables b
getVariables (NAryOperator a b) = foldl S.union S.empty $ map getVariables b

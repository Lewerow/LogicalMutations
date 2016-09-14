module Helpers.ExpressionCreators where

import Control.Monad (replicateM)

import Language

var :: String -> LogicalType
var a = Var $ Variable a


allNAry :: NAryOperatorType -> [String] -> [Expression]
allNAry op vars = [unary $ (NAryOperator op) varset | unary <- unaries, varset <-
  [map u v | v <- replicateM (length vars) $ Operand Truth : map (Operand . var) vars, u <- unaries]]
  where
    unaries = [UnaryOperator Yes, UnaryOperator Not]
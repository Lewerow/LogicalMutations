module Helpers.ExpressionCreators where

import Language

var :: String -> LogicalType
var a = Var $ Variable a
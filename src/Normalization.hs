module Normalization where

import Language

normalize :: Expression -> Expression
normalize (Operand a) = UnaryOperator Yes (Operand a)
normalize (UnaryOperator a (Operand b)) = UnaryOperator a (Operand b)
normalize (UnaryOperator a b) = UnaryOperator a (normalize b)
normalize (NAryOperator a exprs) = UnaryOperator Yes (NAryOperator a $ map normalize exprs)
module Substitution where

import Language

substitute :: Expression -> [(Variable, Expression)] -> Expression
substitute expr vars = foldl substituteOne expr vars

substituteOne :: Expression -> (Variable, Expression) -> Expression
substituteOne expr sub = case expr of
  Operand (Var b) -> if b == fst sub then snd sub else Operand $ Var b
  UnaryOperator op ex -> UnaryOperator op $ substituteOne ex sub
  NAryOperator op exprs -> NAryOperator op $ map (\e -> substituteOne e sub) exprs
  Operand Truth -> Operand Truth
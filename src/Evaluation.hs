module Evaluation where

import Language
import Substitution

import Data.List (nub, sort)
import qualified Data.Map.Strict as M (Map, empty, member, delete, alter, keys, filter)

data RecursionLevel = First | Next deriving (Eq, Show)

evaluate :: Expression -> [(Variable, Expression)] -> Expression
evaluate expr vars = simplify $ substitute expr vars

simplify :: Expression -> Expression
simplify expr = case expr of
  UnaryOperator Yes a -> a
  NAryOperator And a -> solveAnd a
  NAryOperator Or a -> solveOr a
  NAryOperator Xor a -> solveXor a
  otherwise -> expr

solveAnd = solveBinary (Operand Truth, UnaryOperator Not (Operand Truth), And)
solveOr = solveBinary (UnaryOperator Not (Operand Truth), Operand Truth, Or)

solveBinary (defaultValue, terminatingValue, operator) expressions = solveBinaryHelper First leftValues
  where
    leftValues = filter (/= defaultValue) $ map simplify expressions
    solveBinaryHelper _ [] = defaultValue
    solveBinaryHelper _ [a] = a
    solveBinaryHelper First values = if any (== terminatingValue) values then terminatingValue
      else solveBinaryHelper Next $ nub values
    solveBinaryHelper Next values = NAryOperator operator values

solveXor a = decideOnSingles filtered
  where
    simple :: [Expression]
    simple = map simplify a
    incrementCount :: Maybe Int -> Maybe Int
    incrementCount x = Just $ maybe 1 (+1) x
    expressionCounts :: M.Map Expression Int
    expressionCounts = foldl (flip (M.alter incrementCount)) M.empty simple
    simplified = M.filter (\x -> x `mod` 2 == 1) expressionCounts
    filtered = M.keys $ if M.member (Operand Truth) simplified && M.member (UnaryOperator Not (Operand Truth)) simplified
      then M.delete (UnaryOperator Not (Operand Truth)) simplified else simplified
    decideOnSingles [] = UnaryOperator Not (Operand Truth)
    decideOnSingles [Operand Truth] = Operand Truth
    decideOnSingles [UnaryOperator Not (Operand Truth)] = Operand Truth
    decideOnSingles filtered = NAryOperator Xor $ sort filtered
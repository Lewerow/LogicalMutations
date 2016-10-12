module OptimizedMutation where

import qualified Data.Set as Set
import Control.Monad (replicateM)

import Language
import Treefication
import Evaluation
import VariableHelpers


generateInputSpace :: Expression -> [[(Variable, Expression)]]
generateInputSpace expr = inputSequence where
    variables = Set.toList $ getVariables expr
    variablesCount = length variables
    availableValues = replicateM variablesCount [Operand Truth, UnaryOperator Not (Operand Truth)]
    inputSequence = map (\p -> zip (fst p) (snd p)) $ zip (repeat variables) availableValues

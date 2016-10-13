module OptimizedMutation where

import qualified Data.Set as Set
import Control.Monad (replicateM)
import Control.Monad.State

import Language
import Treefication
import Evaluation
import VariableHelpers

data OptimizedMutationConfiguration = OMC
data OptimizedMutationReport = OMR
data SingleMutantReport = SMR

data Mutation = Mutation {
  mutatedNode :: NodeId,
  expressionVersion :: Int
}

data MutationProcedure a = MP {
  runMutation :: a
}

type MutationState = StateT MutationState MutationProcedure

instance Monad MutationProcedure MutationState where
  a >>= b = 
  a >> b = b
  return a = MP a


generateInputSpace :: Expression -> [[(Variable, Expression)]]
generateInputSpace expr = inputSequence where
    variables = Set.toList $ getVariables expr
    variablesCount = length variables
    availableValues = replicateM variablesCount [Operand Truth, UnaryOperator Not (Operand Truth)]
    inputSequence = map (\p -> zip (fst p) (snd p)) $ zip (repeat variables) availableValues


analyzeMutant :: OptimizedMutationConfiguration -> StructuredExpression -> [Mutation] -> State
analyzeMutant conf cache expr mutations =
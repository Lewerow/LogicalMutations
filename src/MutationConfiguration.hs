module MutationConfiguration where

import qualified Data.Set as S (toList)

import Language
import VariableHelpers

data MutationConfiguration = MC {
  naries:: [NAryOperatorType],
  unaries:: [UnaryOperatorType],
  operands:: [LogicalType]
}

maxMC :: Expression -> MutationConfiguration
maxMC expr = MC [And, Or, Xor] [Yes, Not] (Truth : (S.toList $ getVariables expr))

noXorMC :: Expression -> MutationConfiguration
noXorMC expr = MC [And, Or] [Yes, Not] (Truth : (S.toList $ getVariables expr))

onlyMC :: NAryOperatorType -> Expression -> MutationConfiguration
onlyMC op expr = MC [op] [Yes] (Truth : (S.toList $ getVariables expr))

noVarChangeNoXor :: Expression -> MutationConfiguration
noVarChangeNoXor _ = MC [And, Or] [Yes, Not] []
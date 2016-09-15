module MutationConfiguration where

import qualified Data.Set as S (toList)

import Language
import VariableHelpers

data MutationConfiguration = MC {
  naries:: [NAryOperatorType],
  unaries:: [UnaryOperatorType],
  operands:: [LogicalType]
}

getVars :: Expression -> [LogicalType]
getVars expr = map Var $ S.toList $ getVariables expr

maxMC :: Expression -> MutationConfiguration
maxMC expr = MC [And, Or, Xor] [Yes, Not] (Truth : (getVars expr))

noXorMC :: Expression -> MutationConfiguration
noXorMC expr = MC [And, Or] [Yes, Not] (Truth : (getVars expr))

onlyMC :: NAryOperatorType -> Expression -> MutationConfiguration
onlyMC op expr = MC [op] [Yes] (Truth : (getVars expr))

noVarChangeNoXor :: Expression -> MutationConfiguration
noVarChangeNoXor _ = MC [And, Or] [Yes, Not] []
import System.Exit
import Language
import MutationReport
import VariableHelpers
import Mutation
import MutationConfiguration

import Evaluation
import Data.Map.Strict as M (singleton, member)
import Data.Set as S (toList)
import Control.Monad (replicateM)
import Data.List(foldl')

evaluateMutant :: [(Expression, [(Variable, Expression)])] -> MutationReport -> Expression -> MutationReport
evaluateMutant originalTreeResults accumulatingReport mutatedTree =
    if simplified `M.member` (leftSimplified accumulatingReport) then simplified `addTo` accumulatingReport
    else appendNew simplified (firstUnequal originalTreeResults results) accumulatingReport
      where
        simplified = simplify mutatedTree
        results = map (\v -> (evaluate simplified v, v)) (map snd originalTreeResults)
        firstUnequal [] [] = []
        firstUnequal (oh:ot) (nh:nt) = if oh == nh then firstUnequal ot nt else snd nh


executeTests :: (Expression -> MutationConfiguration) -> Expression -> MutationReport
executeTests mc expr = foldl' (evaluateMutant originalTreeResults) reportDraft mutants
  where
    totalForms = countForms mc expr
    totalFunctions = 2 ^ (2 ^ variablesCount)
    variables = S.toList $ getVariables expr
    variablesCount = length variables
    tag = simplify expr
    reportDraft = MutationReport expr totalForms totalFunctions (M.singleton tag (MR 1 []))
    mutants = generateAllMutants mc expr
    originalTreeResults = map (\v -> (evaluate tag v, v)) inputSet
      where
        availableValues = replicateM variablesCount [Operand Truth, UnaryOperator Not (Operand Truth)]
        inputSet = map (\p -> zip (fst p) (snd p)) $ zip (repeat variables) availableValues




main = exitSuccess
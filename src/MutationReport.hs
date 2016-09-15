module MutationReport where

import Language
import Data.Map.Strict

data MutationResult = MR {
  treesCount :: Int,
  exampleDifference :: [(Variable, Expression)]
} deriving Show

data MutationReport = MutationReport {
  originalTree :: Expression,
  possibleStructures :: Int,
  possibleFunctions :: Int,
  leftSimplified :: Map Expression MutationResult
} deriving Show

addTo :: Expression -> MutationReport -> MutationReport
addTo expr rep = rep {leftSimplified = (adjust addTree expr (leftSimplified rep))}
  where
    addTree :: MutationResult -> MutationResult
    addTree mr = mr { treesCount = treesCount mr + 1 }

appendNew :: Expression -> [(Variable, Expression)] -> MutationReport -> MutationReport
appendNew expr vars rep = rep {leftSimplified = insert expr (MR 1 vars) (leftSimplified rep)}
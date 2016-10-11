module Mutation where

import Language
import Normalization
import MutationConfiguration
import VariableHelpers
import Treefication
import MutationReport

import qualified Data.Map.Strict as Map

countForms :: (Expression -> MutationConfiguration) -> Expression -> Int
countForms mc a = countFormsHelper (mc a) $ normalize a

countFormsHelper :: MutationConfiguration -> Expression -> Int
countFormsHelper mc a = case a of
  (Operand b) -> bounded $ length (operands mc)
  (UnaryOperator b c) -> (bounded $ length (unaries mc)) * countFormsHelper mc c
  (NAryOperator b c) -> (bounded $ length (naries mc)) * (foldl (*) 1 $ map (countFormsHelper mc) c)
  where bounded = max 1

data MutationPoint = MP Int deriving (Show, Eq, Ord)

data Annotation m = Annotation {
  originalValue :: m,
  availableValues :: [m],
  pointIndex :: MutationPoint
}

type NAOA = Annotation NAryOperatorType
type UOA = Annotation UnaryOperatorType
type OA = Annotation LogicalType

data AnnotatedExpression = ANAE NAOA [AnnotatedExpression] |
  AUE UOA AnnotatedExpression |
  AO OA

class Annotable a where
  availableMutations :: MutationConfiguration -> a -> [a]

instance Annotable NAryOperatorType where
  availableMutations mc op = filter (/= op) (naries mc)

instance Annotable UnaryOperatorType where
  availableMutations mc op = filter (/= op) (unaries mc)

instance Annotable LogicalType where
  availableMutations mc op = filter (/= op) (operands mc)

makeAnnotation :: Annotable a => MutationConfiguration -> a -> Annotation a
makeAnnotation mc op = Annotation op (availableMutations mc op) (MP 0)

annotate :: (Expression -> MutationConfiguration) -> Expression -> AnnotatedExpression
annotate mc expr = annotateHelper expr where
  conf = mc expr
  annotateHelper :: Expression -> AnnotatedExpression
  annotateHelper expr = case expr of
    NAryOperator b c -> ANAE (makeAnnotation conf b) (map annotateHelper c)
    UnaryOperator b c -> AUE (makeAnnotation conf b) (annotateHelper c)
    Operand c -> AO (makeAnnotation conf c)

generateAllMutants :: (Expression -> MutationConfiguration) -> Expression -> [Expression]
generateAllMutants mc expr = generate anno where
  anno = annotate mc (normalize expr)
  generate :: AnnotatedExpression -> [Expression]
  generate ae = case ae of
    ANAE ann exprs -> [NAryOperator oper elems | oper <- allOptions ann, elems <- allSubmutations exprs]
      where
        allSubmutations :: [AnnotatedExpression] -> [[Expression]]
        allSubmutations exprs = sequence $ map generate exprs
    AUE ann exp -> [UnaryOperator oper elem | oper <- allOptions ann, elem <- generate exp]
    AO ann -> map Operand $ allOptions ann

allOptions :: Annotation a -> [a]
allOptions a = (originalValue a) : (availableValues a)

optimizedMutation :: (Expression -> MutationConfiguration) -> Expression -> MutationReport
optimizedMutation configCreator expr = report
  where
    normalForm = normalize expr
    conf = configCreator normalForm
    structured = treeficate normalForm
    report = MutationReport expr 0 0 Map.empty
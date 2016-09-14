module Mutation where

import qualified Data.Set as S (singleton, empty, union, size, toList, Set)

import Language
import Normalization

countVariables :: Expression -> Int
countVariables a = S.size $ getVariables a

getVariables :: Expression -> S.Set LogicalType
getVariables (Operand (Var a)) = S.singleton (Var a)
getVariables (Operand a) = S.empty
getVariables (UnaryOperator a b) = getVariables b
getVariables (NAryOperator a b) = foldl S.union S.empty $ map getVariables b

countForms :: Expression -> Int
countForms a = countFormsHelper $ normalize a

countFormsHelper :: Expression -> Int
countFormsHelper a = case a of
  (Operand b) -> operandOptionsCount
  (UnaryOperator b c) -> unaryOperatorsCount * countFormsHelper c
  (NAryOperator b c) -> binaryOperatorsCount * (foldl (*) 1 $ map countFormsHelper c)
  where
    operandOptionsCount = 1 + countVariables a


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

data MutationConfiguration = MC {
  naries:: [NAryOperatorType],
  unaries:: [UnaryOperatorType],
  operands:: [LogicalType]
}

maxMC :: Expression -> MutationConfiguration
maxMC expr = MC [And, Or, Xor] [Yes, Not] (Truth : (S.toList $ getVariables expr))

noXorMC :: Expression -> MutationConfiguration
noXorMC expr = MC [And, Or] [Yes, Not] (Truth : (S.toList $ getVariables expr))

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
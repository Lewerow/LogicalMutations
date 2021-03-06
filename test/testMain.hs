import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.List
import Data.Ord

import qualified NormalizationTests as Norm
import qualified MutationTests as Mut
import qualified SubstitutionTests as Subs
import qualified EvaluationTests as Eval
import qualified GeneratingMutantsTests as Gen
import qualified TreeficationTests as Tree
import qualified OptimizedMutationTests as Opt


main = defaultMain tests


tests :: TestTree
tests = testGroup "Language tests" [Norm.tests, Mut.tests, Subs.tests, Eval.tests, Gen.tests, Tree.tests, Opt.tests]
-- tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [scProps, qcProps]

scProps = testGroup "(checked by SmallCheck)"
  [ SC.testProperty "sort == sort . reverse" $
      \list -> sort (list :: [Int]) == sort (reverse list)
  , SC.testProperty "Fermat's little theorem" $
      \x -> ((x :: Integer)^7 - x) `mod` 7 == 0
  -- the following property does not hold
  , SC.testProperty "Fermat's last theorem" $
      \x y z n ->
        (n :: Integer) >= 3 SC.==> x^n + y^n /= (z^n :: Integer)
  ]

qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "sort == sort . reverse" $
      \list -> sort (list :: [Int]) == sort (reverse list)
  , QC.testProperty "Fermat's little theorem" $
      \x -> ((x :: Integer)^7 - x) `mod` 7 == 0
  -- the following property does not hold
  , QC.testProperty "Fermat's last theorem" $
      \x y z n ->
        (n :: Integer) >= 3 QC.==> x^n + y^n /= (z^n :: Integer)
  ]

unitTests = testGroup "Unit tests"
  [ testCase "List comparison (different length)" $
      [1, 2, 3] `compare` [1,2] @?= GT

  -- the following test does not hold
  , testCase "List comparison (same length)" $
      [1, 2, 3] `compare` [1,2,2] @?= LT
  ]

prepend :: a -> [a] -> [a]
prepend elem list = elem : list

x = testGroup "prepend features" 
  [
    QC.testProperty "after prepend list is bigger" $ \list elem -> (length $ prepend elem (list :: [Int])) == length list + 1,
    QC.testProperty "after prepend becomes first element" $ \list elem -> (head $ prepend elem (list :: [Int])) == elem
  ]
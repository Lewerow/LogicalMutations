module Treefication where

import Language
import Data.Monoid ((<>))
import Data.List (foldl')
import Data.Maybe (fromMaybe, mapMaybe, listToMaybe)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

newtype NodeId = NodeId Int deriving (Eq, Ord, Show)


firstNodeId = NodeId 0
nextId :: NodeId -> NodeId
nextId (NodeId a) = NodeId (a + 1)

data TreeStructure = Tree NodeId [TreeStructure] deriving (Eq, Ord, Show)
data NodeDetails = TerminalNode LogicalType | UnaryNode UnaryOperatorType | NAryNode NAryOperatorType deriving (Eq, Ord, Show)


data KnownEvaluations = KE deriving (Eq, Ord, Show)
data NodeInfo = NodeInfo {
  details :: NodeDetails,
  knownEvaluations :: KnownEvaluations
} deriving (Eq, Ord, Show)
type Nodes = Map.Map NodeId NodeInfo
type StructuredExpression = (TreeStructure, Map.Map NodeId NodeInfo)

treeficate :: Expression -> StructuredExpression
treeficate expr = snd $ treeficateHelper expr firstNodeId
  where
  treeficateHelper :: Expression -> NodeId -> (NodeId, StructuredExpression)
  treeficateHelper (Operand t) id = (nextId id,
    (Tree id [], Map.fromList [(id, NodeInfo (TerminalNode t) KE)]))
  treeficateHelper (UnaryOperator t expr) id = (fst treeficated, (Tree id [fst $ snd $ treeficated],
    Map.fromList[(id, NodeInfo (UnaryNode t) KE)] <> (snd $ snd treeficated)))
      where
      treeficated = treeficateHelper expr (nextId id)

  treeficateHelper (NAryOperator t exprs) id = (fst nTreefied, (Tree id $ map fst $ reverse $ snd nTreefied,
    foldl' (<>) (Map.fromList [(id, NodeInfo (NAryNode t) KE)]) (map snd $ snd nTreefied)))
      where
        nTreefied :: (NodeId, [StructuredExpression])
        nTreefied = naryTreefication exprs $ nextId id
        naryTreefication :: [Expression] -> NodeId -> (NodeId, [StructuredExpression])
        naryTreefication exprs id = foldl' merger (id, []) exprs
          where
            merger acc expr = (fst treeficated, snd treeficated : snd acc)
              where
                treeficated = treeficateHelper expr $ fst acc

untreeficate :: StructuredExpression -> Expression
untreeficate tree = forcedUntreeficate tree Map.empty

forcedUntreeficate :: StructuredExpression -> Map.Map NodeId Expression -> Expression
forcedUntreeficate (tree, nodes) forcedLookupTable = forcedUntree tree
   where
     forcedUntree (Tree id subtrees) = fromMaybe defaultValue forcedValue
      where
        node = (Map.!) nodes id
        nodeType = details node
        forcedValue = Map.lookup id forcedLookupTable
        defaultValue = (case nodeType of
             TerminalNode t -> Operand t
             UnaryNode t -> UnaryOperator t $ forcedUntree $ head subtrees
             NAryNode t -> NAryOperator t $ map forcedUntree subtrees)

generateDependenciesTopDown :: StructuredExpression -> Map.Map NodeId (Set.Set NodeId)
generateDependenciesTopDown (tree, _) = snd $ generateDependenciesHelper tree
  where
    generateDependenciesHelper :: TreeStructure -> (Set.Set NodeId, Map.Map NodeId (Set.Set NodeId))
    generateDependenciesHelper (Tree id subtrees) = (myDeps, depMap)
      where
        myDeps = foldl' (\acc childDep -> acc <> fst childDep) (Set.singleton id) childDeps
        childDeps = map generateDependenciesHelper subtrees
        depMap = foldl' (\acc childDep -> acc <> snd childDep) (Map.singleton id myDeps) childDeps

generateDependenciesBottomUp :: StructuredExpression -> Map.Map NodeId NodeId
generateDependenciesBottomUp (tree, _) = generateDependenciesHelper tree
  where
  generateDependenciesHelper :: TreeStructure -> Map.Map NodeId NodeId
  generateDependenciesHelper (Tree id subtrees) =
    foldl' (<>) (Map.fromList $ map (\(Tree subId _) -> (subId, id)) subtrees) $ map generateDependenciesHelper subtrees

extractSubtree :: StructuredExpression -> NodeId -> Maybe TreeStructure
extractSubtree (tree, _) root = extractSubtreeHelper tree root
  where
    extractSubtreeHelper (Tree id subtrees) root =
      if id == root then Just (Tree id subtrees)
      else listToMaybe $ mapMaybe (\x -> extractSubtreeHelper x root) subtrees
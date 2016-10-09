module Treefication where

import Language
import Data.Monoid ((<>))
import Data.List (foldl')

import qualified Data.Map.Strict as Map

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
untreeficate (tree, nodes) = untree tree
  where
    untree (Tree id subtrees) = case nodeType of
      TerminalNode t -> Operand t
      UnaryNode t -> UnaryOperator t (untree $ head subtrees)
      NAryNode t -> NAryOperator t (map untree subtrees)
      where
        node = (Map.!) nodes id
        nodeType = details node
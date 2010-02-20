-- A graph module that takes care of all those pesky details,
-- putting the fun back in functional!
module Data.Graph.Fun (
    FGraph(..), Edge, empty, fromEdges, addEdge, addNode
) where

import qualified Data.Map as M
import qualified Data.Bimap as B
import qualified Data.Set as S

type NodeId = Int
type NodeMap a = B.Bimap NodeId a
type EdgeMap = M.Map NodeId (S.Set NodeId)
type Edge a = (a,a)

instance (Eq a, Ord a) => Eq (FGraph a) where
    x == y = nodes x == nodes y && edges x == edges y

data FGraph a = FGraph {
    nodeMap :: NodeMap a,
    nextNodeId :: NodeId,
    edgeMap :: EdgeMap
} deriving Show

empty :: Eq a => FGraph a
empty = FGraph { nodeMap = B.empty, nextNodeId = 0, edgeMap = M.empty }

fromEdges :: (Eq a, Ord a) => [Edge a] -> FGraph a
fromEdges = foldl (flip addEdge) empty

fromNodes :: (Eq a, Ord a) => [a] -> FGraph a
fromNodes = foldl (flip addNode) empty

addEdge :: (Eq a, Ord a) => Edge a -> FGraph a -> FGraph a
addEdge (n1,n2) g = g''
    where
        g' = addNode n2 $ addNode n1 g
        g'' = g' { edgeMap = eM' }
        eM = edgeMap g'
        eM' = M.insertWith S.union idN1 (S.singleton idN2) eM
        nM = nodeMap g'
        idN1,idN2 :: NodeId
        (idN1,idN2) = (nM B.!> n1, nM B.!> n2)

addNode :: (Eq a, Ord a) => a -> FGraph a -> FGraph a
addNode n g@FGraph{ nodeMap = nM, nextNodeId = nId } = g'
    where
        g' = g { nodeMap = nM', nextNodeId = nId' }
        (nM',nId') = case B.lookupR n nM of
            Nothing -> (B.insert nId n nM, nId + 1)
            _ -> (nM, nId)

nodes :: Ord a => FGraph a -> [a]
nodes FGraph{ nodeMap = nM } = B.elems nM

edges :: Ord a => FGraph a -> [Edge a]
edges FGraph{ edgeMap = eM, nodeMap = nM } = M.foldWithKey f [] eM
    where
        f k x acc = x' ++ acc where
            x' = [ (nM B.! k, nM B.! e) | e <- S.elems x ]

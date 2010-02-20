{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
-- A graph module that takes care of all those pesky details,
-- putting the fun back in functional!
module Data.Graph.Fun (
    GraphF, DGraph(..), Edge,
    empty, fromEdges, addEdge, addNode
) where

import qualified Data.Map as M
import qualified Data.Bimap as B
import qualified Data.Set as S

type NodeId = Int
type NodeMap a = B.Bimap NodeId a
type EdgeMap = M.Map NodeId (S.Set NodeId)
type Edge a = (a,a)

class GraphF g a where
    empty :: g a
    
    fromEdges :: [Edge a] -> g a
    fromEdges = foldl (flip addEdge) empty
    
    fromNodes :: [a] -> g a
    fromNodes = foldl (flip addNode) empty
    
    addEdge :: Edge a -> g a -> g a
    addNode :: a -> g a -> g a
    
    nodes :: g a -> [a]
    edges :: g a -> [Edge a]

-- | A directed graph
data DGraph a = DGraph {
    nodeMap :: NodeMap a,
    nextNodeId :: NodeId,
    edgeMap :: EdgeMap
} deriving Show

instance (Eq a, Ord a) => GraphF DGraph a where
    empty = DGraph { nodeMap = B.empty, nextNodeId = 0, edgeMap = M.empty }
    
    addEdge (n1,n2) g = g''
        where
            g' = addNode n2 $ addNode n1 g
            g'' = g' { edgeMap = eM' }
            eM = edgeMap g'
            eM' = M.insertWith S.union idN1 (S.singleton idN2) eM
            nM = nodeMap g'
            idN1,idN2 :: NodeId
            (idN1,idN2) = (nM B.!> n1, nM B.!> n2)

    addNode n g@DGraph{ nodeMap = nM, nextNodeId = nId } = g'
        where
            g' = g { nodeMap = nM', nextNodeId = nId' }
            (nM',nId') = case B.lookupR n nM of
                Nothing -> (B.insert nId n nM, nId + 1)
                _ -> (nM, nId)
    
    nodes DGraph{ nodeMap = nM } = B.elems nM
    
    edges DGraph{ edgeMap = eM, nodeMap = nM } = M.foldWithKey f [] eM
        where
            f k x acc = x' ++ acc where
                x' = [ (nM B.! k, nM B.! e) | e <- S.elems x ]

instance Ord a => Eq (DGraph a) where
    x == y = xN == yN && xE == yE
        where
            (xN,yN) = (S.fromList $ nodes x, S.fromList $ nodes y)
            (xE,yE) = (S.fromList $ edges x, S.fromList $ edges y)


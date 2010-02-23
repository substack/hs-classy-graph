{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
-- A classy graph module that sweeps all those pesky details like integer
-- indexing under the rug.
module Data.Graph.Class (
    Graph(..), GraphD(..), Edge,
) where

import qualified Data.Map as M
import qualified Data.Bimap as B
import qualified Data.Set as S

type NodeMap a = B.Bimap Vertex a
type EdgeMap = M.Map Vertex (S.Set Vertex)

type Edge a = (a,a)
type Vertex = Int

class Graph g a where
    empty :: g a
    
    withEdges :: [Edge a] -> g a -> g a
    withEdges = flip $ foldl (flip addEdge)
    
    fromEdges :: [Edge a] -> g a
    fromEdges = foldl (flip addEdge) empty
    
    withNodes :: [a] -> g a -> g a
    withNodes = flip $ foldl (flip addNode)
    
    fromNodes :: [a] -> g a
    fromNodes = foldl (flip addNode) empty
    
    addEdge :: Edge a -> g a -> g a
    addNode :: a -> g a -> g a
    
    nodes :: g a -> [a]
    vertices :: g a -> [Vertex]
    edges :: g a -> [Edge a]

-- | A directed graph
data GraphD a = GraphD {
    nodeMap :: NodeMap a,
    nextVertex :: Vertex,
    edgeMap :: EdgeMap
} deriving Show

instance (Eq a, Ord a) => Graph GraphD a where
    empty = GraphD { nodeMap = B.empty, nextVertex = 0, edgeMap = M.empty }
    
    addEdge (n1,n2) g = g''
        where
            g' = addNode n2 $ addNode n1 g
            g'' = g' { edgeMap = eM' }
            eM = edgeMap g'
            eM' = M.insertWith S.union v1 (S.singleton v2) eM
            nM = nodeMap g'
            (v1,v2) = (nM B.!> n1, nM B.!> n2) :: (Vertex,Vertex)
    
    addNode n g@GraphD{ nodeMap = nM, nextVertex = nId } = g'
        where
            g' = g { nodeMap = nM', nextVertex = nId' }
            (nM',nId') = case B.lookupR n nM of
                Nothing -> (B.insert nId n nM, nId + 1)
                _ -> (nM, nId)
    
    vertices GraphD{ nodeMap = nM } = B.keys nM
    
    nodes GraphD{ nodeMap = nM } = B.elems nM
    
    edges GraphD{ edgeMap = eM, nodeMap = nM } = M.foldWithKey f [] eM
        where
            f k x acc = x' ++ acc where
                x' = [ (nM B.! k, nM B.! e) | e <- S.elems x ]

instance Ord a => Eq (GraphD a) where
    x == y = xN == yN && xE == yE
        where
            (xN,yN) = (S.fromList $ nodes x, S.fromList $ nodes y)
            (xE,yE) = (S.fromList $ edges x, S.fromList $ edges y)


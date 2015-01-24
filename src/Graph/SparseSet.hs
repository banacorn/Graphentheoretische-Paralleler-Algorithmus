module Graph.SparseSet (
        module Graph.Type.SparseSet
    ,   bronKerboschLai
) where

import Graph.Type.SparseSet

import Data.Set hiding (map)
import Prelude hiding (null)

--------------------------------------------------------------------------------
-- Utilities

vertices :: Graph -> Vertices
vertices (Graph list) = fromList [0 .. length list - 1]

neighbor :: Graph -> Vertex -> Vertices
neighbor (Graph list) v = list !! v

--------------------------------------------------------------------------------
-- Bron-Kerbosch-Lai Algorithm
-- finds the size of the biggest maximal cliques in an undirected graph
bronKerboschLai :: Graph -> Int
bronKerboschLai g = go g empty (vertices g) empty

-- with pivoting, without vertex degeneracy ordering
go :: Graph -> Vertices -> Vertices -> Vertices -> Int
go g r p x
    | null p && null x = size r          -- found a clique
    | null p           = 0                -- backtracks
    | otherwise        = maximum (map next (toList vs))
    where   pivot   = findMin (p `union` x)
            vs      = p `difference` neighbor g pivot
            next v  = go g
                        (v `insert` r)
                        (p `intersection` neighbor g v)
                        (x `intersection` neighbor g v)

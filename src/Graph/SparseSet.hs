module Graph.SparseSet (
        module Graph.Type.SparseSet
    ,   bronKerbosch
) where

import Graph.Type.SparseSet

import Data.Set
import Prelude hiding (null, map)

--------------------------------------------------------------------------------
-- Utilities

vertices :: Graph -> Vertices
vertices (Graph list) = fromList [0 .. length list - 1]

neighbor :: Graph -> Vertex -> Vertices
neighbor (Graph list) v = list !! v

--------------------------------------------------------------------------------
-- Bron-Kerbosch Algorithm
-- finds all maximal cliques in an undirected graph
bronKerbosch :: Graph -> Set Clique
bronKerbosch g = bronKerbosch2 g empty (vertices g) empty

-- with pivoting, without vertex degeneracy ordering
bronKerbosch2 :: Graph -> Vertices -> Vertices -> Vertices -> Set Clique
bronKerbosch2 g r p x
    | null p && null x = singleton r          -- found a clique
    | null p           = empty                -- backtracks
    | otherwise        = unions (toList (map next vs))
    where   pivot   = findMin (p `union` x)
            vs      = p `difference` neighbor g pivot
            next v  = bronKerbosch2 g
                        (v `insert` r)
                        (p `intersection` neighbor g v)
                        (x `intersection` neighbor g v)

module Graph.SparseList (
        module Graph.Type.SparseList
    ,   bronKerbosch
) where

import Graph.Type.SparseList

import Data.List (union, intersect, (\\), sort, nub)

--------------------------------------------------------------------------------
-- Utilities

vertices :: Graph -> Vertices
vertices (Graph list) = [0 .. length list - 1]

neighbor :: Graph -> Vertex -> Vertices
neighbor (Graph list) v = list !! v

--------------------------------------------------------------------------------
-- Bron-Kerbosch Algorithm
-- finds all maximal cliques in an undirected graph
bronKerbosch :: Graph -> [Clique]
bronKerbosch g = nub . map sort $ bronKerbosch2 g [] (vertices g) []

-- with pivoting, without vertex degeneracy ordering
bronKerbosch2 :: Graph -> Vertices -> Vertices -> Vertices -> [Clique]
bronKerbosch2 _ r [] [] = [r]               -- found a clique
bronKerbosch2 _ _ [] _  = []                -- backtracks
bronKerbosch2 g r p  x  = vs >>= next
    where   pivot   = head (p `union` x)
            vs      = p \\ neighbor g pivot
            next v  = bronKerbosch2 g
                        (r `union` [v])
                        (p `intersect` neighbor g v)
                        (x `intersect` neighbor g v)

module Graph.Clique where

import Data.List (union, intersect, (\\), sort, nub)

import Graph.Type
import Graph.Util

-- Bron-Kerbosch Algorithm, finds all maximal cliques in an undirected graph
bronKerbosch :: Graph -> [[V]]
bronKerbosch g = nub . map sort $ bronKerbosch2 g [] (vertices g) []

-- with pivoting, without vertex degeneracy ordering
bronKerbosch2 :: Graph -> [V] -> [V] -> [V] -> [[V]]
bronKerbosch2 _ r [] [] = [r]               -- found a clique
bronKerbosch2 _ _ [] _  = []                -- backtracks
bronKerbosch2 g r p  x  = vs >>= next
    where   pivot = head (p `union` x)
            vs = p \\ neighbor g pivot
            next v = bronKerbosch2 g
                        (r `union` [v])
                        (p `intersect` neighbor g v)
                        (x `intersect` neighbor g v)

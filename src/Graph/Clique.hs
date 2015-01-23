module Graph.Clique where

import Data.List (elemIndices, union, intersect, (\\), sort, nub)

import Graph.Type

vertices :: Graph -> [V]
vertices (AdjList list) = [0 .. length list - 1]

neighbor :: Graph -> V -> [V]
neighbor (AdjList list) v = list !! v

degree :: Graph -> V -> Int
degree g = length . neighbor g


-- Bron-Kerbosch Algorithm, finds all maximal cliques in an undirected graph
bronKerbosch :: Graph -> [[V]]
bronKerbosch g =  bronKerbosch2 g [] (vertices g) []

bronKerbosch' :: Graph -> [[V]]
bronKerbosch' g = nub . map sort $ bronKerbosch2 g [] (vertices g) []


-- with pivoting, without vertex degeneracy ordering
bronKerbosch2 :: Graph -> [V] -> [V] -> [V] -> [[V]]
bronKerbosch2 g r [] [] = [r]               -- found a clique
bronKerbosch2 g r [] _  = []                -- backtracks
bronKerbosch2 g r p  x  = vs >>= next
    where   pivot = head (p `union` x)
            vs = p \\ neighbor g pivot
            next v = bronKerbosch2 g
                        (r `union` [v])
                        (p `intersect` neighbor g v)
                        (x `intersect` neighbor g v)

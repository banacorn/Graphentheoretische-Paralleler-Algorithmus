{-# LANGUAGE BangPatterns #-}
module Graph.SparseSetRepa (
        module Graph.Type.SparseSet
    ,   bronKerboschLai
) where

import Graph.Type.SparseSet

import Data.Set hiding (map)
import Prelude hiding (null)
import qualified Data.Array.Repa as R

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
go !g !r !p !x
    | null p && null x = size r          -- found a clique
    | null p           = 0                -- backtracks
    | otherwise        = R.foldAllS max 0 (R.map next vs')
    --maximum . R.toList . computeP $ (R.map next vs')

    where   pivot   = findMin (p `union` x)
            vs      = p `difference` neighbor g pivot
            next !v  = go g
                        (v `insert` r)
                        (p `intersection` neighbor g v)
                        (x `intersection` neighbor g v)

            vs' = fromListUnboxed' (toList vs)
            fromListUnboxed' xs = R.fromListUnboxed (R.Z R.:. (length xs)) xs
            --computeP = runIdentity . R.computeP :: R.Array R.D R.DIM1 Int -> R.Array R.U R.DIM1 Int

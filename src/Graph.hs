module Graph where

import qualified Graph.DenseList as D
import qualified Graph.SparseList as S
import qualified Graph.SparseSet as SS
import qualified Graph.SparseSetRepa as SSR

import qualified Data.Array.Repa as R

import Data.List (nub)
import Data.Set (fromList)

--------------------------------------------------------------------------------
-- conversions between different type of representation of Graphs

graphSize :: SS.Graph -> Int
graphSize (SS.Graph xs) = length xs
bronKerboschLai g = sum $ zipWith (*) [0 .. graphSize g] [0 .. graphSize g]
bronKerboschLaiR g = head $ R.sumAllP $ R.zipWith (*) list list
    where   fromList x = R.fromListUnboxed (R.Z R.:. length x) x
            list = fromList [0 .. graphSize g]

toSparse :: D.Graph -> S.Graph
toSparse (D.Graph xs) = S.Graph (map D.toVertex xs)

fromSparse :: S.Graph -> D.Graph
fromSparse (S.Graph xs) = D.Graph (map fromVertex xs)
    where   fromVertex = foldl D.insert (replicate (length xs) False)

toSparseSet :: D.Graph -> SS.Graph
toSparseSet (D.Graph xs) = SS.Graph (map (fromList . D.toVertex) xs)

--------------------------------------------------------------------------------
-- grow graph

-- given a Graph of size N and a list of integers, returns a Graph of size N+1
addVertex :: D.Graph -> [Int] -> D.Graph
addVertex (D.Graph []) _ = D.Graph [[False]]
addVertex (D.Graph xs) [] = D.Graph xs
addVertex (D.Graph xs) ns = D.Graph (xs' ++ [vector ++ [False]])
    where   ns' = nub (map (flip mod (length xs)) ns) -- make indices in bound
            fromVertex = foldl D.insert (replicate (length xs) False)
            vector = fromVertex ns'
            xs' = zipWith (\a x -> a ++ [x]) xs vector

--------------------------------------------------------------------------------
-- test data

q :: D.Graph
q = D.Graph
    [   [False, True,  False, False, True,  False]
    ,   [True,  False, True,  False, True,  False]
    ,   [False, True,  False, True,  False, False]
    ,   [False, False, True,  False, True,  True]
    ,   [True,  True,  False, True,  False, False]
    ,   [False, False, False, True,  False, False]
    ]

w :: Int
w = SSR.bronKerboschLai $ toSparseSet q

module Graph where

import qualified Graph.Dense as D
import qualified Graph.Sparse as S
import qualified Graph.SparseSet as SS
-- import qualified Graph.Repa as R


import Data.List (nub)
import Data.Set (fromList)

--------------------------------------------------------------------------------
-- conversions between different type of representation of Graphs

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

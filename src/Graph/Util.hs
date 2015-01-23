module Graph.Util where

import Graph.Type

vertices :: Graph -> [V]
vertices (AdjList list) = [0 .. length list - 1]

neighbor :: Graph -> V -> [V]
neighbor (AdjList list) v = list !! v

degree :: Graph -> V -> Int
degree g = length . neighbor g

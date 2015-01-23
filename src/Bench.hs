module Bench where

import Criterion.Main

import Graph

runBench :: IO ()
runBench = defaultMain
    [   bgroup "Bron-Kerbosch"      [   bench "a" $ whnf bronKerbosch a
                                    ]
    ]

a :: Graph
a = AdjList
    [   [1, 4]
    ,   [0, 2, 4]
    ,   [1, 3]
    ,   [2, 4, 5]
    ,   [0, 1, 3]
    ,   [3]
    ]

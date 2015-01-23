module Bench where

import Criterion.Main
import qualified Data.Array.Repa as R

import Graph

runBench :: IO ()
runBench = defaultMain
    [   --bgroup "Bron-Kerbosch"      [   bench "a" $ whnf bronKerbosch a
        --                            ]
        bgroup "Dot Product"
            [   bench "1M"      $ whnf (dopt [1 .. 1000000]) [1 .. 1000000]
            ,   bench "10M"     $ whnf (dopt [1 .. 10000000]) [1 .. 10000000]
            -- ,   bench "100M"    $ whnf (dopt [1 .. 100000000]) [1 .. 100000000]
            -- ,   bench "1G"      $ whnf (dopt [1 .. 1000000000]) [1 .. 1000000000]
            ]
    ,   bgroup "Dot Product Repa"
            [   bench "1M"      $ whnf (doptR [1 .. 1000000]) [1 .. 1000000]
            ,   bench "10M"     $ whnf (doptR [1 .. 10000000]) [1 .. 10000000]
            -- ,   bench "100M"    $ whnf (doptR [1 .. 100000000]) [1 .. 100000000]
            -- ,   bench "1G"      $ whnf (doptR [1 .. 1000000000]) [1 .. 1000000000]
            ]
    ]
--
-- a :: Graph
-- a = AdjList
--     [   [1, 4]
--     ,   [0, 2, 4]
--     ,   [1, 3]
--     ,   [2, 4, 5]
--     ,   [0, 1, 3]
--     ,   [3]
--     ]

dopt :: [Int] -> [Int] -> Int
dopt a b = sum $ zipWith (*) a b

type VecU = R.Array R.U R.DIM1 Int
type VecD = R.Array R.D R.DIM1 Int
type ScaU = R.Array R.U R.DIM0 Int
doptR :: [Int] -> [Int] -> Int
doptR a b = head $ R.sumAllP $ R.zipWith (*) (fromList a) (fromList b)
    where
            fromList x = R.fromListUnboxed (R.Z R.:. length x) x

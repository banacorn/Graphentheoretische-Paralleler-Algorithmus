module Bench where

import Graph
import qualified Graph.DenseList as D
import qualified Graph.SparseSet as SS
import qualified Graph.SparseSetRepa as SSR

import Criterion.Main
import System.Random

runBench :: IO ()
runBench = do
    d <- genGraph 1000
    e <- genGraph 2000
    let d' = toSparseSet d
    let e' = toSparseSet e
    defaultMain
        [   bgroup "Bron-Kerbosch SparseSet"
                [   bench "1K" $ whnf SS.bronKerboschLai d'
                ,   bench "2K" $ whnf SS.bronKerboschLai e'
                ]
        ,   bgroup "Bron-Kerbosch SparseSet Repa"
                [   bench "1K" $ whnf SSR.bronKerboschLai d'
                ,   bench "2K" $ whnf SSR.bronKerboschLai e'
                ]
        ]

genGraph :: Int -> IO D.Graph
genGraph n = do
    degreeSeed <- newStdGen
    vertexSeed <- newStdGen
    let list = take n (takes degreeSeed vertexSeed)
    return (foldl addVertex (D.Graph []) list)
    where   takes s t = map (flip take (randoms t :: [Int])) (randomRs (1, 3) s :: [Int])

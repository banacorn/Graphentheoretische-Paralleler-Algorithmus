module Bench where

import Graph
import qualified Graph.DenseList as D
import qualified Graph.SparseList as S
import qualified Graph.SparseSet as SS
import qualified Graph.SparseSetRepa as SSR

import Criterion.Main
import System.Random


runBench :: IO ()
runBench = do
    d <- genGraph 1000
    let s = toSparseSet d
    defaultMain
        [
            -- bgroup "Bron-Kerbosch Dense"    [   bench "100" $ nf D.bronKerbosch d
            --                                 ]
            bgroup "Bron-Kerbosch SparseSet"   [   bench "1K" $ nf SS.bronKerboschLai s
                                            ]
        ,   bgroup "Bron-Kerbosch SparseSet Repa"   [   bench "1K" $ nf SSR.bronKerboschLai s
                                            ]
        --     bgroup "Dot Product"
        --         [   bench "1M"      $ whnf (dopt [1 .. 1000000]) [1 .. 1000000]
        --         ,   bench "10M"     $ whnf (dopt [1 .. 10000000]) [1 .. 10000000]
        --         -- ,   bench "100M"    $ whnf (dopt [1 .. 100000000]) [1 .. 100000000]
        --         -- ,   bench "1G"      $ whnf (dopt [1 .. 1000000000]) [1 .. 1000000000]
        --         ]
        -- ,   bgroup "Dot Product Repa"
        --         [   bench "1M"      $ whnf (doptR [1 .. 1000000]) [1 .. 1000000]
        --         ,   bench "10M"     $ whnf (doptR [1 .. 10000000]) [1 .. 10000000]
        --         -- ,   bench "100M"    $ whnf (doptR [1 .. 100000000]) [1 .. 100000000]
        --         -- ,   bench "1G"      $ whnf (doptR [1 .. 1000000000]) [1 .. 1000000000]
        --         ]
        ]

genGraph :: Int -> IO D.Graph
genGraph n = do
    degreeSeed <- newStdGen
    vertexSeed <- newStdGen
    let list = take n (takes degreeSeed vertexSeed)
    return (foldl addVertex (D.Graph []) list)
    where   takes s t = map (flip take (randoms t :: [Int])) (randomRs (1, 3) s :: [Int])

-- dopt :: [Int] -> [Int] -> Int
-- dopt a b = sum $ zipWith (*) a b
--
-- type VecU = R.Array R.U R.DIM1 Int
-- type VecD = R.Array R.D R.DIM1 Int
-- type ScaU = R.Array R.U R.DIM0 Int
-- doptR :: [Int] -> [Int] -> Int
-- doptR a b = head $ R.sumAllP $ R.zipWith (*) (fromList a) (fromList b)
--     where
--             fromList x = R.fromListUnboxed (R.Z R.:. length x) x

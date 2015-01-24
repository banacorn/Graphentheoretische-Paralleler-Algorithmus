module Graph.Repa where

import Prelude hiding (zipWith, null, map)
-- import qualified Data.List as List
-- import Data.Maybe (fromJust)
import Data.Array.Repa


--------------------------------------------------------------------------------
-- Data Type

type Vertex = Int
type Vertices a = Array a DIM1 Bool

data Graph = Graph (Array U DIM2 Bool) deriving (Eq, Show)

type Clique = Array U DIM1 Int

--------------------------------------------------------------------------------
-- Graph related functions

graphSize :: Graph -> Int
graphSize (Graph xs) = size (extent xs)

vertices :: Graph -> Vertices U
vertices g = fromListUnboxed (Z :. s) $ flip replicate True s
    where s = graphSize g

neighbor :: Graph -> Vertex -> Vertices U
neighbor (Graph xs) v = computeS (slice xs (Any :. v :. All))

pickPivot :: Vertices U -> Int
pickPivot = search 0
    where   search n arr | arr ! (Z :. n) = n
                         | otherwise      = search (n + 1) arr

toVertex :: Vertices U -> Array U DIM1 Int
toVertex = fromListUnboxed' . search 0
    where   search n arr | n >= len       = []
                         | arr ! (Z :. n) = n : search (n + 1) arr
                         | otherwise      =     search (n + 1) arr
                where    len = size (extent arr)
            fromListUnboxed' xs = fromListUnboxed (Z :. (length xs)) xs

-- set operations
union :: Vertices U -> Vertices U -> Vertices U
union a b = computeS (zipWith (||) a b)

intersect :: Vertices U -> Vertices U -> Vertices U
intersect a b = computeS (zipWith (&&) a b)

(\\) :: Vertices U -> Vertices U -> Vertices U
(\\) a b = computeS (zipWith (\ m n -> m && not n) a b)

insert :: Vertices U -> Vertex -> Vertices U
insert arr n = computeS (traverse arr id hit)
    where   hit f i | i == (Z :. n) = True
                    | otherwise = f i

null :: Vertices U -> Bool
null = not . foldAllS (||) False


--------------------------------------------------------------------------------
-- Bron-Kerbosch Algorithm
-- finds all maximal cliques in an undirected graph
-- bronKerbosch :: Graph -> [Clique]
-- bronKerbosch g = map toVertex . nub $ bronKerbosch2 g empty (vertices g) empty
--     where   empty = replicate (size g) False

-- with pivoting, without vertex degeneracy ordering
bronKerbosch2 :: Graph -> Vertices U -> Vertices U -> Vertices U -> [Vertices U]
bronKerbosch2 g r p x
    | null p && null x = [r]            -- found a clique
    | null p           = []             -- backtracks
    -- | otherwise        = concat $ toList (head (computeP (map next vs)) :: Array D DIM1 [Vertices D])
    -- where   pivot   = pickPivot (p `union` x)
    --         vs      = toVertex (p \\ neighbor g pivot)
    --         next v  = bronKerbosch2 g
    --                     (r `insert` v)
    --                     (p `intersect` neighbor g v)
    --                     (x `intersect` neighbor g v)

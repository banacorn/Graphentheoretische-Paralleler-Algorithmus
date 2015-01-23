module Graph.Dense where

import Prelude hiding (null)
import Data.List (elemIndex, elemIndices, nub)
import Data.Maybe (fromJust)

--------------------------------------------------------------------------------
-- Data Type

type Vertex = Int
type Vertices = [Bool]

data Graph = Graph [Vertices] deriving (Eq, Show)

type Clique = [Int]

--------------------------------------------------------------------------------
-- Graph related functions

size :: Graph -> Int
size (Graph xs) = length xs

vertices :: Graph -> Vertices
vertices = flip replicate True . size

neighbor :: Graph -> Vertex -> Vertices
neighbor (Graph xs) v = xs !! v

pickPivot :: Vertices -> Vertex
pickPivot = fromJust . elemIndex True

toVertex :: Vertices -> [Vertex]
toVertex = elemIndices True

-- set operations
union :: Vertices -> Vertices -> Vertices
union = zipWith (||)

intersect :: Vertices -> Vertices -> Vertices
intersect = zipWith (&&)

(\\) :: Vertices -> Vertices -> Vertices
(\\) = zipWith (\ a b -> a && not b)

insert :: Vertices -> Vertex -> Vertices
insert [] _ = []
insert (_:xs) 0 = True : xs
insert (x:xs) n = x : insert xs (n - 1)

null :: Vertices -> Bool
null = not . or


--------------------------------------------------------------------------------
-- Bron-Kerbosch Algorithm
-- finds all maximal cliques in an undirected graph
bronKerbosch :: Graph -> [Clique]
bronKerbosch g = map toVertex . nub $ bronKerbosch2 g empty (vertices g) empty
    where   empty = replicate (size g) False

-- with pivoting, without vertex degeneracy ordering
bronKerbosch2 :: Graph -> Vertices -> Vertices -> Vertices -> [Vertices]
bronKerbosch2 g r p x
    | null p && null x = [r]            -- found a clique
    | null p           = []             -- backtracks
    | otherwise        = vs >>= next
    where   pivot   = pickPivot (p `union` x)
            vs      = toVertex (p \\ neighbor g pivot)
            next v  = bronKerbosch2 g
                        (r `insert` v)
                        (p `intersect` neighbor g v)
                        (x `intersect` neighbor g v)

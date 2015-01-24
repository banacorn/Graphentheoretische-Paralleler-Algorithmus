module Graph.Type.DenseList where

type Vertex = Int
type Vertices = [Bool]

data Graph = Graph [Vertices] deriving (Eq, Show)

type Clique = [Int]

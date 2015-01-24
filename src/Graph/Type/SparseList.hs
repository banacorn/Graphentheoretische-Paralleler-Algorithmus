module Graph.Type.SparseList where

type Vertex = Int
type Vertices = [Vertex]

data Graph  = Graph [Vertices] deriving (Eq, Show)

type Clique = Vertices

module Graph.Type.SparseSet where

import Data.Set

type Vertex = Int
type Vertices = Set Vertex

data Graph  = Graph [Vertices] deriving (Eq, Show)

type Clique = Vertices

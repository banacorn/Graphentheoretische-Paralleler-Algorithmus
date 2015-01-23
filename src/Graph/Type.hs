module Graph.Type where

type V = Int
data Graph  = AdjList [[V]]
            -- | AdjMat [[Bool]]
            deriving (Eq, Show)

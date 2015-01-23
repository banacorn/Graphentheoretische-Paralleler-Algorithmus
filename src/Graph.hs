module Graph where

import qualified Graph.Dense as D
import qualified Graph.Sparse as S

toSparse :: D.Graph -> S.Graph
toSparse (D.Graph xs) = S.Graph (map D.toVertex xs)

fromSparse :: S.Graph -> D.Graph
fromSparse (S.Graph xs) = D.Graph (map fromVertex xs)
    where   fromVertex = foldl D.insert (replicate (length xs) False)

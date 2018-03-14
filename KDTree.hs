import Data.Array.IArray

type Node k = (Array Int) k

data KDNode k = KDNode {node :: Node k, splitsAt :: Int}

data KDTree k = Empty | Fork (KDTree k) (KDNode k) (KDTree k)

insert :: (Ord k) => Int -> Int -> Node k -> KDTree k -> KDTree k
insert d sp n Empty = Fork Empty (KDNode {node = n, splitsAt = sp `mod` d}) Empty
insert d sp n t@(Fork l x@KDNode{node = xn, splitsAt = s} r)
  | n == xn = t
  | n ! s < xn ! s = Fork (insert d (sp + 1) n l) x r
  | otherwise = Fork l x (insert d (sp + 1) n r)
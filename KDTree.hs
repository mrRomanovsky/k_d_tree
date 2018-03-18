import Data.Array.IArray

type Node k = (Array Int) k

data KDNode k = KDNode {node :: Node k, splitsAt :: Int}

data KDTree k = Empty | Fork (KDTree k) (KDNode k) (KDTree k)

data Focus k = LeftChild (KDNode k) (KDTree k) | RightChild (KDNode k) (KDTree k)

type Zipper k = (KDTree k, [Focus k])

goUp :: Zipper k -> Zipper k
goUp (t, LeftChild p r : fs) = (Fork t p r, fs)
goUp (t, RightChild p l : fs) = (Fork l p t, fs)

goLeft :: Zipper k -> Zipper k
goLeft (Fork l x r, fs) = (l, LeftChild x r : fs)

goRight :: Zipper k -> Zipper k
goRight (Fork l x r, fs) = (r, RightChild x l : fs)

goTop :: Zipper k -> Zipper k
goTop (t, []) = (t, [])
goTop z = goTop $ goUp z

insert :: (Ord k) => Int -> Int -> Node k -> KDTree k -> KDTree k
insert d sp n Empty = Fork Empty (KDNode {node = n, splitsAt = sp `mod` d}) Empty
insert d sp n t@(Fork l x@KDNode{node = xn, splitsAt = s} r)
  | n == xn = t
  | n ! s < xn ! s = Fork (insert d (sp + 1) n l) x r
  | otherwise = Fork l x (insert d (sp + 1) n r)


fromNodesList :: (Ord k) => Int -> [Node k] -> KDTree k
fromNodesList d = foldr (insert d 0) Empty
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

find :: (Ord k) => Node k -> KDTree k -> Zipper k
find = ((reverse <$>) .) . find'
  where
    find' :: (Ord k) => Node k -> KDTree k -> Zipper k
    find' n Empty = (Empty, [])
    find' n t@(Fork l x@KDNode{node = xn, splitsAt = s} r)
      | n == xn = (t, [])
      | n ! s < xn ! s = (LeftChild x r :) <$> find n l
      | otherwise = (RightChild x l :) <$> find n r

{-обдумать следующие стратегии работы с деревом:
1)Каждый раз после вставки возвращаться к вершине дерева,
и в следующий раз начинать поиск с вершины
2)Каждый раз после вставки просто оставаться в зиппере,
и в следующий раз продолжать поиск(вниз или вверх) с текущего места
-} 
insert :: (Ord k) => Int -> Node k -> KDTree k -> Zipper k
insert d n t = case find n t of
    (Empty, bs) -> let newNode = KDNode n (length bs `mod` d)
                   in (Fork Empty newNode Empty, bs)
    z           -> z


{-переписать!
fromNodesList :: (Ord k) => Int -> [Node k] -> KDTree k
fromNodesList d = foldr (insert d 0) Empty-}
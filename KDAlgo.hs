module KDAlgo where

import KDTree
import KDZipper
import Data.Array.IArray

find :: (Ord k) => Node k -> KDTree k -> Zipper k
find = ((reverse <$>) .) . find'
  where
    find' :: (Ord k) => Node k -> KDTree k -> Zipper k
    find' n Empty = (Empty, [])
    find' n t@(Fork l x@KDNode{node = xn, splitsAt = s} r)
      | n == xn = (t, [])
      | (n ! s) < (xn ! s) = (LeftChild x r :) <$> find n l
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
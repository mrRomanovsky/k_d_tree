module KDAlgo where

import KDTree
import KDZipper
import Data.Array.IArray


insert :: (Ord k) => Int -> Int -> Node k -> KDTree k -> KDTree k
insert d sp n Empty = Fork Empty (KDNode {node = n, splitsAt = sp `mod` d}) Empty
insert d sp n t@(Fork l x@KDNode{node = xn, splitsAt = s} r)
  | n == xn = t
  | n ! s < xn ! s = Fork (insert d (sp + 1) n l) x r
  | otherwise = Fork l x (insert d (sp + 1) n r) 



findZipper :: (Ord k) => Node k -> KDTree k -> Zipper k
findZipper = ((reverse <$>) .) . findZipper'
  where
    findZipper' :: (Ord k) => Node k -> KDTree k -> Zipper k
    findZipper' n Empty = (Empty, [])
    findZipper' n t@(Fork l x@KDNode{node = xn, splitsAt = s} r)
      | n == xn = (t, [])
      | (n ! s) < (xn ! s) = (LeftChild x r :) <$> findZipper n l
      | otherwise = (RightChild x l :) <$> findZipper n r



{-обдумать следующие стратегии работы с деревом:
1)Каждый раз после вставки возвращаться к вершине дерева,
и в следующий раз начинать поиск с вершины
2)Каждый раз после вставки просто оставаться в зиппере,
и в следующий раз продолжать поиск(вниз или вверх) с текущего места
-} 
insertZipper :: (Ord k) => Int -> Node k -> KDTree k -> Zipper k
insertZipper d n t = case findZipper n t of
    (Empty, bs) -> let newNode = KDNode n (length bs `mod` d)
                   in (Fork Empty newNode Empty, bs)
    z           -> z


fromNodesList :: (Ord k) => Int -> [Node k] -> KDTree k
fromNodesList d = foldl (flip $ insert d 0) Empty
module KDAlgo where

import KDTree
--import KDZipper
import Data.Array.IArray

insert :: (Ord k) => Int -> Int -> Node k -> KDTree k -> KDTree k
insert d s n Empty = Fork Empty n Empty
insert d s n t@(Fork l x r)
  | n == x = t
  | n ! s < x ! s = Fork (insert d ((s + 1) `mod` d) n l) x r
  | otherwise = Fork l x (insert d ((s + 1) `mod` d) n r) 


fromNodesList :: (Ord k) => Int -> [Node k] -> KDTree k
fromNodesList d = foldl (flip $ insert d 0) Empty
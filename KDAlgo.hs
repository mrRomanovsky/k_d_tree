module KDAlgo where

import KDTree
import Data.Array.IArray

insert :: (Ord k) => Int -> Int -> Node k -> KDTree k -> KDTree k
insert d s n Empty = Fork Empty n Empty
insert d s n t@(Fork l x r)
  | n == x = t
  | n ! s < x ! s = Fork (insert d ((s + 1) `mod` d) n l) x r
  | otherwise = Fork l x (insert d ((s + 1) `mod` d) n r) 


fromNodesList :: (Ord k) => Int -> [Node k] -> KDTree k
fromNodesList d = foldl (flip $ insert d 0) Empty

treeHeight :: KDTree k -> Int
treeHeight Empty = 0
treeHeight (Fork l x r) = max (treeHeight l + 1) (treeHeight r + 1)

treeCorrect :: Ord k => KDTree k -> Int -> Int -> Bool
treeCorrect Empty _ _ = True
treeCorrect (Fork Empty x Empty) _ _ = True
treeCorrect (Fork Empty x rt@(Fork _ y _)) d s = x ! s <=  y ! s
                                                 && treeCorrect rt d ((s + 1) `mod` d)
treeCorrect (Fork lt@(Fork _ y _) x Empty) d s = x ! s > y ! s
                                                 && treeCorrect lt d ((s + 1) `mod` d)
treeCorrect (Fork lt@(Fork _ y _) x rt@(Fork _ z _)) d s = x ! s > y ! s
                                                 && x ! s <= z ! s
                                                 && treeCorrect lt d ((s + 1) `mod` d)
                                                 && treeCorrect rt d ((s + 1) `mod` d)

checkTree :: Ord k => KDTree k -> Int -> Bool
checkTree t d = treeCorrect t d 0
module KDAlgo where
import KDTree
import KDZipper
import Prelude hiding (length)
import Data.Vector hiding (foldl, reverse)

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

find :: Ord k => Node k -> KDTree k -> Zipper k
find = ((reverse <$>) .) . (findZipper 0)
  where
    findZipper :: Ord k => Int -> Node k -> KDTree k -> Zipper k
    findZipper s n Empty = ((Empty, s), [])
    findZipper s n t@(Fork l x r)
      | n == x = ((t, s), [])
      | (n ! s) < (x ! s) = (LeftChild x r :) <$>
                            findZipper ((s + 1) `mod` length n) n l
      | otherwise = (RightChild x l :) <$> 
                    findZipper ((s + 1) `mod` length n) n r

delete :: Ord k => Zipper k -> Zipper k
delete z@((Empty, _), _) = z
delete z@((Fork Empty _ Empty, i), h) = ((Empty, i), h)
delete z@((Fork l x Empty, i), h) = 
    let ml@((Fork _ y _, _), _) =
          min_node i ((l, (i + 1) `mod` length x), [])
        in ((Fork Empty y $ get_subtree ml, i), h)
delete z@((Fork l x r, i), h) = 
    let mr@((Fork _ y _, _), _) =
          min_node i ((r, (i + 1) `mod` length x), [])
        in ((Fork l y $ get_subtree mr, i), h)

get_subtree z = let del = delete z
                    ((t, _), _) = goTop del
                    in t 

min_node :: Ord k => Int -> Zipper k -> Zipper k
min_node _ z@((Empty, _), _) = z
min_node i z = 
    let lm = min_node i $ goLeft z
        rm = min_node i $ goRight z
        in min_zip z (min_zip lm rm i) i

min_zip :: Ord k => Zipper k -> Zipper k -> Int -> Zipper k
min_zip ((Empty, _), _) z2 _ = z2
min_zip z1 ((Empty, _), _) _ = z1
min_zip z1@((Fork _ x _, _), _) z2@((Fork _ y _, _), _) i
  | x ! i < y ! i = z1
  | otherwise     = z2
